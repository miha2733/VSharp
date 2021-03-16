namespace VSharp.Interpreter.IL

open System
open System.Reflection
open System.Reflection.Emit

open VSharp
open VSharp.Core
open CFG

type public ILMethodMetadata =
    { methodBase : MethodBase }
    override x.ToString () = x.methodBase.Name
    interface IMethodIdentifier with
        member x.IsStatic = x.methodBase.IsStatic
        member x.IsConstructor = x.methodBase.IsConstructor
        member x.Method = x.methodBase
        member x.DeclaringType = x.methodBase.DeclaringType
        member x.DeclaringAssembly = x.methodBase.DeclaringType.Assembly
        member x.ReturnType =
            match x.methodBase with
            | :? MethodInfo as mi -> mi.ReturnType
            | :? ConstructorInfo -> typeof<Void>
            | _ -> __notImplemented__()

module internal TypeUtils =
    open Types

    // TODO: get all this functions from Core #mbdo
    let float64Type = Numeric typedefof<double>
    let float32Type = Numeric typedefof<float32>
    let int8Type    = Numeric typedefof<int8>
    let int16Type   = Numeric typedefof<int16>
    let int32Type   = Numeric typedefof<int32>
    let int64Type   = Numeric typedefof<int64>
    let uint8Type   = Numeric typedefof<uint8>
    let uint16Type  = Numeric typedefof<uint16>
    let uint32Type  = Numeric typedefof<uint32>
    let uint64Type  = Numeric typedefof<uint64>
    let charType    = Numeric typedefof<char>

    let signed2unsignedOrId = function
        | Bool -> uint32Type
        | Numeric (Id typ) when typ = typedefof<int32> || typ = typedefof<uint32> -> uint32Type
        | Numeric (Id typ) when typ = typedefof<int8>  || typ = typedefof<uint8>  -> uint8Type
        | Numeric (Id typ) when typ = typedefof<int16> || typ = typedefof<uint16> -> uint16Type
        | Numeric (Id typ) when typ = typedefof<int64> || typ = typedefof<uint64> -> uint64Type
        | Numeric (Id typ) when typ = typedefof<double> -> float64Type
        | _ -> __unreachable__()
    let unsigned2signedOrId = function
        | Bool -> int32Type
        | Numeric (Id typ) when typ = typedefof<int32> || typ = typedefof<uint32> -> int32Type
        | Numeric (Id typ) when typ = typedefof<int8>  || typ = typedefof<uint8> -> int8Type
        | Numeric (Id typ) when typ = typedefof<int16> || typ = typedefof<uint16> -> int16Type
        | Numeric (Id typ) when typ = typedefof<int64> || typ = typedefof<uint64> -> int64Type
        | Numeric (Id typ) when typ = typedefof<double> -> float64Type
        | Pointer _ as t -> t
        | _ -> __unreachable__()
    let integers = [charType; int8Type; int16Type; int32Type; int64Type; uint8Type; uint16Type; uint32Type; uint64Type]

    let isIntegerTermType typ = integers |> List.contains typ
    let isFloatTermType typ = typ = float32Type || typ = float64Type
    // TODO: empty state is wrong! we always need state, because the term could be heapRef! (mb make type of location to return Option) #do
    let isInteger = Terms.TypeOf Memory.EmptyState >> isIntegerTermType
    let isBool = Terms.TypeOf Memory.EmptyState >> IsBool
    let (|Int8|_|) t = if Terms.TypeOf Memory.EmptyState t = int8Type then Some() else None
    let (|UInt8|_|) t = if Terms.TypeOf Memory.EmptyState t = uint8Type then Some() else None
    let (|Int16|_|) t = if Terms.TypeOf Memory.EmptyState t = int16Type then Some() else None
    let (|UInt16|_|) t = if Terms.TypeOf Memory.EmptyState t = uint16Type then Some() else None
    let (|Int32|_|) t = if Terms.TypeOf Memory.EmptyState t = int32Type then Some() else None
    let (|UInt32|_|) t = if Terms.TypeOf Memory.EmptyState t = uint32Type then Some() else None
    let (|Int64|_|) t = if Terms.TypeOf Memory.EmptyState t = int64Type then Some() else None
    let (|UInt64|_|) t = if Terms.TypeOf Memory.EmptyState t = uint64Type then Some() else None
    let (|Bool|_|) t = if isBool t then Some() else None
    let (|Float32|_|) t = if Terms.TypeOf Memory.EmptyState t = float32Type then Some() else None
    let (|Float64|_|) t = if Terms.TypeOf Memory.EmptyState t = float64Type then Some() else None
    let (|Float|_|) t = if Terms.TypeOf Memory.EmptyState t |> isFloatTermType then Some() else None

    module Char =
        let Zero = MakeNumber '\000'
    module Int8 =
        let Zero = MakeNumber 0y
    module UInt8 =
        let Zero = MakeNumber 0uy
    module Int16 =
        let Zero = MakeNumber 0s
    module UInt16 =
        let Zero = MakeNumber 0us
    module Int32 =
        let Zero = MakeNumber 0
        let One = MakeNumber 1
        let MinusOne = MakeNumber -1
        let MinValue = MakeNumber Int32.MinValue
        let MaxValue = MakeNumber Int32.MaxValue
    module UInt32 =
        let Zero = MakeNumber 0u
        let MaxValue = MakeNumber UInt32.MaxValue
    module Int64 =
        let Zero = MakeNumber 0L
        let MinusOne = MakeNumber -1L
        let MinValue = MakeNumber Int64.MinValue
        let MaxValue = MakeNumber Int64.MaxValue
    module UInt64 =
        let Zero = MakeNumber 0UL
        let MaxValue = MakeNumber UInt64.MaxValue

module internal InstructionsSet =
    open CilStateOperations

    let idTransformation term k = k term

    // --------------------------------------- Metadata Interaction ----------------------------------------


    let resolveFieldFromMetadata (cfg : cfgData) = Instruction.resolveFieldFromMetadata cfg.methodBase cfg.ilBytes
    let resolveTypeFromMetadata (cfg : cfgData) = Instruction.resolveTypeFromMetadata cfg.methodBase cfg.ilBytes
    let resolveTermTypeFromMetadata (cfg : cfgData) = resolveTypeFromMetadata cfg >> Types.FromDotNetType
    let resolveMethodFromMetadata (cfg : cfgData) = Instruction.resolveMethodFromMetadata cfg.methodBase cfg.ilBytes
    let resolveTokenFromMetadata (cfg : cfgData) = Instruction.resolveTokenFromMetadata cfg.methodBase cfg.ilBytes

    let hashFunction (opcode : OpCode) =
        let v = opcode.Value |> int
        let offsetToMakeReadonlyOpcode256 = 226
        if v >= 0 then v
        else -v - offsetToMakeReadonlyOpcode256

    // ------------------------------- Environment interaction -------------------------------

    let rec internalCall (methodInfo : MethodInfo) (argsAndThis : term list) (s : state) (k : state list -> 'a) =
        let parameters : obj [] =
            // Sometimes F# compiler merges tuple with the rest arguments!
            match methodInfo.GetParameters().Length with
            | 2 -> [| s; argsAndThis |]
// TODO: this should be now broken!
//            | 6 -> [| s.stack; s.heap; s.statics; s.frames; s.pc; argsAndThis |]
            | _ -> __notImplemented__()
        let result =
            try
                methodInfo.Invoke(null, parameters)
            with
            | :? TargetInvocationException as targetException ->
                Logger.trace "InternalCall got TargetInvocationException %s" targetException.Message
                let actualException = targetException.GetBaseException()
                Logger.trace "TargetInvocationException.GetBaseException %s" actualException.Message
                raise <| actualException
            | e ->
                Logger.trace "InternalCall got exception %s" e.Message
                raise e

        let appendResultToState (term : term, state : state) =
            match term.term with
            | Nop -> {state with returnRegister = None}
            | _ -> {state with returnRegister = Some term}
        match result with
        | :? (term * state) as r -> r |> appendResultToState |> List.singleton |> k
        | :? ((term * state) list) as r -> r |> List.map appendResultToState |> k
        | _ -> internalfail "internal call should return tuple term * state!"


    // ------------------------------- CIL instructions -------------------------------

    let referenceLocalVariable index (methodBase : MethodBase) =
        let lvi = methodBase.GetMethodBody().LocalVariables.[index]
        let stackKey = LocalVariableKey(lvi, methodBase)
        Ref (PrimitiveStackLocation stackKey)
    let getArgTerm index (methodBase : MethodBase) =
        let pi = methodBase.GetParameters().[index]
        PrimitiveStackLocation (ParameterKey pi) |> Ref

    let castReferenceToPointerIfNeeded term typ state =
        if IsReference term && Types.IsPointer typ
        then Types.CastReferenceToPointer state term
        else term
    let castUnchecked typ term (state : state) : term =
        let term = castReferenceToPointerIfNeeded term typ state
        Types.Cast term typ
    let ldc numberCreator t (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let num = numberCreator cfg.ilBytes shiftedOffset
        let termType = Types.FromDotNetType t
        push (Concrete num termType) cilState |> List.singleton

    let ldloc numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let index = numberCreator cfg.ilBytes shiftedOffset
        let reference = referenceLocalVariable index cfg.methodBase
        let term = Memory.ReadSafe cilState.state reference
        push term cilState |> List.singleton

    let ldarg numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator cfg.ilBytes shiftedOffset
        let arg =
            let state = cilState.state
            let this = if cfg.methodBase.IsStatic then None else Some <| Memory.ReadThis state cfg.methodBase
            match this, cfg.methodBase.IsStatic with
            | None, _
            | Some _, true ->
                let term = getArgTerm argumentIndex cfg.methodBase
                Memory.ReadSafe state term
            | Some this, _ when argumentIndex = 0 -> this
            | Some _, false ->
                let term = getArgTerm (argumentIndex - 1) cfg.methodBase
                Memory.ReadSafe state term
        push arg cilState |> List.singleton
    let ldarga numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let argumentIndex = numberCreator cfg.ilBytes shiftedOffset
        let address =
            let this = if cfg.methodBase.IsStatic then None else Some <| Memory.ReadThis cilState.state cfg.methodBase
            match this with
            | None -> getArgTerm argumentIndex cfg.methodBase
            | Some _ when argumentIndex = 0 -> internalfail "can't load address of ``this''"
            | Some _ -> getArgTerm (argumentIndex - 1) cfg.methodBase
        push address cilState |> List.singleton
    let stloc numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let variableIndex = numberCreator cfg.ilBytes shiftedOffset
        let right, cilState = pop cilState
        let left = referenceLocalVariable variableIndex cfg.methodBase
        let state = cilState.state
        let typ = TypeOf state left
        let value = castUnchecked typ right state
        let states = Memory.WriteSafe state left value
        states |> List.map (fun state -> cilState |> withState state)
    let private simplifyConditionResult state res k =
        if WithPathCondition state !!res |> IsFalsePathCondition then k True
        elif WithPathCondition state res |> IsFalsePathCondition then k False
        else k res
    let performCILUnaryOperation op (cilState : cilState) =
        let x, cilState = pop cilState
        API.PerformUnaryOperation op x (fun interimRes ->
        let res = if Terms.TypeOf cilState.state x |> Types.IsBool then simplifyConditionResult cilState.state interimRes id else interimRes
        push res cilState |> List.singleton)

    let performCILBinaryOperation op operand1Transform operand2Transform resultTransform (cilState : cilState) =
        let arg2, arg1, cilState = pop2 cilState
        operand1Transform arg1 (fun arg1 ->
        operand2Transform arg2 (fun arg2 ->
        API.PerformBinaryOperation op arg1 arg2 (fun interimRes ->
        resultTransform interimRes (fun res ->
        push res cilState |> List.singleton))))
    let standardPerformBinaryOperation op =
        performCILBinaryOperation op idTransformation idTransformation idTransformation
    let dup (cilState : cilState) =
        let x, cilState = pop cilState
        cilState |> push x |> push x |> List.singleton

    let ret (cfg : cfgData) _ (cilState : cilState) =
        let resultTyp =
            match cfg.methodBase with
            | :? ConstructorInfo -> Void
            | :? MethodInfo as mi -> Types.FromDotNetType mi.ReturnType
            | _ -> __notImplemented__()
        let term, cilState =
            if not <| Reflection.HasNonVoidResult cfg.methodBase then None, cilState
            else pop cilState |> mapfst Some
        let typ =
            match term with
            | Some t -> TypeOf cilState.state t
            | None -> Void
        match term, resultTyp with
        | None, Void -> cilState :: []
        | Some t, _ when typ = resultTyp -> cilState |> withResult t |> List.singleton // TODO: [simplification] remove this heuristics
        | Some t, _ ->
            let t = castUnchecked resultTyp t cilState.state
            cilState |> withResult t |> List.singleton
        | _ -> __unreachable__()
    let transform2BooleanTerm state (term : term) =
        let check term =
            match TypeOf state term with
            | Bool -> term
            | t when t = TypeUtils.charType -> term !== TypeUtils.Char.Zero
            | t when t = TypeUtils.int8Type -> term !== TypeUtils.Int8.Zero
            | t when t = TypeUtils.uint8Type -> term !== TypeUtils.UInt8.Zero
            | t when t = TypeUtils.int16Type -> term !== TypeUtils.Int16.Zero
            | t when t = TypeUtils.uint16Type -> term !== TypeUtils.UInt16.Zero
            | t when t = TypeUtils.int32Type -> term !== TypeUtils.Int32.Zero
            | t when t = TypeUtils.uint32Type -> term !== TypeUtils.UInt32.Zero
            | t when t = TypeUtils.int64Type -> term !== TypeUtils.Int64.Zero
            | t when t = TypeUtils.uint64Type -> term !== TypeUtils.UInt64.Zero
            | Numeric(Id t) when t.IsEnum ->
                term !== MakeNumber (t.GetEnumValues().GetValue(0))
            | _ when IsReference term -> !!(IsNullReference term)
            | _ -> __notImplemented__()
        GuardedApplyExpressionWithPC state.pc term check

    let binaryOperationWithBoolResult op operand1Transformation operand2Transformation (cilState : cilState) =
        performCILBinaryOperation op operand1Transformation operand2Transformation (simplifyConditionResult cilState.state) cilState

    let ceq (cilState : cilState) =
        let y, x, _ = pop2 cilState
        let transform =
            if TypeUtils.isBool x || TypeUtils.isBool y
            then fun t k -> k (transform2BooleanTerm cilState.state t)
            else idTransformation
        binaryOperationWithBoolResult OperationType.Equal transform transform cilState
    let starg numCreator (cfg : cfgData) offset (cilState : cilState) =
        let argumentIndex = numCreator cfg.ilBytes offset
        let argTerm =
           let this = if cfg.methodBase.IsStatic then None else Some <| Memory.ReadThis cilState.state cfg.methodBase
           match this with
           | None -> getArgTerm argumentIndex cfg.methodBase
           | Some this when argumentIndex = 0 -> this
           | Some _ -> getArgTerm (argumentIndex - 1) cfg.methodBase
        let value, cilState = pop cilState
        let states = Memory.WriteSafe cilState.state argTerm value
        states |> List.map (fun state -> cilState |> withState state)
    let brcommon condTransform offsets (cilState : cilState) =
        let cond, cilState = pop cilState
        let offsetThen, offsetElse =
            match offsets with
            | [offsetThen; offsetElse] -> offsetThen, offsetElse
            | _ -> __unreachable__()
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (condTransform <| transform2BooleanTerm state cond, state))
            (fun cilState k -> k [offsetThen, cilState])
            (fun cilState k -> k [offsetElse, cilState])
            id
    let brfalse = brcommon id
    let brtrue = brcommon (!!)
    let applyAndBranch errorStr additionalFunction brtrueFunction (cfg : cfgData) offset newOffsets (cilState : cilState) =
        match additionalFunction cfg offset [Instruction offset] cilState with
        | [_, st] -> brtrueFunction newOffsets st
        | _ -> internalfail errorStr
    let boolToInt b =
        BranchExpressions (fun k -> k b) (fun k -> k TypeUtils.Int32.One) (fun k -> k TypeUtils.Int32.Zero) id
    let bitwiseOrBoolOperation op (cilState : cilState) =
        let arg2, arg1, _ = pop2 cilState
        let typ1, typ2 = TypeOf cilState.state arg1, TypeOf cilState.state arg2
        match typ1, typ2 with
        | Bool, Bool ->
            binaryOperationWithBoolResult op idTransformation idTransformation cilState
        | _ when TypeUtils.isIntegerTermType typ1 && TypeUtils.isIntegerTermType typ2 ->
            standardPerformBinaryOperation op cilState
        | Bool, typ2 when TypeUtils.isIntegerTermType typ2 ->
            let newArg1 = boolToInt arg1
            performCILBinaryOperation op (fun _ k -> k newArg1) idTransformation idTransformation cilState
        | typ1, Bool when TypeUtils.isIntegerTermType typ1 ->
            let newArg2 = boolToInt arg2
            performCILBinaryOperation op idTransformation (fun _ k -> k newArg2) idTransformation cilState
        | typ1, typ2 -> internalfailf "unhandled case for Bitwise operation %O and types: %O %O" op typ1 typ2
    let retrieveActualParameters (methodBase : MethodBase) (cilState : cilState) =
        let paramsNumber = methodBase.GetParameters().Length
        let parameters, opStack = Memory.PopArgumentsFromOpStack paramsNumber cilState.state.opStack
        let castParameter parameter (parInfo : ParameterInfo) =
            if Reflection.IsDelegateConstructor methodBase && parInfo.ParameterType = typeof<IntPtr> then parameter
            else
                let typ = Types.FromDotNetType parInfo.ParameterType
                castUnchecked typ parameter cilState.state
        let parameters = Seq.map2 castParameter (List.rev parameters) (methodBase.GetParameters()) |> List.ofSeq
        parameters, withOpStack opStack cilState

    let makeUnsignedInteger term k =
        let typ = Terms.TypeOf Memory.EmptyState term
        let unsignedTyp = TypeUtils.signed2unsignedOrId typ
        if TypeUtils.isIntegerTermType typ && typ <> unsignedTyp then
            k <| Types.Cast term unsignedTyp // no specs found about overflows
        else k term
    let performUnsignedIntegerOperation op (cilState : cilState) =
        let arg2, arg1, _ = pop2 cilState
        if TypeUtils.isInteger arg1 && TypeUtils.isInteger arg2 then
            performCILBinaryOperation op makeUnsignedInteger makeUnsignedInteger idTransformation cilState
        else internalfailf "arguments for %O are not Integers!" op
    let ldstr (cfg : cfgData) offset (cilState : cilState) =
        let stringToken = NumberCreator.extractInt32 cfg.ilBytes (offset + OpCodes.Ldstr.Size)
        let string = cfg.methodBase.Module.ResolveString stringToken
        let reference, state = Memory.AllocateString string cilState.state
        cilState |> withState state |> push reference |> List.singleton
    let allocateValueTypeInHeap v (cilState : cilState) =
        let address, state = Memory.BoxValueType cilState.state v
        cilState |> withState state |> push address |> List.singleton
    let ldnull (cilState : cilState) = push NullRef cilState |> List.singleton
    let convu (cilState : cilState) =
        let value, cilState = pop cilState
        if IsReference value then
            let ptr = Types.CastReferenceToPointer cilState.state value
            cilState |> push ptr |> List.singleton
        else cilState :: []
    let convi (cilState : cilState) = cilState :: []
    let castTopOfOperationalStackUnchecked targetType (cilState : cilState) =
        let t, cilState = pop cilState
        let termForStack = castUnchecked targetType t cilState.state
        cilState |> push termForStack |> List.singleton
    let ldloca numberCreator (cfg : cfgData) shiftedOffset (cilState : cilState) =
        let index = numberCreator cfg.ilBytes shiftedOffset
        let term = referenceLocalVariable index cfg.methodBase
        push term cilState |> List.singleton
    let switch newOffsets (cilState : cilState) =
        let value, cilState = pop cilState
        let value = makeUnsignedInteger value id
        let checkOneCase (guard, newOffset) cilState kRestCases =
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (guard, state))
                (fun cilState k -> k [newOffset, cilState])
                (fun _ k -> kRestCases cilState k) // ignore pc because we always know that cases do not overlap
        let fallThroughOffset, newOffsets = List.head newOffsets, List.tail newOffsets
        let casesAndOffsets = List.mapi (fun i offset -> value === MakeNumber i, offset) newOffsets
        let fallThroughGuard = Arithmetics.(>>=) value (List.length newOffsets |> MakeNumber)
        Cps.List.foldrk checkOneCase cilState ((fallThroughGuard, fallThroughOffset)::casesAndOffsets) (fun _ k -> k []) id
    let ldtoken (cfg : cfgData) offset (cilState : cilState) =
        let memberInfo = resolveTokenFromMetadata cfg (offset + OpCodes.Ldtoken.Size)
        let res =
            match memberInfo with
            | :? FieldInfo as fi -> Terms.Concrete fi.FieldHandle (Types.FromDotNetType typeof<RuntimeFieldHandle>)
            | :? Type as t -> Terms.Concrete t.TypeHandle (Types.FromDotNetType typeof<RuntimeTypeHandle>)
            | :? MethodInfo as mi -> Terms.Concrete mi.MethodHandle (Types.FromDotNetType typeof<RuntimeMethodHandle>)
            | _ -> internalfailf "Could not resolve token"
        push res cilState |> List.singleton
    let ldftn (cfg : cfgData) offset (cilState : cilState) =
        let methodInfo = resolveMethodFromMetadata cfg (offset + OpCodes.Ldftn.Size)
        let methodPtr = Terms.Concrete methodInfo (Types.FromDotNetType (methodInfo.GetType()))
        push methodPtr cilState |> List.singleton
    let initobj (cfg : cfgData) offset (cilState : cilState) =
        let targetAddress, cilState = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Initobj.Size)
        let states = Memory.WriteSafe cilState.state targetAddress (Memory.DefaultOf typ)
        states |> List.map (fun state -> cilState |> withState state)
    let ldind valueCast (cilState : cilState) =
        let address, cilState = pop cilState
        let value = Memory.ReadSafe cilState.state address
        let value = valueCast value cilState.state
        cilState |> push value |> List.singleton
    let ldindref = ldind always
    let clt = binaryOperationWithBoolResult OperationType.Less idTransformation idTransformation
    let cltun = binaryOperationWithBoolResult OperationType.Less makeUnsignedInteger makeUnsignedInteger
    let bgeHelper (cilState : cilState) =
        let arg1, arg2, _ = pop2 cilState
        let typ1, typ2 = Terms.TypeOf cilState.state arg1, Terms.TypeOf cilState.state arg2
        if Types.IsInteger typ1 && Types.IsInteger typ2 then clt cilState
        elif Types.IsReal typ1 && Types.IsReal typ2 then cltun cilState
        else __notImplemented__()
    let isinst (cfg : cfgData) offset (cilState : cilState) =
        let object, cilState = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Isinst.Size)
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference object, state))
            (fun cilState k -> cilState |> push NullRef |> List.singleton |> k)
            (fun cilState k ->
                StatedConditionalExecutionCIL cilState
                    (fun state k -> k (Types.IsCast cilState.state object typ, state))
                    (fun cilState k -> cilState |> push object |> List.singleton |> k)
                    (fun cilState k -> cilState |> push NullRef |> List.singleton |> k)
                    k)
            id
    let cgtun (cilState : cilState) =
        let arg2, arg1, _ = pop2 cilState
        if IsReference arg2 && IsReference arg1 then
            binaryOperationWithBoolResult OperationType.NotEqual idTransformation idTransformation cilState
        else binaryOperationWithBoolResult OperationType.Greater makeUnsignedInteger makeUnsignedInteger cilState
    let ldobj (cfg : cfgData) offset (cilState : cilState) =
        let address, cilState = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Ldobj.Size)
        let value = Memory.ReadSafe cilState.state address
        let typedValue = castUnchecked typ value cilState.state
        cilState |> push typedValue |> List.singleton
    let stobj (cfg : cfgData) offset (cilState : cilState) =
        let src, dest, cilState = pop2 cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Stobj.Size)
        let value = castUnchecked typ src cilState.state
        let states = Memory.WriteSafe cilState.state dest value
        states |> List.map (fun state -> cilState |> withState state)
    let stind typ (cilState : cilState) =
        let value, address, cilState = pop2 cilState
        let value = castUnchecked typ value cilState.state
        let states = Memory.WriteSafe cilState.state address value
        states |> List.map (fun state -> cilState |> withState state)
    let sizeofInstruction (cfg : cfgData) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Sizeof.Size)
        let size = Types.SizeOf typ
        cilState |> push (MakeNumber size) |> List.singleton
    let throw _ _ (cilState : cilState) =
        let error, _ = pop cilState
        cilState |> withException (Unhandled error) |> withOpStack emptyOpStack |> List.singleton
    let leave _ _ (cilState : cilState) = cilState :: []
    let rethrow _ _ (cilState : cilState) =
        let state = cilState.state
        assert(Option.isSome state.exceptionsRegister.ExceptionTerm)
        let state = {state with exceptionsRegister = state.exceptionsRegister.TransformToUnhandled()}
        { cilState with state = state} |> List.singleton
    let endfilter _ _ (cilState : cilState) =
        let value, tmp = pop cilState
        if tmp.state.opStack = emptyOpStack then {cilState with filterResult = Some value} :: []
        else __notImplemented__()
    let endfinally _ _ (cilState : cilState) =
        cilState |> withOpStack emptyOpStack |> List.singleton
    let zipWithOneOffset op cfgData offset newOffsets cilState =
        assert(List.length newOffsets = 1)
        let newOffset = List.head newOffsets
        let cilStates = op cfgData offset cilState
        List.map (withFst newOffset) cilStates

    let opcode2Function : (cfgData -> offset -> ip list -> cilState -> (ip * cilState) list) [] = Array.create 300 (fun _ _ _ -> internalfail "Interpreter is not ready")
    opcode2Function.[hashFunction OpCodes.Br]                 <- zipWithOneOffset <| fun _ _ cilState -> cilState :: []
    opcode2Function.[hashFunction OpCodes.Br_S]               <- zipWithOneOffset <| fun _ _ cilState -> cilState :: []
    opcode2Function.[hashFunction OpCodes.Add]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.Add // TODO: check float overflow [spec]
    opcode2Function.[hashFunction OpCodes.Mul]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.Multiply
    opcode2Function.[hashFunction OpCodes.Sub]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.Subtract
    opcode2Function.[hashFunction OpCodes.Shl]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.ShiftLeft
    opcode2Function.[hashFunction OpCodes.Shr]                <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.ShiftRight
    opcode2Function.[hashFunction OpCodes.Shr_Un]             <- zipWithOneOffset <| fun _ _ -> standardPerformBinaryOperation OperationType.ShiftRight // TODO: implement using specification
    opcode2Function.[hashFunction OpCodes.Ceq]                <- zipWithOneOffset <| fun _ _ -> ceq
    opcode2Function.[hashFunction OpCodes.Cgt]                <- zipWithOneOffset <| fun _ _ -> binaryOperationWithBoolResult OperationType.Greater idTransformation idTransformation
    opcode2Function.[hashFunction OpCodes.Cgt_Un]             <- zipWithOneOffset <| fun _ _ -> cgtun
    opcode2Function.[hashFunction OpCodes.Clt]                <- zipWithOneOffset <| fun _ _ -> clt
    opcode2Function.[hashFunction OpCodes.Clt_Un]             <- zipWithOneOffset <| fun _ _ -> cltun
    opcode2Function.[hashFunction OpCodes.And]                <- zipWithOneOffset <| fun _ _ -> bitwiseOrBoolOperation OperationType.LogicalAnd
    opcode2Function.[hashFunction OpCodes.Or]                 <- zipWithOneOffset <| fun _ _ -> bitwiseOrBoolOperation OperationType.LogicalOr
    opcode2Function.[hashFunction OpCodes.Xor]                <- zipWithOneOffset <| fun _ _ -> bitwiseOrBoolOperation OperationType.LogicalXor
    opcode2Function.[hashFunction OpCodes.Neg]                <- zipWithOneOffset <| fun _ _ -> performCILUnaryOperation OperationType.UnaryMinus
    opcode2Function.[hashFunction OpCodes.Not]                <- zipWithOneOffset <| fun _ _ -> performCILUnaryOperation OperationType.LogicalNeg
    opcode2Function.[hashFunction OpCodes.Stloc]              <- zipWithOneOffset <| stloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Stloc.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Stloc_0]            <- zipWithOneOffset <| stloc (fun _ _ -> 0)
    opcode2Function.[hashFunction OpCodes.Stloc_1]            <- zipWithOneOffset <| stloc (fun _ _ -> 1)
    opcode2Function.[hashFunction OpCodes.Stloc_2]            <- zipWithOneOffset <| stloc (fun _ _ -> 2)
    opcode2Function.[hashFunction OpCodes.Stloc_3]            <- zipWithOneOffset <| stloc (fun _ _ -> 3)
    opcode2Function.[hashFunction OpCodes.Stloc_S]            <- zipWithOneOffset <| stloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Stloc_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Starg]              <- zipWithOneOffset <| starg (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Starg.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Starg_S]            <- zipWithOneOffset <| starg (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Starg_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldc_I4]             <- zipWithOneOffset <| ldc (fun ilBytes offset -> NumberCreator.extractInt32 ilBytes (offset + OpCodes.Ldc_I4.Size)) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_0]           <- zipWithOneOffset <| ldc (fun _ _ -> 0) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_1]           <- zipWithOneOffset <| ldc (fun _ _ -> 1) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_2]           <- zipWithOneOffset <| ldc (fun _ _ -> 2) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_3]           <- zipWithOneOffset <| ldc (fun _ _ -> 3) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_4]           <- zipWithOneOffset <| ldc (fun _ _ -> 4) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_5]           <- zipWithOneOffset <| ldc (fun _ _ -> 5) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_6]           <- zipWithOneOffset <| ldc (fun _ _ -> 6) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_7]           <- zipWithOneOffset <| ldc (fun _ _ -> 7) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_8]           <- zipWithOneOffset <| ldc (fun _ _ -> 8) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_M1]          <- zipWithOneOffset <| ldc (fun _ _ -> -1) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I4_S]           <- zipWithOneOffset <| ldc (fun ilBytes offset -> NumberCreator.extractInt8 ilBytes (offset + OpCodes.Ldc_I4_S.Size)) typedefof<int32>
    opcode2Function.[hashFunction OpCodes.Ldc_I8]             <- zipWithOneOffset <| ldc (fun ilBytes offset -> NumberCreator.extractInt64 ilBytes (offset + OpCodes.Ldc_I8.Size)) typedefof<int64>
    opcode2Function.[hashFunction OpCodes.Ldc_R4]             <- zipWithOneOffset <| ldc (fun ilBytes offset -> NumberCreator.extractFloat32 ilBytes (offset + OpCodes.Ldc_R4.Size)) typedefof<float32>
    opcode2Function.[hashFunction OpCodes.Ldc_R8]             <- zipWithOneOffset <| ldc (fun ilBytes offset -> NumberCreator.extractFloat64 ilBytes (offset + OpCodes.Ldc_R8.Size)) typedefof<double>
    opcode2Function.[hashFunction OpCodes.Ldarg]              <- zipWithOneOffset <| ldarg (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldarg.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldarg_0]            <- zipWithOneOffset <| ldarg (fun _ _ -> 0)
    opcode2Function.[hashFunction OpCodes.Ldarg_1]            <- zipWithOneOffset <| ldarg (fun _ _ -> 1)
    opcode2Function.[hashFunction OpCodes.Ldarg_2]            <- zipWithOneOffset <| ldarg (fun _ _ -> 2)
    opcode2Function.[hashFunction OpCodes.Ldarg_3]            <- zipWithOneOffset <| ldarg (fun _ _ -> 3)
    opcode2Function.[hashFunction OpCodes.Ldarg_S]            <- zipWithOneOffset <| ldarg (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldarg_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Nop]                <- zipWithOneOffset <| fun _ _ st -> [st]
    opcode2Function.[hashFunction OpCodes.Ldloc]              <- zipWithOneOffset <| ldloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldloc.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldloc_0]            <- zipWithOneOffset <| ldloc (fun _ _ -> 0)
    opcode2Function.[hashFunction OpCodes.Ldloc_1]            <- zipWithOneOffset <| ldloc (fun _ _ -> 1)
    opcode2Function.[hashFunction OpCodes.Ldloc_2]            <- zipWithOneOffset <| ldloc (fun _ _ -> 2)
    opcode2Function.[hashFunction OpCodes.Ldloc_3]            <- zipWithOneOffset <| ldloc (fun _ _ -> 3)
    opcode2Function.[hashFunction OpCodes.Ldloc_S]            <- zipWithOneOffset <| ldloc (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldloc_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldloca]             <- zipWithOneOffset <| ldloca (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldloca.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldloca_S]           <- zipWithOneOffset <| ldloca (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldloca_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ret]                <- zipWithOneOffset <| ret
    opcode2Function.[hashFunction OpCodes.Dup]                <- zipWithOneOffset <| fun _ _ -> dup

    // branching
    opcode2Function.[hashFunction OpCodes.Brfalse]            <- fun _ _ -> brfalse
    opcode2Function.[hashFunction OpCodes.Brfalse_S]          <- fun _ _ -> brfalse
    opcode2Function.[hashFunction OpCodes.Brtrue]             <- fun _ _ -> brtrue
    opcode2Function.[hashFunction OpCodes.Brtrue_S]           <- fun _ _ -> brtrue
    opcode2Function.[hashFunction OpCodes.Beq]                <- applyAndBranch "Beq" (opcode2Function.[hashFunction OpCodes.Ceq]) brtrue
    opcode2Function.[hashFunction OpCodes.Beq_S]              <- applyAndBranch "Beq_S" (opcode2Function.[hashFunction OpCodes.Ceq]) brtrue
    opcode2Function.[hashFunction OpCodes.Bge]                <- applyAndBranch "Bge" (zipWithOneOffset <| fun _ _ -> bgeHelper) brfalse
    opcode2Function.[hashFunction OpCodes.Bge_S]              <- applyAndBranch "Bge_S" (zipWithOneOffset <| fun _ _ -> bgeHelper) brfalse

    opcode2Function.[hashFunction OpCodes.Bgt]                <- applyAndBranch "Bgt" (opcode2Function.[hashFunction OpCodes.Cgt]) brtrue
    opcode2Function.[hashFunction OpCodes.Bgt_S]              <- applyAndBranch "Bgt_S" (opcode2Function.[hashFunction OpCodes.Cgt]) brtrue
    opcode2Function.[hashFunction OpCodes.Bgt_Un]             <- applyAndBranch "Bgt_Un" (opcode2Function.[hashFunction OpCodes.Cgt_Un]) brtrue
    opcode2Function.[hashFunction OpCodes.Bgt_Un_S]           <- applyAndBranch "Bgt_Un_S" (opcode2Function.[hashFunction OpCodes.Cgt_Un]) brtrue
    opcode2Function.[hashFunction OpCodes.Ble]                <- applyAndBranch "Ble" (opcode2Function.[hashFunction OpCodes.Cgt]) brfalse
    opcode2Function.[hashFunction OpCodes.Ble_S]              <- applyAndBranch "Ble_S" (opcode2Function.[hashFunction OpCodes.Cgt]) brfalse
    opcode2Function.[hashFunction OpCodes.Ble_Un]             <- applyAndBranch "Ble_Un" (opcode2Function.[hashFunction OpCodes.Cgt_Un]) brfalse
    opcode2Function.[hashFunction OpCodes.Ble_Un_S]           <- applyAndBranch "Ble_Un_S" (opcode2Function.[hashFunction OpCodes.Cgt_Un]) brfalse
    opcode2Function.[hashFunction OpCodes.Blt]                <- applyAndBranch "Blt" (opcode2Function.[hashFunction OpCodes.Clt]) brtrue
    opcode2Function.[hashFunction OpCodes.Blt_S]              <- applyAndBranch "Blt_S" (opcode2Function.[hashFunction OpCodes.Clt]) brtrue
    opcode2Function.[hashFunction OpCodes.Blt_Un]             <- applyAndBranch "Blt_Un" (opcode2Function.[hashFunction OpCodes.Clt_Un]) brtrue
    opcode2Function.[hashFunction OpCodes.Blt_Un_S]           <- applyAndBranch "Blt_Un_S" (opcode2Function.[hashFunction OpCodes.Clt_Un]) brtrue
    opcode2Function.[hashFunction OpCodes.Bne_Un]             <- applyAndBranch "Bne_Un" (opcode2Function.[hashFunction OpCodes.Ceq]) brfalse
    opcode2Function.[hashFunction OpCodes.Bne_Un_S]           <- applyAndBranch "Bne_Un_S" (opcode2Function.[hashFunction OpCodes.Ceq]) brfalse
    opcode2Function.[hashFunction OpCodes.Bge_Un]             <- applyAndBranch "Bge_Un" (opcode2Function.[hashFunction OpCodes.Clt_Un]) brfalse
    opcode2Function.[hashFunction OpCodes.Bge_Un_S]           <- applyAndBranch "Bge_Un_S" (opcode2Function.[hashFunction OpCodes.Clt_Un]) brfalse

    opcode2Function.[hashFunction OpCodes.Ldstr]              <- zipWithOneOffset <| ldstr
    opcode2Function.[hashFunction OpCodes.Ldnull]             <- zipWithOneOffset <| fun _ _ -> ldnull
    opcode2Function.[hashFunction OpCodes.Conv_I1]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int8Type
    opcode2Function.[hashFunction OpCodes.Conv_I2]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int16Type
    opcode2Function.[hashFunction OpCodes.Conv_I4]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int32Type
    opcode2Function.[hashFunction OpCodes.Conv_I8]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.int64Type
    opcode2Function.[hashFunction OpCodes.Conv_R4]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float32Type
    opcode2Function.[hashFunction OpCodes.Conv_R8]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float64Type
    opcode2Function.[hashFunction OpCodes.Conv_U1]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint8Type
    opcode2Function.[hashFunction OpCodes.Conv_U2]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint16Type
    opcode2Function.[hashFunction OpCodes.Conv_U4]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint32Type
    opcode2Function.[hashFunction OpCodes.Conv_U8]            <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.uint64Type
    opcode2Function.[hashFunction OpCodes.Conv_I]             <- zipWithOneOffset <| fun _ _ -> convi //castTopOfOperationalStackUnchecked TypeUtils.nativeintType TypeUtils.nativeintType
    opcode2Function.[hashFunction OpCodes.Conv_U]             <- zipWithOneOffset <| fun _ _ -> convu //castTopOfOperationalStackUnchecked TypeUtils.unativeintType TypeUtils.nativeintType
    opcode2Function.[hashFunction OpCodes.Conv_R_Un]          <- zipWithOneOffset <| fun _ _ -> castTopOfOperationalStackUnchecked TypeUtils.float64Type
    opcode2Function.[hashFunction OpCodes.Switch]             <- fun _ _ -> switch
    opcode2Function.[hashFunction OpCodes.Ldtoken]            <- zipWithOneOffset <| ldtoken
    opcode2Function.[hashFunction OpCodes.Ldftn]              <- zipWithOneOffset <| ldftn
    opcode2Function.[hashFunction OpCodes.Pop]                <- zipWithOneOffset <| fun _ _ st -> pop st |> snd |> List.singleton
    opcode2Function.[hashFunction OpCodes.Initobj]            <- zipWithOneOffset <| initobj
    opcode2Function.[hashFunction OpCodes.Ldarga]             <- zipWithOneOffset <| ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt16 ilBytes (offset + OpCodes.Ldarga.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldarga_S]           <- zipWithOneOffset <| ldarga (fun ilBytes offset -> NumberCreator.extractUnsignedInt8 ilBytes (offset + OpCodes.Ldarga_S.Size) |> int)
    opcode2Function.[hashFunction OpCodes.Ldind_I4]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.int32Type)
    opcode2Function.[hashFunction OpCodes.Ldind_I1]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.int8Type)
    opcode2Function.[hashFunction OpCodes.Ldind_I2]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.int16Type)
    opcode2Function.[hashFunction OpCodes.Ldind_I8]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.int64Type)
    opcode2Function.[hashFunction OpCodes.Ldind_U1]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.uint8Type)
    opcode2Function.[hashFunction OpCodes.Ldind_U2]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.uint16Type)
    opcode2Function.[hashFunction OpCodes.Ldind_U4]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.uint32Type)
    opcode2Function.[hashFunction OpCodes.Ldind_R4]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.float32Type)
    opcode2Function.[hashFunction OpCodes.Ldind_R8]           <- zipWithOneOffset <| fun _ _ -> ldind (castUnchecked <| TypeUtils.float64Type)
    opcode2Function.[hashFunction OpCodes.Ldind_Ref]          <- zipWithOneOffset <| fun _ _ -> ldindref
    opcode2Function.[hashFunction OpCodes.Ldind_I]            <- zipWithOneOffset <| fun _ _ -> ldind always
    opcode2Function.[hashFunction OpCodes.Isinst]             <- zipWithOneOffset isinst
    opcode2Function.[hashFunction OpCodes.Stobj]              <- zipWithOneOffset <| stobj
    opcode2Function.[hashFunction OpCodes.Ldobj]              <- zipWithOneOffset <| ldobj
    opcode2Function.[hashFunction OpCodes.Stind_I]            <- Options.HandleNativeInt opcode2Function.[hashFunction OpCodes.Stind_I4] opcode2Function.[hashFunction OpCodes.Stind_I8]
    opcode2Function.[hashFunction OpCodes.Stind_I1]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.int8Type
    opcode2Function.[hashFunction OpCodes.Stind_I2]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.int16Type
    opcode2Function.[hashFunction OpCodes.Stind_I4]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.int32Type
    opcode2Function.[hashFunction OpCodes.Stind_I8]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.int64Type
    opcode2Function.[hashFunction OpCodes.Stind_R4]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.float32Type
    opcode2Function.[hashFunction OpCodes.Stind_R8]           <- zipWithOneOffset <| fun _ _ -> stind TypeUtils.float64Type
    opcode2Function.[hashFunction OpCodes.Stind_Ref]          <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Sizeof]             <- zipWithOneOffset <| sizeofInstruction
    opcode2Function.[hashFunction OpCodes.Throw]              <- zipWithOneOffset <| throw
    opcode2Function.[hashFunction OpCodes.Leave]              <- zipWithOneOffset <| leave
    opcode2Function.[hashFunction OpCodes.Leave_S]            <- zipWithOneOffset <| leave
    opcode2Function.[hashFunction OpCodes.Endfinally]         <- zipWithOneOffset <| endfinally
    opcode2Function.[hashFunction OpCodes.Rethrow]            <- zipWithOneOffset <| rethrow
    opcode2Function.[hashFunction OpCodes.Endfilter]          <- zipWithOneOffset <| endfilter

    // TODO: notImplemented instructions
    opcode2Function.[hashFunction OpCodes.Stelem_I]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Arglist]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Jmp]                <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Break]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Calli]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Ckfinite]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Constrained]        <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Cpblk]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Cpobj]              <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Localloc]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Ldelema]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Ldelem_I]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Mkrefany]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix1]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix2]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix3]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix4]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix5]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix6]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefix7]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Prefixref]          <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Readonly]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Refanytype]         <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Refanyval]          <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Tailcall]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Unaligned]          <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Volatile]           <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
    opcode2Function.[hashFunction OpCodes.Initblk]            <- zipWithOneOffset <| (fun _ _ _ -> Prelude.__notImplemented__())
