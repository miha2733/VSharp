namespace VSharp.Interpreter.IL

open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open FSharpx.Collections
open InstructionsSet
open CilStateOperations
open VSharp
open VSharp.Core

type cfg = CFG.cfgData

type public MethodInterpreter((*ilInterpreter : ILInterpreter, funcId : IFunctionIdentifier, cfg : cfg*)) =
    inherit ExplorerBase()
    let results = Dictionary<IFunctionIdentifier, List<cilState>>()
    let workingSet = Dictionary<IFunctionIdentifier, List<cilState>>()
    let exceptionsSet = Dictionary<IFunctionIdentifier, List<cilState>>()
    let incompleteStatesSet = Dictionary<IFunctionIdentifier, List<cilState>>()

    let (|CilStateWithIIE|_|) (cilState : cilState) = cilState.iie

    static let cfgs = Dictionary<IFunctionIdentifier, cfg>()
    static let findCfg (ilmm : IFunctionIdentifier) =
        Dict.getValueOrUpdate cfgs ilmm (fun () -> CFG.build ilmm.Method)

    let maxBound = 10u // 10u is caused by number of iterations for tests: Always18, FirstEvenGreaterThen7

    member private x.Used k (cilState : cilState) =
        if PersistentDict.contains k cilState.level then
            PersistentDict.find cilState.level k >= maxBound
        else false

    member x.Interpret (funcId : IFunctionIdentifier) (start : cilState) : unit =
        let cfg = findCfg funcId

        let merge (x : cilState) (y : cilState) =
            match x.state.returnRegister, y.state.returnRegister with
            | None, None -> Memory.Merge2States x.state y.state |> List.map (withFst None)
            | Some t1, Some t2 -> Memory.Merge2Results (t1, x.state) (t2, y.state) |> List.map (fun (r, s) -> (Some r, s))
            | _ -> internalfail "only one state has result"
            |> List.map (fun (r, s) -> {x with state = {s with returnRegister = r}})
        let addResultOrContinue state =
            if x.IsResultState funcId state then results.[funcId].Add(state)
            match x.FindSimilar funcId state with
            | None -> x.Add funcId state
            | Some similar -> merge state similar |> List.iter (x.Add funcId)
        let rec interpret' (current : cilState) : unit =
            if not <| x.Used (current.ip, cfg.methodBase) current then
                let states = x.EvaluateOneStep (funcId, current)
                states |> List.iter addResultOrContinue
            x.PickNext funcId |> Option.iter interpret'
        interpret' start

    override x.Invoke funcId cilState k =
        workingSet.TryAdd(funcId, List<cilState>()) |> ignore
        results.TryAdd(funcId, List<cilState>()) |> ignore
        exceptionsSet.TryAdd(funcId, List<cilState>()) |> ignore
        incompleteStatesSet.TryAdd(funcId, List<cilState>()) |> ignore
        let cleanSets () =
            results.[funcId] <- List()
            incompleteStatesSet.[funcId] <- List()
        let getResultsAndStates () =
            let results = results.[funcId] |> List.ofSeq
            let incompleteStates = incompleteStatesSet.[funcId] |> List.ofSeq
            let errors = exceptionsSet.[funcId] |> List.ofSeq
            match incompleteStates, errors, results with
            | CilStateWithIIE iie :: _ , _, _ -> cleanSets(); raise iie
            | _ :: _, _, _ -> __unreachable__()
            | _, _ :: _, _ -> internalfailf "exception handling is not implemented yet"
            | _, _, [] -> internalfailf "No states were obtained. Most likely such a situation is a bug. Check it!"
            | _ -> results
        let printResults (cilStates : cilState list) =
            let states = List.fold (fun acc (cilState : cilState) -> acc + Memory.Dump cilState.state + "\n") "" cilStates
            let fullMethodName = Reflection.GetFullMethodName funcId.Method
            Logger.info "For method %O got %i states :\n%O" fullMethodName (List.length cilStates) states
        let interpret (cilState : cilState) =
            { cilState with ip = Instruction 0} |> x.Interpret funcId
            let results = getResultsAndStates ()
            printResults results
            results
        x.InitializeStatics cilState funcId.Method.DeclaringType (List.map interpret >> List.concat >> (fun x -> cleanSets(); k x))

    override x.MakeMethodIdentifier m = { methodBase = m } :> IMethodIdentifier
    abstract member EvaluateOneStep : IFunctionIdentifier * cilState -> cilState list
    default x.EvaluateOneStep (funcId : IFunctionIdentifier, cilState) =
        let cfg = findCfg funcId
        let ilInterpreter = ILInterpreter(x)
        let goodStates, incompleteStates, errors = ilInterpreter.ExecuteAllInstructions cfg cilState
        incompleteStatesSet.[funcId].AddRange(incompleteStates)
        exceptionsSet.[funcId].AddRange(errors)
        goodStates

    member x.Add (funcId :IFunctionIdentifier) cilState = if cilState.ip <> ip.Exit then workingSet.[funcId].Add cilState
    member x.FindSimilar (funcId : IFunctionIdentifier) cilState =
        let areCapableForMerge (st1 : cilState) (st2 : cilState) = st1.state.opStack = st2.state.opStack && st1.ip = st2.ip
        match Seq.tryFindIndex (areCapableForMerge cilState) workingSet.[funcId] with
        | None -> None
        | Some i ->
            let res = Some workingSet.[funcId].[i]
            workingSet.[funcId].RemoveAt i
            res
    member x.IsResultState (funcId : IFunctionIdentifier) (cilState : cilState) =
        // this is a hack, it should be gone with cfa
        let needToAddResult () = not <| Seq.exists ((=) cilState) results.[funcId]
        cilState.ip = ip.Exit && cilState.state.opStack = emptyOpStack && needToAddResult()

    member x.PickNext (funcId : IFunctionIdentifier) =
        if workingSet.[funcId].Count > 0 then
            let st = workingSet.[funcId].[0]
            workingSet.[funcId].RemoveAt 0
            Some st
        else None

and public ILInterpreter(methodInterpreter : MethodInterpreter) as this =
    do
        opcode2Function.[hashFunction OpCodes.Call]           <- zipWithOneOffset <| this.Call
        opcode2Function.[hashFunction OpCodes.Callvirt]       <- zipWithOneOffset <| this.CallVirt
        opcode2Function.[hashFunction OpCodes.Newobj]         <- zipWithOneOffset <| this.NewObj
        opcode2Function.[hashFunction OpCodes.Ldsfld]         <- zipWithOneOffset <| this.LdsFld false
        opcode2Function.[hashFunction OpCodes.Ldsflda]        <- zipWithOneOffset <| this.LdsFld true
        opcode2Function.[hashFunction OpCodes.Stsfld]         <- zipWithOneOffset <| this.StsFld
        opcode2Function.[hashFunction OpCodes.Ldfld]          <- zipWithOneOffset <| this.LdFld false
        opcode2Function.[hashFunction OpCodes.Ldflda]         <- zipWithOneOffset <| this.LdFld true
        opcode2Function.[hashFunction OpCodes.Stfld]          <- zipWithOneOffset <| this.StFld
        opcode2Function.[hashFunction OpCodes.Ldelem]         <- zipWithOneOffset <| this.LdElem
        opcode2Function.[hashFunction OpCodes.Ldelem_I1]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.int8Type
        opcode2Function.[hashFunction OpCodes.Ldelem_I2]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.int16Type
        opcode2Function.[hashFunction OpCodes.Ldelem_I4]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Ldelem_I8]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.int64Type
        opcode2Function.[hashFunction OpCodes.Ldelem_R4]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.float32Type
        opcode2Function.[hashFunction OpCodes.Ldelem_R8]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.float64Type
        opcode2Function.[hashFunction OpCodes.Ldelem_U1]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.uint8Type
        opcode2Function.[hashFunction OpCodes.Ldelem_U2]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.uint16Type
        opcode2Function.[hashFunction OpCodes.Ldelem_U4]      <- zipWithOneOffset <| fun _ _ -> this.LdElemTyp TypeUtils.uint32Type
        opcode2Function.[hashFunction OpCodes.Ldelem_Ref]     <- zipWithOneOffset <| fun _ _ -> this.LdElemRef
        opcode2Function.[hashFunction OpCodes.Stelem]         <- zipWithOneOffset <| this.StElem
        opcode2Function.[hashFunction OpCodes.Stelem_I1]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.int8Type
        opcode2Function.[hashFunction OpCodes.Stelem_I2]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.int16Type
        opcode2Function.[hashFunction OpCodes.Stelem_I4]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Stelem_I8]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.int64Type
        opcode2Function.[hashFunction OpCodes.Stelem_R4]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.float32Type
        opcode2Function.[hashFunction OpCodes.Stelem_R8]      <- zipWithOneOffset <| fun _ _ -> this.StElemTyp TypeUtils.float64Type
        opcode2Function.[hashFunction OpCodes.Stelem_Ref]     <- zipWithOneOffset <| fun _ _ -> this.StElemRef
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I1]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.int8Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I2]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.int16Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I4]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I8]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.int64Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U1]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.uint8Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U2]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.uint16Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U4]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.uint32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U8]    <- zipWithOneOffset <| fun _ _ -> this.ConvOvf TypeUtils.uint64Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I1_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int8Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I2_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int16Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I4_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.int32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I8_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint64Type TypeUtils.int64Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U1_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint8Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U2_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint16Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U4_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint32Type TypeUtils.uint32Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U8_Un] <- zipWithOneOffset <| fun _ _ -> this.ConvOvfUn TypeUtils.uint64Type TypeUtils.uint64Type
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I]     <- Options.HandleNativeInt opcode2Function.[hashFunction OpCodes.Conv_Ovf_I4]    opcode2Function.[hashFunction OpCodes.Conv_Ovf_I8]
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_I_Un]  <- Options.HandleNativeInt opcode2Function.[hashFunction OpCodes.Conv_Ovf_I4_Un] opcode2Function.[hashFunction OpCodes.Conv_Ovf_I8_Un]
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U]     <- Options.HandleNativeInt opcode2Function.[hashFunction OpCodes.Conv_Ovf_U4]    opcode2Function.[hashFunction OpCodes.Conv_Ovf_U8]
        opcode2Function.[hashFunction OpCodes.Conv_Ovf_U_Un]  <- Options.HandleNativeInt opcode2Function.[hashFunction OpCodes.Conv_Ovf_U4_Un] opcode2Function.[hashFunction OpCodes.Conv_Ovf_U8_Un]
        opcode2Function.[hashFunction OpCodes.Castclass]      <- zipWithOneOffset <| this.CastClass
        opcode2Function.[hashFunction OpCodes.Ldlen]          <- zipWithOneOffset <| fun _ _ -> this.LdLen
        opcode2Function.[hashFunction OpCodes.Ldvirtftn]      <- zipWithOneOffset <| this.LdVirtFtn
        opcode2Function.[hashFunction OpCodes.Box]            <- zipWithOneOffset <| this.Box
        opcode2Function.[hashFunction OpCodes.Unbox]          <- zipWithOneOffset <| this.Unbox
        opcode2Function.[hashFunction OpCodes.Unbox_Any]      <- zipWithOneOffset <| this.UnboxAny
        opcode2Function.[hashFunction OpCodes.Add_Ovf_Un]     <- zipWithOneOffset <| fun _ _ -> this.Add_ovf_un
        opcode2Function.[hashFunction OpCodes.Sub_Ovf_Un]     <- zipWithOneOffset <| fun _ _ -> this.Sub_ovf_un
        opcode2Function.[hashFunction OpCodes.Mul_Ovf_Un]     <- zipWithOneOffset <| fun _ _ -> this.Mul_ovf_un
        opcode2Function.[hashFunction OpCodes.Add_Ovf]        <- zipWithOneOffset <| fun _ _ -> this.Add_ovf
        opcode2Function.[hashFunction OpCodes.Sub_Ovf]        <- zipWithOneOffset <| fun _ _ -> this.Sub_ovf
        opcode2Function.[hashFunction OpCodes.Mul_Ovf]        <- zipWithOneOffset <| fun _ _ -> this.Mul_ovf
        opcode2Function.[hashFunction OpCodes.Div]            <- zipWithOneOffset <| fun _ _ -> this.Div
        opcode2Function.[hashFunction OpCodes.Div_Un]         <- zipWithOneOffset <| fun _ _ -> this.DivUn
        opcode2Function.[hashFunction OpCodes.Rem]            <- zipWithOneOffset <| fun _ _ -> this.Rem
        opcode2Function.[hashFunction OpCodes.Rem_Un]         <- zipWithOneOffset <| fun _ _ -> this.RemUn
        opcode2Function.[hashFunction OpCodes.Newarr]         <- zipWithOneOffset <| this.Newarr

    let internalImplementations : Map<string, (cilState -> term option -> term list -> cilState list)> =
        Map.ofList [
            "System.Int32 System.Array.GetLength(this, System.Int32)", this.CommonGetArrayLength
            "System.Int32 System.Array.GetLowerBound(this, System.Int32)", this.GetArrayLowerBound
            "System.Void System.Runtime.CompilerServices.RuntimeHelpers.InitializeArray(System.Array, System.RuntimeFieldHandle)", this.CommonInitializeArray
        ]
    let __corruptedStack__() = raise (System.InvalidProgramException())

    member private x.Raise createException (cilState : cilState) k =
        // TODO: exception handling
        let statesWithCreatedExceptions : cilState list = createException cilState
        k statesWithCreatedExceptions

    member private x.AccessArray accessor (cilState : cilState) upperBound index k =
        let checkArrayBounds upperBound x =
            let lowerBound = Concrete 0 Types.TLength
            let notTooSmall = Arithmetics.(>>=) x lowerBound
            let notTooLarge = Arithmetics.(<<) x upperBound
            notTooSmall &&& notTooLarge
        StatedConditionalExecutionAppendResultsCIL cilState
            (fun state k -> k (checkArrayBounds upperBound index, state))
            accessor
            (x.Raise x.IndexOutOfRangeException)
            k

    member private x.AccessArrayDimension accessor (cilState : cilState) (this : term) (dimension : term) =
        let upperBound = Memory.ArrayRank cilState.state this
        x.AccessArray (accessor this dimension) cilState upperBound dimension id
    member private x.CommonGetArrayLength (cilState : cilState) thisOption args =
        match args with
        | dimensionsKey :: [] ->
            let arrayLengthByDimension arrayRef index cilState (k : cilState list -> 'a) =
                cilState |> withResult (Memory.ArrayLengthByDimension cilState.state arrayRef index) |> List.singleton |> k
            x.AccessArrayDimension arrayLengthByDimension cilState (Option.get thisOption) dimensionsKey
        | _ -> internalfail "unexpected number of arguments"

    member private x.GetArrayLowerBound (cilState : cilState) (this : term option) args =
        match args with
        | dimension :: [] ->
            let arrayLowerBoundByDimension arrayRef index (cilState : cilState) k =
                cilState |> withResult (Memory.ArrayLowerBoundByDimension cilState.state arrayRef index) |> List.singleton |> k
            x.AccessArrayDimension arrayLowerBoundByDimension cilState (Option.get this) dimension
        | _ -> internalfail "unexpected number of arguments"

    member private x.NpeOrInvokeStatementCIL (cilState : cilState) (this : term) statement (k : cilState list -> 'a) =
         StatedConditionalExecutionCIL cilState
            (fun state k -> k (IsNullReference this, state))
            (x.Raise x.NullReferenceException)
            statement
            k

    member private x.CommonInitializeArray (cilState : cilState) _ (args : term list) =
        match args with
        | arrayRef :: handleTerm :: [] ->
            x.NpeOrInvokeStatementCIL cilState arrayRef (fun cilState k ->
            x.NpeOrInvokeStatementCIL cilState handleTerm (fun cilState k ->
            let results : state list = VSharp.System.Runtime_CompilerServices_RuntimeHelpers.InitializeArray cilState.state arrayRef handleTerm
            let cilResults = List.map (fun state -> withState state cilState) results
            k cilResults) k) id
        | _ -> internalfail "unexpected number of arguments"
    member private x.ReduceMethodBaseCall (methodBase : MethodBase) (cilState : cilState) (k : cilState list -> 'a) =
        let state = cilState.state
        let k = List.map popStackOf >> k
        let thisOption = if methodBase.IsStatic then None else Some <| Memory.ReadThis state methodBase
        let args = methodBase.GetParameters() |> Seq.map (Memory.ReadArgument state) |> List.ofSeq
        let fullMethodName = Reflection.GetFullMethodName methodBase
        let (&&&) = Microsoft.FSharp.Core.Operators.(&&&)
        if Map.containsKey fullMethodName internalImplementations then
            (internalImplementations.[fullMethodName] cilState thisOption args) |> k
        elif Map.containsKey fullMethodName Loader.internalImplementations then
            let thisAndArguments : term list =
                match thisOption with
                | None -> args
                | Some this -> this :: args
            internalCall Loader.internalImplementations.[fullMethodName] thisAndArguments state (List.map (changeState cilState) >> k)
        elif Map.containsKey fullMethodName Loader.concreteExternalImplementations then
            // TODO: check that all parameters were specified
            let methodInfo = Loader.concreteExternalImplementations.[fullMethodName]
            let thisOption, args =
                match thisOption, methodInfo.IsStatic with
                | Some this, true -> None, this :: args
                | None, false -> internalfail "Calling non-static concrete implementation for static method"
                | _ -> thisOption, args
            let state = methodInterpreter.ReduceFunctionSignature state methodInfo thisOption (Specified args) false id
            methodInterpreter.ReduceFunction {cilState with state = state} methodInfo (List.map popStackOf >> k)
        elif int (methodBase.GetMethodImplementationFlags() &&& MethodImplAttributes.InternalCall) <> 0 then
            internalfailf "new extern method: %s" fullMethodName
        elif methodBase.GetMethodBody() <> null then
            methodInterpreter.ReduceFunction cilState methodBase k
        else
            internalfail "nonextern method without body!"

    member x.CallMethodFromTermType (cilState : cilState) (*this parameters *) termType (calledMethod : MethodInfo) (k : cilState list -> 'a) =
        let t = termType |> Types.ToDotNetType
        let genericCalledMethod = if calledMethod.IsGenericMethod then calledMethod.GetGenericMethodDefinition() else calledMethod
        let genericMethodInfo =
            match genericCalledMethod.DeclaringType with
            | t1 when t1.IsInterface ->
                let createSignature (m : MethodInfo) =
                    m.GetParameters() |> Seq.map (fun p -> (p.ParameterType |> safeGenericTypeDefinition).FullName)
                    |> join ","
                let onlyLastName (m : MethodInfo) =
                    match m.Name.LastIndexOf('.') with
                    | i when i < 0 -> m.Name
                    | i -> m.Name.Substring(i + 1)
                let sign = createSignature genericCalledMethod
                let lastName = onlyLastName genericCalledMethod
                let methods =
                    match t with
                    | _ when t.IsArray -> t.GetMethods()
                    | _ -> t.GetInterfaceMap(t1).TargetMethods
                methods |> Seq.find (fun mi -> createSignature mi = sign && onlyLastName mi = lastName)
            | _ ->
                let (|||) = Microsoft.FSharp.Core.Operators.(|||)
                let allMethods = t.GetMethods(BindingFlags.Instance ||| BindingFlags.Public ||| BindingFlags.NonPublic)
                Seq.find (fun (mi : MethodInfo) -> mi.GetBaseDefinition() = genericCalledMethod.GetBaseDefinition()) allMethods
        let targetMethod = if genericMethodInfo.IsGenericMethod then genericMethodInfo.MakeGenericMethod(calledMethod.GetGenericArguments()) else genericMethodInfo
        if targetMethod.IsAbstract
            then x.CallAbstract (methodInterpreter.MakeMethodIdentifier targetMethod) cilState k
            elif calledMethod = targetMethod then x.ReduceMethodBaseCall targetMethod cilState k // optimization
            else
                // Getting this and arguments values by old keys
                let this = Memory.ReadThis cilState.state calledMethod
                let args = calledMethod.GetParameters() |> Seq.map (Memory.ReadArgument cilState.state) |> List.ofSeq
                // Popping frame created for ancestor calledMethod
                let cilState = popStackOf cilState
                // Creating valid frame with stackKeys corresponding to actual targetMethod
                methodInterpreter.ReduceFunctionSignature cilState.state targetMethod (Some this) (Specified args) false (fun rightState ->
                x.ReduceMethodBaseCall targetMethod {cilState with state = rightState} k)

    member x.CallVirtualMethod (ancestorMethod : MethodInfo) (cilState : cilState) (k : cilState list -> 'a) =
        let methodId = methodInterpreter.MakeMethodIdentifier ancestorMethod
        let this = Memory.ReadThis cilState.state ancestorMethod
        let callVirtual (cilState : cilState) this k =
            let baseType = MostConcreteTypeOfHeapRef cilState.state this
            let callForConcreteType typ state k =
                x.CallMethodFromTermType state typ ancestorMethod k
            let tryToCallForBaseType (cilState : cilState) (k : cilState list -> 'a) =
                StatedConditionalExecutionAppendResultsCIL cilState
                    (fun state k -> k (API.Types.TypeIsRef state baseType this, state))
                    (callForConcreteType baseType)
                    (x.CallAbstract methodId)
                    k
            let baseDotNetType = Types.ToDotNetType baseType
            if baseDotNetType.IsInterface
                then x.CallAbstract methodId cilState k
                else tryToCallForBaseType cilState k
        GuardedApplyCIL cilState this callVirtual k

    member x.CallAbstract funcId cilState k =
        methodInterpreter.CallAbstractMethod funcId cilState k

    member private x.ConvOvf targetType (cilState : cilState) =
        let supersetsOf =
            PersistentDict.ofSeq [
                TypeUtils.int8Type,    [|TypeUtils.int8Type; TypeUtils.int16Type; TypeUtils.int32Type; TypeUtils.int64Type|]
                TypeUtils.int16Type,   [|TypeUtils.int16Type; TypeUtils.int32Type; TypeUtils.int64Type|]
                TypeUtils.int32Type,   [|TypeUtils.int32Type; TypeUtils.int64Type|]
                TypeUtils.int64Type,   [|TypeUtils.int64Type|]
                TypeUtils.uint8Type,   [|TypeUtils.uint8Type; TypeUtils.uint16Type; TypeUtils.uint32Type; TypeUtils.uint64Type|]
                TypeUtils.uint16Type,  [|TypeUtils.uint16Type; TypeUtils.uint32Type; TypeUtils.uint64Type|]
                TypeUtils.uint32Type,  [|TypeUtils.uint32Type; TypeUtils.uint64Type|]
                TypeUtils.uint64Type,  [|TypeUtils.uint64Type|]
                TypeUtils.float32Type, [|TypeUtils.float32Type; TypeUtils.float64Type|]
                TypeUtils.float64Type, [|TypeUtils.float64Type|] ]
        let isSubset leftTyp rightTyp = Array.contains rightTyp supersetsOf.[leftTyp]
        let minMaxOf =
            PersistentDict.ofSeq [
                TypeUtils.int8Type,    (System.SByte.MinValue |> int64, System.SByte.MaxValue |> int64)
                TypeUtils.int16Type,   (System.Int16.MinValue |> int64, System.Int16.MaxValue |> int64)
                TypeUtils.int32Type,   (System.Int32.MinValue |> int64, System.Int32.MaxValue |> int64)
                TypeUtils.int64Type,   (System.Int64.MinValue, System.Int64.MaxValue)
                TypeUtils.uint8Type,   (System.Byte.MinValue |> int64, System.Byte.MaxValue |> int64)
                TypeUtils.uint16Type,  (System.UInt16.MinValue |> int64, System.UInt16.MaxValue |> int64)
                TypeUtils.uint32Type,  (System.UInt32.MinValue |> int64, System.UInt32.MaxValue |> int64)
                TypeUtils.uint64Type,  (System.UInt64.MinValue |> int64, System.UInt64.MaxValue |> int64)
                TypeUtils.float32Type, (System.Single.MinValue |> int64, System.Single.MaxValue |> int64)
                TypeUtils.float64Type, (System.Double.MinValue |> int64, System.Double.MaxValue |> int64) ]
        let getSegment leftTyp rightTyp =
            let min1, max1 = minMaxOf.[leftTyp]
            let min2, max2 = minMaxOf.[rightTyp]
            match min1 < min2, max1 < max2 with
            | true, true   -> min2, max1
            | true, false  -> min2, max2
            | false, true  -> min1, max1
            | false, false -> min1, max2
        let canCastWithoutOverflow term targetTermType =
            let (<<=) = API.Arithmetics.(<<=)
            let termType = Terms.TypeOf Memory.EmptyState term
            assert(termType |> Types.IsNumeric)
            if isSubset termType targetTermType then True
            elif termType = TypeUtils.int64Type && targetTermType = TypeUtils.uint64Type then
                let int64Zero = MakeNumber (0 |> int64)
                int64Zero <<= term
            elif termType = TypeUtils.uint64Type && targetTermType = TypeUtils.int64Type then
                let uint64RightBorder = MakeNumber (System.Int64.MaxValue |> uint64)
                term <<= uint64RightBorder
            else
                let min, max = getSegment termType targetTermType
                let leftBorder  = Concrete min termType // must save type info, because min is int64
                let rightBorder = Concrete max termType // must save type info, because max is int64
                (leftBorder <<= term) &&& (term <<= rightBorder)
        let t, cilState = pop cilState
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (canCastWithoutOverflow t targetType, state))
            (fun cilState k ->
                let castedResult = Types.Cast t targetType
                push castedResult cilState |> List.singleton |> k)
            (x.Raise x.OverflowException)
            id

    member private x.ConvOvfUn unsignedSightType targetType (cilState : cilState) =
        let t, cilState = pop cilState
        let unsignedT = castUnchecked unsignedSightType t cilState.state
        x.ConvOvf targetType (push unsignedT cilState)

    member private x.CommonCastClass (cilState : cilState) (term : term) (typ : symbolicType) k =
        let term = castReferenceToPointerIfNeeded term typ cilState.state
        StatedConditionalExecutionAppendResultsCIL cilState
            (fun state k -> k (IsNullReference term ||| Types.IsCast state term typ, state))
            (fun cilState k -> cilState |> withResult (Types.Cast term typ) |> List.singleton |> k)
            (x.Raise x.InvalidCastException)
            k
    member private x.CastClass (cfg : cfg) offset (cilState : cilState) : cilState list =
        let term, cilState = pop cilState
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Castclass.Size)
        x.CommonCastClass cilState term typ pushResultToOperationalStack

    member x.CommonCall (calledMethodBase : MethodBase) (cilState : cilState) (k : cilState list -> 'a) =
        let call cilState k =
            methodInterpreter.InitializeStatics cilState calledMethodBase.DeclaringType (List.map (fun cilState ->
            x.ReduceMethodBaseCall calledMethodBase cilState id) >> List.concat >> k)
        match calledMethodBase.IsStatic with
        | true -> call cilState k
        | false ->
            let this = Memory.ReadThis cilState.state calledMethodBase
            x.NpeOrInvokeStatementCIL cilState this call k
    member x.Call (cfg : cfg) offset (cilState : cilState) =
        let calledMethodBase = resolveMethodFromMetadata cfg (offset + OpCodes.Call.Size)
        let args, cilState = retrieveActualParameters calledMethodBase cilState
        let this, cilState = if not calledMethodBase.IsStatic then pop cilState |> mapfst Some else None, cilState
        methodInterpreter.ReduceFunctionSignature cilState.state calledMethodBase this (Specified args) false (fun state ->
        x.CommonCall calledMethodBase (withState state cilState) pushResultToOperationalStack)
     member x.CommonCallVirt (ancestorMethodBase : MethodBase) (cilState : cilState) (k : cilState list -> 'a) =
        let this = Memory.ReadThis cilState.state ancestorMethodBase
        let call (cilState : cilState) k =
            methodInterpreter.InitializeStatics cilState ancestorMethodBase.DeclaringType (List.map (fun cilState ->
            if ancestorMethodBase.DeclaringType.IsSubclassOf typedefof<System.Delegate> && ancestorMethodBase.Name = "Invoke" then
                Lambdas.invokeDelegate cilState this id
            elif ancestorMethodBase.IsVirtual && not ancestorMethodBase.IsFinal then
                let methodInfo = ancestorMethodBase :?> MethodInfo
                x.CallVirtualMethod methodInfo cilState id
            else
                x.ReduceMethodBaseCall ancestorMethodBase cilState id) >> List.concat >> k)
        x.NpeOrInvokeStatementCIL cilState this call k
    member x.CallVirt (cfg : cfg) offset (cilState : cilState) =
        let ancestorMethodBase = resolveMethodFromMetadata cfg (offset + OpCodes.Callvirt.Size)
        let args, cilState = retrieveActualParameters ancestorMethodBase cilState
        let this, cilState = pop cilState
        // NOTE: It is not quite strict to ReduceFunctionSignature here because, but it does not matter because signatures of virtual methods are the same
        methodInterpreter.ReduceFunctionSignature cilState.state ancestorMethodBase (Some this) (Specified args) false (fun state ->
        x.CommonCallVirt ancestorMethodBase (withState state cilState) pushResultToOperationalStack)
    member x.ReduceArrayCreation (arrayType : System.Type) (cilState : cilState) (parameters : term list) k =
        let arrayTyp = Types.FromDotNetType arrayType
        let reference, state = Memory.AllocateDefaultArray cilState.state parameters arrayTyp
        withResultState reference state |> changeState cilState |> List.singleton |> k
    member x.CommonCreateDelegate (ctor : ConstructorInfo) (cilState : cilState) (args : term list) (k : cilState list -> 'a) =
        let target, methodPtr =
            assert(List.length args = 2)
            args.[0], args.[1]
        let retrieveMethodInfo methodPtr =
            match methodPtr.term with
            | Concrete(:? MethodInfo as mi, _) -> mi
            | _ -> __unreachable__()
        let invoke cilState =
            GuardedApplyCIL cilState methodPtr
                (fun cilState methodPtr k ->
                    BranchOnNullCIL cilState target
                        (x.Raise x.NullReferenceException)
                        (x.ReduceMethodBaseCall (retrieveMethodInfo methodPtr))
                        k)
        let typ = Types.FromDotNetType ctor.DeclaringType
        Lambdas.make invoke typ (fun lambda ->
        let deleg, state = Memory.AllocateDelegate cilState.state lambda
        let state = withResultState deleg state
        withState state cilState |> List.singleton |> k)
    member x.CommonNewObj isCallNeeded (constructorInfo : ConstructorInfo) (cilState : cilState) (args : term list) (k : cilState list -> 'a) : 'a =
        let typ = constructorInfo.DeclaringType
        let constructedTermType = Types.FromDotNetType typ
        let blockCase (cilState : cilState) =
            let callConstructor (cilState : cilState) reference afterCall =
                if isCallNeeded then
                    methodInterpreter.ReduceFunctionSignature cilState.state constructorInfo (Some reference) (Specified args) false (fun state ->
                    x.ReduceMethodBaseCall constructorInfo (withState state cilState) afterCall)
                else withResultState reference cilState.state |> changeState cilState |> List.singleton
            let referenceTypeCase (cilState : cilState) =
                let ref, state = Memory.AllocateDefaultClass cilState.state constructedTermType
                callConstructor (withState state cilState) ref (List.map (withResult ref))
            let valueTypeCase (cilState : cilState) =
                let freshValue = Memory.DefaultOf constructedTermType
                let ref, state = Memory.AllocateTemporaryLocalVariable cilState.state typ freshValue
                let modifyResult (cilState : cilState) =
                    let value = Memory.ReadSafe cilState.state ref
                    withResult value cilState
                callConstructor (withState state cilState) ref (List.map modifyResult)
            if Types.IsValueType constructedTermType then valueTypeCase cilState
            else referenceTypeCase cilState
        let nonDelegateCase (cilState : cilState) =
            methodInterpreter.InitializeStatics cilState typ (List.map (fun cilState ->
            if typ.IsArray && constructorInfo.GetMethodBody() = null
                then x.ReduceArrayCreation typ cilState args id
                else blockCase cilState) >> List.concat)
        if Reflection.IsDelegateConstructor constructorInfo
            then x.CommonCreateDelegate constructorInfo cilState args k
            else nonDelegateCase cilState |> k

    member x.NewObj (cfg : cfg) offset (cilState : cilState) : cilState list =
        let constructorInfo = resolveMethodFromMetadata cfg (offset + OpCodes.Newobj.Size) :?> ConstructorInfo
        assert (constructorInfo.IsConstructor)
        let args, cilState = retrieveActualParameters constructorInfo cilState
        x.CommonNewObj true constructorInfo cilState args pushResultToOperationalStack

    member x.LdsFld addressNeeded (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Ldsfld.Size)
        assert (fieldInfo.IsStatic)
        methodInterpreter.InitializeStatics cilState fieldInfo.DeclaringType (List.map (fun cilState ->
        let declaringTermType = Types.FromDotNetType fieldInfo.DeclaringType
        let fieldId = Reflection.wrapField fieldInfo
        let value =
            if addressNeeded then StaticField(declaringTermType, fieldId) |> Ref
            else Memory.ReadStaticField cilState.state declaringTermType fieldId
        push value cilState :: []) >> List.concat)
    member private x.StsFld (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Stsfld.Size)
        let state = cilState.state
        assert (fieldInfo.IsStatic)
        let declaringTermType = Types.FromDotNetType fieldInfo.DeclaringType
        let fieldId = Reflection.wrapField fieldInfo
        let value, cilState = pop cilState
        methodInterpreter.InitializeStatics cilState fieldInfo.DeclaringType (List.map (fun cilState ->
        let fieldType = Types.FromDotNetType fieldInfo.FieldType
        let value = castUnchecked fieldType value cilState.state
        let state = Memory.WriteStaticField cilState.state declaringTermType fieldId value
        cilState |> withState state))
    member x.LdFld addressNeeded (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Ldfld.Size)
        assert (not fieldInfo.IsStatic)
        let target, cilState = pop cilState
        let loadWhenTargetIsNotNull (cilState : cilState) k =
            let createCilState value = push value cilState |> List.singleton |> k
            let fieldId = Reflection.wrapField fieldInfo
            if addressNeeded then Memory.ReferenceField cilState.state target fieldId |> createCilState
            else Memory.ReadField cilState.state target fieldId |> createCilState
        x.NpeOrInvokeStatementCIL cilState target loadWhenTargetIsNotNull id
    member x.StFld (cfg : cfg) offset (cilState : cilState) =
        let fieldInfo = resolveFieldFromMetadata cfg (offset + OpCodes.Stfld.Size)
        assert (not fieldInfo.IsStatic)
        let value, targetRef, cilState = pop2 cilState
        let storeWhenTargetIsNotNull (cilState : cilState) k =
            let fieldType = Types.FromDotNetType fieldInfo.FieldType
            let fieldId = Reflection.wrapField fieldInfo
            let reference = Memory.ReferenceField cilState.state targetRef fieldId
            let value = castUnchecked fieldType value cilState.state
            Memory.WriteSafe cilState.state reference value |> List.map (changeState cilState) |> k
        x.NpeOrInvokeStatementCIL cilState targetRef storeWhenTargetIsNotNull id
    member private x.LdElemWithCast cast (cilState : cilState) : cilState list =
        let index, arrayRef, cilState = pop2 cilState
        let uncheckedLdElem (cilState : cilState) k =
            let value = Memory.ReadArrayIndex cilState.state arrayRef [index]
            let castedValue = cast value cilState.state
            push castedValue cilState |> List.singleton |> k
        let checkedLdElem (cilState : cilState) k =
            let length = Memory.ArrayLengthByDimension cilState.state arrayRef (MakeNumber 0)
            x.AccessArray uncheckedLdElem cilState length index k
        x.NpeOrInvokeStatementCIL cilState arrayRef checkedLdElem id
    member private x.LdElemTyp typ (cilState : cilState) = x.LdElemWithCast (castUnchecked typ) cilState
    member private x.LdElem (cfg : cfg) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Ldelem.Size)
        x.LdElemTyp typ cilState
    member private x.LdElemRef = x.LdElemWithCast always
    member private x.StElemWithCast cast (cilState : cilState) =
        let value, index, arrayRef, cilState = pop3 cilState
        let checkedStElem (cilState : cilState) (k : cilState list -> 'a) =
            let typeOfValue = TypeOf cilState.state value
            let uncheckedStElem (cilState : cilState) (k : cilState list -> 'a) =
                let typedValue = cast value cilState.state
                Memory.WriteArrayIndex cilState.state arrayRef [index] typedValue |> List.map (changeState cilState) |> k
            let checkTypeMismatchBasedOnTypeOfValue cond (cilState : cilState) =
                StatedConditionalExecutionAppendResultsCIL cilState
                    (fun state k -> k (cond, state))
                    uncheckedStElem
                    (x.Raise x.ArrayTypeMismatchException)
            let rec checkTypeMismatch (cilState : cilState) (k : cilState list -> 'a) =
                let baseType = MostConcreteTypeOfHeapRef cilState.state arrayRef |> Types.ElementType
                if Types.IsValueType typeOfValue then
                    checkTypeMismatchBasedOnTypeOfValue (Types.TypeIsType typeOfValue baseType) cilState k
                else
                    checkTypeMismatchBasedOnTypeOfValue (Types.RefIsAssignableToType cilState.state value baseType) cilState k
            let length = Memory.ArrayLengthByDimension cilState.state arrayRef (MakeNumber 0)
            x.AccessArray checkTypeMismatch cilState length index k
        x.NpeOrInvokeStatementCIL cilState arrayRef checkedStElem id
    member private x.StElemTyp typ (cilState : cilState) =
        x.StElemWithCast (castUnchecked typ) cilState
    member private x.StElem (cfg : cfg) offset (cilState : cilState) =
        let typ = resolveTermTypeFromMetadata cfg (offset + OpCodes.Stelem.Size)
        x.StElemTyp typ cilState
    member private x.StElemRef = x.StElemWithCast always
    member private x.LdLen (cilState : cilState) =
        let arrayRef, cilState = pop cilState
        let ldlen (cilState : cilState) k =
            let length = Memory.ArrayLengthByDimension cilState.state arrayRef (MakeNumber 0)
            push length cilState |> List.singleton |> k
        x.NpeOrInvokeStatementCIL cilState arrayRef ldlen id
    member private x.LdVirtFtn (_ : cfg) _ (_ : cilState) =
        __notImplemented__()
//        let ancestorMethodBase = resolveMethodFromMetadata cfg (offset + OpCodes.Ldvirtftn.Size)
//        match cilState.opStack with
//        | this :: stack ->
//            let ldvirtftn (cilState : cilState) k =
//                assert(isReference this)
//                let t = this |> SightTypeOfRef |> Types.ToDotNetType
//                let methodInfo = t.GetMethod(ancestorMethodBase.Name, allBindingFlags)
//                let methodPtr = Terms.Concrete methodInfo (Types.FromDotNetType cilState.state (methodInfo.GetType()))
//                k [methodPtr, cilState]
//            x.NpeOrInvokeStatement {cilState with opStack = stack} this ldvirtftn pushFunctionResults
//        | _ -> __corruptedStack__()
    member x.BoxNullable (t : System.Type) v (cilState : cilState) : cilState list =
        // TODO: move it to Reflection.fs; add more validation in case if .NET implementation does not have these methods
        let boxValue (cilState : cilState) =
            match cilState.state.returnRegister with
            | None -> __unreachable__()
            | Some value ->
                let address, state = Memory.BoxValueType cilState.state value
                cilState |> withState state |> withResult address
        let hasValueMethodInfo = t.GetMethod("get_HasValue")
        let hasValueCase (cilState : cilState) k =
            let valueMethodInfo = t.GetMethod("get_Value")
            methodInterpreter.ReduceFunctionSignature cilState.state valueMethodInfo (Some v) (Specified []) false (fun state ->
            x.ReduceMethodBaseCall valueMethodInfo (withState state cilState) ((List.map boxValue) >> k))
        let boxNullable (hasValue, cilState : cilState) k =
            StatedConditionalExecutionAppendResultsCIL cilState
                (fun state k -> k (hasValue, state))
                hasValueCase
                (fun cilState k -> cilState |> withResult NullRef |> List.singleton |> k)
                k
        methodInterpreter.ReduceFunctionSignature cilState.state hasValueMethodInfo (Some v) (Specified []) false (fun state ->
        x.ReduceMethodBaseCall hasValueMethodInfo (withState state cilState) (fun hasValueResults ->
        let hasValueResults = hasValueResults |> List.map (fun cilState -> Option.get cilState.state.returnRegister, cilState)
        Cps.List.mapk boxNullable hasValueResults (List.concat >> pushResultToOperationalStack)))

    member x.Box (cfg : cfg) offset (initialCilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Box.Size)
        let termType = Types.FromDotNetType t
        let v, cilState = pop initialCilState
        if Types.IsValueType termType then
            if Types.TypeIsNullable termType then x.BoxNullable t v cilState
            else allocateValueTypeInHeap v cilState
        else [initialCilState]
    member private x.UnboxCommon (cilState : cilState) (obj : term) (t : System.Type) (handleRestResults : term * state -> term * state) (k : cilState list -> 'a) =
        let nonExceptionCont (cilState : cilState) res state k =
            cilState |> withState state |> withResult res |> List.singleton |> k
        let termType = Types.FromDotNetType t
        assert(IsReference obj)
        assert(Types.IsValueType termType)
        let nullCase (cilState : cilState) (k : cilState list -> 'a) : 'a = // TODO: mb too complex #do
            if Types.TypeIsNullable termType then
                let nullableTerm = Memory.DefaultOf termType
                let address, state = Memory.BoxValueType cilState.state nullableTerm
                let res, state = handleRestResults (HeapReferenceToBoxReference address, state)
                nonExceptionCont cilState res state k
            else
                x.Raise x.NullReferenceException cilState k
        let nullableCase (cilState : cilState) =
            let underlyingTypeOfNullableT = System.Nullable.GetUnderlyingType t
            StatedConditionalExecutionAppendResultsCIL cilState
                (fun state k -> k (Types.RefIsType state obj (Types.FromDotNetType underlyingTypeOfNullableT), state))
                (fun cilState k ->
                    let value = HeapReferenceToBoxReference obj |> Memory.ReadSafe cilState.state
                    let nullableTerm = Memory.DefaultOf termType
                    let valueField, hasValueField = Reflection.fieldsOfNullable t
                    let nullableTerm = Memory.WriteStructField nullableTerm valueField value
                    let nullableTerm = Memory.WriteStructField nullableTerm hasValueField (MakeBool true)
                    let address, state = Memory.BoxValueType cilState.state nullableTerm
                    let res, state = handleRestResults (HeapReferenceToBoxReference address, state)
                    nonExceptionCont cilState res state k)
                (x.Raise x.InvalidCastException)
        let nonNullCase (cilState : cilState) =
            if Types.TypeIsNullable termType then
                nullableCase cilState
            else
                StatedConditionalExecutionAppendResultsCIL cilState
                    (fun state k -> k (Types.IsCast state obj termType, state))
                    (fun cilState k ->
                        let res, state = handleRestResults (Types.Cast obj termType |> HeapReferenceToBoxReference, cilState.state)
                        cilState |> withState state |> withResult res |> List.singleton |> k)
                    (x.Raise x.InvalidCastException)
        BranchOnNullCIL cilState obj
            nullCase
            nonNullCase
            k

    member private x.Unbox (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Unbox.Size)
        let obj, cilState = pop cilState
        // TODO: Nullable.GetUnderlyingType for generics; use meta-information of generic type parameter
        if t.IsGenericParameter then __insufficientInformation__ "Unboxing generic parameter"
        x.UnboxCommon cilState obj t id pushResultToOperationalStack
    member private x.UnboxAny (cfg : cfg) offset (cilState : cilState) =
        let t = resolveTypeFromMetadata cfg (offset + OpCodes.Unbox_Any.Size)
        let termType = Types.FromDotNetType t
        let valueType = Types.FromDotNetType typeof<System.ValueType>
        let obj, cilState = pop cilState
        // TODO: Nullable.GetUnderlyingType for generics; use meta-information of generic type parameter
        if t.IsGenericParameter then __insufficientInformation__ "Can't introduce generic type X for equation: T = Nullable<X>"
        StatedConditionalExecutionAppendResultsCIL cilState
            (fun state k -> k (Types.TypeIsType termType valueType, state))
            (fun cilState k ->
                let handleRestResults (address, state : state) = Memory.ReadSafe state address, state
                x.UnboxCommon cilState obj t handleRestResults k)
            (fun state k -> x.CommonCastClass state obj termType k)
            pushResultToOperationalStack

    member private this.CommonDivRem performAction (cilState : cilState) =
        let integerCase (cilState : cilState) x y minusOne minValue =
            assert(TypeOf cilState.state x = TypeOf cilState.state y)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Arithmetics.IsZero y, state))
                (this.Raise this.InvalidCastException)
                (fun cilState ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k -> k ((x === minValue) &&& (y === minusOne), state))
                        (this.Raise this.InvalidCastException)
                        (fun cilState k -> cilState |> push (performAction x y) |> List.singleton |> k))
                id
        let y, x, cilState = pop2 cilState
        match y, x with
        | TypeUtils.Float, TypeUtils.Float -> cilState |> push (performAction x y) |> List.singleton
        | TypeUtils.Int64, _
        | TypeUtils.UInt64, _
        | _, TypeUtils.Int64
        | _, TypeUtils.UInt64 -> integerCase cilState x y TypeUtils.Int64.MinusOne TypeUtils.Int64.MinValue
        | _ -> integerCase cilState x y TypeUtils.Int32.MinusOne TypeUtils.Int32.MinValue
        | _ -> __corruptedStack__()
    member private this.Div (cilState : cilState) =
        let div x y = API.PerformBinaryOperation OperationType.Divide x y id
        this.CommonDivRem div cilState

    member private this.Rem (cilState : cilState) =
        let rem x y = API.PerformBinaryOperation OperationType.Remainder x y id
        this.CommonDivRem rem cilState

    member private this.CommonUnsignedDivRem isRem performAction (cilState : cilState) =
        let y, x, cilState = pop2 cilState
        match y, x with
        | _ when TypeUtils.isInteger x && TypeUtils.isInteger y ->
            let x = makeUnsignedInteger x id
            let y = makeUnsignedInteger y id
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (Arithmetics.IsZero y, state))
                (this.Raise this.DivideByZeroException)
                (fun cilState k -> cilState |> push (performAction x y) |> List.singleton |> k)
                id
        | TypeUtils.Float, _
        | _, TypeUtils.Float when isRem -> internalfailf "Rem.Un is unspecified for Floats"
        | _ -> internalfailf "incompatible operands for %s" (if isRem then "Rem.Un" else "Div.Un")
    member private this.DivUn (cilState : cilState) =
        let div x y = API.PerformBinaryOperation OperationType.Divide x y id
        this.CommonUnsignedDivRem false div cilState

    member private this.RemUn cilState =
        let rem x y = API.PerformBinaryOperation OperationType.Remainder x y id
        this.CommonUnsignedDivRem true rem cilState

    member private this.UnsignedCheckOverflow checkOverflowForUnsigned (cilState : cilState) =
        let y, x, cilState = pop2 cilState
        match y, x with
        | TypeUtils.Int64, _
        | _, TypeUtils.Int64
        | TypeUtils.UInt64, _
        | _, TypeUtils.UInt64 ->
            let x = makeUnsignedInteger x id
            let y = makeUnsignedInteger y id
            let max = TypeUtils.UInt64.MaxValue
            let zero = TypeUtils.UInt64.Zero
            checkOverflowForUnsigned zero max x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ when TypeUtils.isInteger x && TypeUtils.isInteger y ->
            let x, y = makeUnsignedInteger x id, makeUnsignedInteger y id
            let max = TypeUtils.UInt32.MaxValue
            let zero = TypeUtils.UInt32.Zero
            checkOverflowForUnsigned zero max x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | _ -> internalfailf "incompatible operands for UnsignedCheckOverflow"
    member private this.SignedCheckOverflow checkOverflow (cilState : cilState) =
        let y, x, cilState = pop2 cilState
        match y, x with
        | TypeUtils.Int64, _
        | _, TypeUtils.Int64 ->
            let min = TypeUtils.Int64.MinValue
            let max = TypeUtils.Int64.MaxValue
            let zero = TypeUtils.Int64.Zero
            let minusOne = TypeUtils.Int64.MinusOne
            checkOverflow min max zero minusOne x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
        | TypeUtils.UInt64, _
        | _, TypeUtils.UInt64 -> __unreachable__() // instead of add_ovf should be called add_ovf_un
        | TypeUtils.Float, _
        | _, TypeUtils.Float -> __unreachable__() // only integers
        | _ ->
            let min = TypeUtils.Int32.MinValue
            let max = TypeUtils.Int32.MaxValue
            let zero = TypeUtils.Int32.Zero
            let minusOne = TypeUtils.Int32.MinusOne
            checkOverflow min max zero minusOne x y cilState // TODO: maybe rearrange x and y if y is concrete and x is symbolic
    member private this.Add_ovf (cilState : cilState) =
        // min <= x + y <= max
        let checkOverflow min max zero _ x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let xMoreThan0 state k = k (x >>= zero, state)
            let yMoreThan0 state k = k (y >>= zero, state)
            let checkOverflowWhenMoreThan0 (state : state) k = // x >= 0 && y >= 0
                PerformBinaryOperation OperationType.Subtract max y (fun diff ->
                k (diff >>= x, state))
            let checkOverflowWhenLessThan0 (state : state) k =
                PerformBinaryOperation OperationType.Subtract min y (fun diff ->
                k (x >>= diff, state))
            let add (cilState : cilState) k = // no overflow
                PerformBinaryOperation OperationType.Add x y (fun sum -> push sum cilState |> List.singleton |> k)
            StatedConditionalExecutionCIL cilState xMoreThan0
                (fun cilState -> // x >= 0
                    StatedConditionalExecutionCIL cilState yMoreThan0
                        (fun cilState -> // y >= 0
                            StatedConditionalExecutionCIL cilState
                                checkOverflowWhenMoreThan0
                                add
                                (this.Raise this.OverflowException))
                        add)
                (fun cilState -> // x < 0
                    StatedConditionalExecutionCIL cilState yMoreThan0
                        add
                        (fun cilState -> // x < 0 && y < 0
                            StatedConditionalExecutionCIL cilState
                                checkOverflowWhenLessThan0
                                add
                                (this.Raise this.OverflowException)))
                id
        this.SignedCheckOverflow checkOverflow cilState
    member private this.Mul_ovf (cilState : cilState) =
        // min <= x * y <= max
        let checkOverflow min max zero _ x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let (>>) = API.Arithmetics.(>>)
            let isZero state k = k ((x === zero) ||| (y === zero), state)
            let xMoreThan0 state k = k (x >> zero, state)
            let yMoreThan0 state k = k (y >> zero, state)
            let checkOverflowWhenXM0YM0 (state : state) k = // x > 0 && y > 0
                PerformBinaryOperation OperationType.Divide max y (fun quotient ->
                k (quotient >>= x, state))
            let checkOverflowWhenXL0YL0 (state : state) k = // x < 0 && y < 0
                PerformBinaryOperation OperationType.Divide max y (fun quotient ->
                k (x >>= quotient, state))
            let checkOverflowWhenXM0YL0 (state : state) k = // x > 0 && y < 0
                PerformBinaryOperation OperationType.Divide min x (fun quotient ->
                k (y >>= quotient, state))
            let checkOverflowWhenXL0YM0 (state : state) k = // x < 0 && y > 0
                PerformBinaryOperation OperationType.Divide min y (fun quotient ->
                k (x >>= quotient, state))
            let mul (cilState : cilState) k = // no overflow
                PerformBinaryOperation OperationType.Multiply x y (fun res -> push res cilState |> List.singleton |> k)
            StatedConditionalExecutionCIL cilState isZero
                (fun cilState k -> cilState |> push zero |> List.singleton |> k)
                (fun cilState ->
                    StatedConditionalExecutionCIL cilState
                        xMoreThan0
                        (fun cilState -> // x > 0
                            StatedConditionalExecutionCIL cilState yMoreThan0
                                (fun cilState -> // y > 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXM0YM0
                                        mul
                                        (this.Raise this.OverflowException))
                                (fun cilState -> // y < 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXM0YL0
                                        mul
                                        (this.Raise this.OverflowException)))
                        (fun cilState -> // x < 0
                            StatedConditionalExecutionCIL cilState
                                yMoreThan0
                                (fun cilState -> // y > 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXL0YM0
                                        mul
                                        (this.Raise this.OverflowException))
                                (fun cilState k -> // y < 0
                                    StatedConditionalExecutionCIL cilState
                                        checkOverflowWhenXL0YL0
                                        mul
                                        (this.Raise this.OverflowException)
                                        k)))
                id
        this.SignedCheckOverflow checkOverflow cilState
    member private this.Add_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned _ max x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            StatedConditionalExecutionCIL cilState
                (fun state k ->
                    PerformBinaryOperation OperationType.Subtract max x (fun diff ->
                    k (diff >>= y, state)))
                (fun cilState k -> PerformBinaryOperation OperationType.Add x y (fun res ->
                    cilState |> push res |> List.singleton |> k))
                (this.Raise this.OverflowException)
                id
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Mul_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned zero max x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            let isZero state k = k ((x === zero) ||| (y === zero), state)
            StatedConditionalExecutionCIL cilState isZero
                (fun cilState k -> push zero cilState |> List.singleton |> k)
                (fun cilState k ->
                    StatedConditionalExecutionCIL cilState
                        (fun state k ->
                            PerformBinaryOperation OperationType.Divide max x (fun quotient ->
                            k (quotient >>= y, state)))
                        (fun cilState k ->
                            PerformBinaryOperation OperationType.Multiply x y (fun res ->
                                cilState |> push res |> List.singleton |> k))
                        (this.Raise this.OverflowException)
                        k)
                id
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Sub_ovf_un (cilState : cilState) =
        let checkOverflowForUnsigned _ _ x y cilState =
            let (>>=) = API.Arithmetics.(>>=)
            StatedConditionalExecutionCIL cilState
                (fun state k -> k (x >>= y, state))
                (fun (cilState : cilState) k -> // no overflow
                    PerformBinaryOperation OperationType.Subtract x y (fun res ->
                    cilState |> push res |> List.singleton |> k))
                (this.Raise this.OverflowException)
                id
        this.UnsignedCheckOverflow checkOverflowForUnsigned cilState
    member private this.Sub_ovf (cilState : cilState) =
        // there is no way to reduce current operation to [x `Add_Ovf` (-y)]
        // min <= x - y <= max
        let checkOverflowForSigned min max zero minusOne x y cilState =
                let (>>=) = API.Arithmetics.(>>=)
                let xGreaterEqualZero state k = k (x >>= zero, state)
                let sub (cilState : cilState) k = // no overflow
                    PerformBinaryOperation OperationType.Subtract x y (fun res ->
                    cilState |> push res |> List.singleton |> k)
                StatedConditionalExecutionCIL cilState
                    xGreaterEqualZero
                    (fun cilState -> // x >= 0 => max - x >= 0 => no overflow for [-1 * (max - x)]
                        StatedConditionalExecutionCIL cilState
                            (fun state k ->
                                PerformBinaryOperation OperationType.Subtract max x (fun diff ->
                                PerformBinaryOperation OperationType.Multiply diff minusOne (fun minusDiff ->
                                k (y >>= minusDiff, state)))) // y >= -(max - x)
                            sub
                            (this.Raise this.OverflowException))
                    (fun cilState -> // x < 0 => no overflow for [min - x] # x < 0 => [min - x] != min => no overflow for (-1) * [min - x]
                        StatedConditionalExecutionCIL cilState
                           (fun state k ->
                                PerformBinaryOperation OperationType.Subtract min x (fun diff ->
                                PerformBinaryOperation OperationType.Multiply diff minusOne (fun minusDiff ->
                                k (minusDiff >>= y, state)))) // -(min - x) >= y
                            sub
                            (this.Raise this.OverflowException))
                    id
        this.SignedCheckOverflow checkOverflowForSigned cilState
    member private x.Newarr (cfg : cfg) offset (cilState : cilState) =
        let (>>=) = API.Arithmetics.(>>=)
        let elemType = resolveTermTypeFromMetadata cfg (offset + OpCodes.Newarr.Size)
        let numElements, cilState = pop cilState
        StatedConditionalExecutionCIL cilState
            (fun state k -> k (numElements >>= TypeUtils.Int32.Zero, state))
            (fun cilState k ->
                let ref, state = Memory.AllocateDefaultArray cilState.state [numElements] (ArrayType(elemType, Vector))
                cilState |> withState state |> push ref |> List.singleton |> k)
            (this.Raise this.OverflowException)
            id

    member x.CreateInstance args = methodInterpreter.CreateInstance args

    member x.InvalidProgramException cilState = methodInterpreter.InvalidProgramException cilState
    member x.NullReferenceException cilState = methodInterpreter.NullReferenceException cilState
    member x.IndexOutOfRangeException cilState = methodInterpreter.IndexOutOfRangeException cilState
    member x.ArrayTypeMismatchException cilState = methodInterpreter.ArrayTypeMismatchException cilState
    member x.DivideByZeroException cilState = methodInterpreter.DivideByZeroException cilState
    member x.OverflowException cilState = methodInterpreter.OverflowException cilState
    member x.ArithmeticException cilState = methodInterpreter.ArithmeticException cilState
    member x.TypeInitializerException qualifiedTypeName innerException cilState =
        methodInterpreter.TypeInitializerException qualifiedTypeName innerException cilState
    member x.InvalidCastException cilState = methodInterpreter.InvalidCastException cilState
    // -------------------------------- ExplorerBase operations -------------------------------------

    // returns finishedStates, incompleteStates, erroredStates
    member x.ExecuteAllInstructions (cfg : cfg) (cilState : cilState) : (cilState list * cilState list * cilState list)  =
        assert (cilState.ip.CanBeExpanded())
        let startingOffset = cilState.ip.Offset ()
        let endOffset =
            let lastOffset = Seq.last cfg.sortedOffsets
            let rec binarySearch l r =
                if l + 1 = r then l
                else
                    let mid = (l + r) / 2
                    if cfg.sortedOffsets.[mid] <= startingOffset then binarySearch mid r
                    else binarySearch l mid
            let index = binarySearch 0 (Seq.length cfg.sortedOffsets)
            if cfg.sortedOffsets.[index] = lastOffset then cfg.ilBytes.Length
            else cfg.sortedOffsets.[index + 1]

        let isIpOfCurrentBasicBlock offset = startingOffset <= offset && offset < endOffset

        let rec executeAllInstructions (finishedStates, incompleteStates, errors) (offset : offset) cilState =
            try
                let allStates = x.ExecuteInstruction cfg offset {cilState with iie = None}
                let newErrors, goodStates = allStates |> List.partition (fun (_, cilState : cilState) -> cilState.HasException)
                let errors = errors @ List.map (fun (erroredOffset, (cilState : cilState)) -> {cilState with ip = erroredOffset}) newErrors
                match goodStates with
                | list when List.forall (fst >> (=) ip.Exit) list ->
                    (List.map (fun (_, cilState : cilState) -> {cilState with ip = ip.Exit})) list @ finishedStates, incompleteStates, errors
                | (Instruction nextOffset as nextIp, _)::xs as list when isIpOfCurrentBasicBlock nextOffset && List.forall (fst >> (=) nextIp) xs ->
                    List.fold (fun acc (_, cilState) -> executeAllInstructions acc nextOffset cilState) (finishedStates, incompleteStates, errors) list
                | list -> List.map (fun (ip, cilState) -> {cilState with ip = ip}) list @ finishedStates, incompleteStates, errors
            with
            | :? InsufficientInformationException as iie -> finishedStates, {cilState with iie = Some iie; ip = Instruction offset} :: incompleteStates, errors
        executeAllInstructions ([],[],[]) startingOffset cilState

    member x.IncrementLevelIfNeeded (cfg : cfg) (offset : offset) (cilState : cilState) : cilState =
        let isRecursiveVertex offset =
            if cfg.dfsOut.ContainsKey offset then
                let t1 = cfg.dfsOut.[offset]
                cfg.reverseGraph.[offset] |> Seq.exists (fun w -> cfg.dfsOut.[w] <= t1)
            else false
        if offset = 0 || isRecursiveVertex offset then
            incrementLevel cilState (Instruction offset, cfg.methodBase)
        else cilState

    member x.ExecuteInstruction (cfg : cfg) (offset : int) (cilState : cilState) =
        let opCode = Instruction.parseInstruction cfg.ilBytes offset
        let newOffsets : ip list =
            if Instruction.isLeaveOpCode opCode || opCode = OpCodes.Endfinally
            then cfg.graph.[offset] |> Seq.map Instruction |> List.ofSeq
            else
                let nextTargets = Instruction.findNextInstructionOffsetAndEdges opCode cfg.ilBytes offset
                match nextTargets with
                | UnconditionalBranch nextInstruction
                | FallThrough nextInstruction -> [Instruction nextInstruction]
                | Return -> [Exit]
                | ExceptionMechanism -> [FindingHandler offset]
                | ConditionalBranch targets -> targets |> List.map Instruction
        let newSts = opcode2Function.[hashFunction opCode] cfg offset newOffsets cilState

        newSts |> List.map (fun (d, cilState : cilState) -> d, x.IncrementLevelIfNeeded cfg offset cilState)
