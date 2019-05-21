namespace VSharp.Core

open VSharp
open VSharp.Core.Types.Constructor

module internal TypeCasting =

    let rec primitiveCast mtd isChecked hierarchyCast targetType state term k =
        match term.term with
        | Nop -> internalfailf "casting void to %O!" targetType
        | Concrete(value, _) ->
            if Terms.isFunction term && Types.isFunction targetType
            then k (Concrete term.metadata value targetType, state)
            else k (CastConcrete value (Types.toDotNetType targetType) term.metadata, state)
        | Constant(_, _, t)
        | Expression(_, _, t) -> k (makeCast t targetType term isChecked mtd, state)
        | Ref(NullAddress, path) ->
            assert(List.isEmpty path)
            k (Terms.makeNullRef mtd, state)
        | Ref(TopLevelHeap _, _)
        | Ptr(TopLevelHeap _, _, _, _) ->
            Common.statedConditionalExecution state
                (fun state k -> k <| (Pointers.isNull mtd term, state))
                (fun state k -> k (Terms.makeNullRef mtd, state))
                (fun state k -> hierarchyCast targetType state term k)
                Merging.merge Merging.merge2Terms id k
        | Ref _
        | Ptr _
        | Struct _ -> hierarchyCast targetType state term k
        | _ -> __notImplemented__()

    let castReferenceToPointerHelper mtd targetType = term >> function
        | Ref(topLevel, path)
        | Ptr(topLevel, path, _, None) -> Ptr mtd topLevel path targetType
        | Ptr(topLevel, path, _, Some indent) -> IndentedPtr mtd topLevel path targetType indent
        | t -> internalfailf "Expected reference or pointer, got %O" t

    let private doCast mtd term targetType isChecked = // TODO: do boxing and unboxing using cast from value type to object (valueType.Equals -> ((object) valueType).Equals)
        let castPointer term typ = // For Pointers
            match targetType with
            | Pointer typ' -> castReferenceToPointerHelper mtd typ' term
            | _ -> makeCast (termType.Pointer typ) targetType term isChecked mtd

        match term.term with
        | Ptr(_, _, typ, _) -> castPointer term typ
        | Ref(TopLevelHeap(addr, baseType, _), []) -> HeapRef term.metadata addr baseType targetType []
        | _ -> __unreachable__()

    let rec canCast mtd state targetType term =
        let derefForCast = Memory.derefWith (fun m s _ -> Concrete m null Null, s)
        let castCheck state term =
            match term.term with
            | Ptr(_, _, typ, _) -> Common.is mtd (termType.Pointer typ) targetType, state
            | Ref _ -> // TODO: just take base type from reference
                let contents, state = derefForCast mtd state term
                canCast mtd state targetType contents
            | _ -> Common.is mtd (typeOf term) targetType, state
        Merging.guardedErroredStatedApply castCheck state term

    let cast mtd state argument targetType isChecked primitiveCast fail k =
        let hierarchyCast targetType state term k =
            Common.statedConditionalExecution state
                (fun state k -> k (canCast mtd state targetType term))
                (fun state k -> k (doCast mtd term targetType isChecked |> Return mtd, state))
                (fun state k -> k (fail state term targetType))
                ControlFlow.mergeResults ControlFlow.merge2Results ControlFlow.throwOrIgnore
                (fun (statementResult, state) -> k (ControlFlow.resultToTerm statementResult, state))
        Merging.guardedErroredStatedApplyk (primitiveCast hierarchyCast targetType) state argument k

    let makePointerFromRef mtd state =
        let derefForCast = Memory.derefWith (fun m s _ -> Concrete m null Null, s)
        Merging.guardedErroredApply (fun reference ->
            let term, _ = derefForCast mtd state reference
            castReferenceToPointerHelper mtd (typeOf term) reference)

    let persistentLocalAndConstraintTypes mtd state term defaultLocalType =
        let derefForCast = Memory.derefWith (fun m s _ -> Concrete m null Null, s)
        let p, l =
            match term.term with
            | Ref _
            | Ptr _ ->
                let contents, _ = derefForCast mtd state term
                typeOf contents, typeOf term
            | _ -> typeOf term, defaultLocalType
        p |> Types.unwrapReferenceType, l |> Types.unwrapReferenceType, p |> Types.unwrapReferenceType |> Types.specifyType
