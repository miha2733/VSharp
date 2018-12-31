namespace VSharp.Core

open VSharp
open VSharp.Core.Types.Constructor

module internal TypeCasting =

    let rec primitiveCast mtd isChecked hierarchyCast targetType state term k =
        match term.term with
        | Error _ -> k (term, state)
        | Nop -> internalfailf "casting void to %O!" targetType
        | Concrete(value, _) ->
            if Terms.isFunction term && Types.isFunction targetType
            then k (Concrete term.metadata value targetType, state)
            else k (CastConcrete value (Types.toDotNetType targetType) term.metadata, state)
        | Constant(_, _, t)
        | Expression(_, _, t) -> k (makeCast t targetType term isChecked mtd, state)
        | Ref(NullAddress, _)
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

    let private doCast mtd term targetType isChecked =
        let changeLast = // For References
            List.changeLast (function
                | StructField(f, _) -> StructField(f, targetType)
                | ArrayIndex(i, _) -> ArrayIndex(i, targetType)
                | s -> s)

        let castPointer term typ = // For Pointers
            match targetType with
            | Pointer typ' -> castReferenceToPointer mtd typ' term
            | _ -> makeCast (termType.Pointer typ) targetType term isChecked mtd

        match term.term with
        | Ptr(_, _, typ, _) -> castPointer term typ
        | Ref(TopLevelHeap(addr, baseType, _), path) -> HeapRef term.metadata addr baseType targetType path
        | Ref(key, path) -> Ref mtd key (changeLast path)
        | _ -> __unreachable__()

    let rec canCast mtd state targetType term =
        let derefForCast = Memory.derefWith (fun m s _ -> Concrete m null Null, s)
        match term.term with
        | Ptr(_, _, typ, _) -> Common.is mtd (termType.Pointer typ) targetType, state
        | Ref _ ->
            let contents, state = derefForCast mtd state term
            canCast mtd state targetType contents
        | Union gvs -> Merging.guardedStateMap (fun state term -> canCast mtd state targetType term) gvs state
        | _ -> Common.is mtd (typeOf term) targetType, state

    let cast mtd state argument targetType isChecked primitiveCast fail k =
        let hierarchyCast targetType state term k =
            Common.statedConditionalExecution state
                (fun state k -> k (canCast mtd state targetType term))
                (fun state k -> k (doCast mtd term targetType isChecked |> Return mtd, state))
                (fun state k -> k (fail state term targetType))
                ControlFlow.mergeResults ControlFlow.merge2Results ControlFlow.throwOrIgnore
                (fun (statementResult, state) -> k (ControlFlow.resultToTerm statementResult, state))
        Merging.statedMapk (primitiveCast hierarchyCast targetType) state argument k

    let castReferenceToPointer mtd state reference k =
        let derefForCast = Memory.derefWith (fun m s _ -> makeNullRef m, s)
        let term, state = derefForCast mtd state reference
        k (castReferenceToPointer mtd (typeOf term) reference, state)

    let persistentLocalAndConstraintTypes mtd state term defaultLocalType =
        let derefForCast = Memory.derefWith (fun m s _ -> Concrete m null Null, s)
        let p, l =
            match term.term with
            | Ref _
            | Ptr _ ->
                let contents, _ = derefForCast mtd state term
                typeOf contents, typeOf term
            | _ -> typeOf term, defaultLocalType
        p |> unwrapReferenceType, l |> unwrapReferenceType, p |> unwrapReferenceType |> Types.specifyType
