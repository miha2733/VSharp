namespace VSharp.Core

open VSharp
open Memory
open Types
open State
open Strings
open System.Collections.Immutable

module internal Interning =

    let private internCommon read lazyValue mtd =
        let intern state poolKey =
            let time = tick()
            let li = fun () -> lazyValue
            let ptr = {location = poolKey; typ = String; time = time; path = []; isTopLevel = false; arrayTarget = ArrayContents}
            let accessDefined keyMapper valueMapper _ groundHeap r h =
                accessHeap read r mtd groundHeap (makeTrue mtd) makePair h keyMapper valueMapper (instantiationFactory.CustomInstantiator li mtd) ptr
            mapsnd (withPool state) <| accessGeneralizedHeapRec ImmutableHashSet.Empty id id (Some li) read poolOf poolKey accessDefined state.iPool
        Merging.statedMap intern

    let intern mtd state strRef =
        let strStruct, state = deref mtd state strRef
        internCommon false strRef mtd state strStruct

    let isInterned mtd state strRef =
        let strStruct, state = deref mtd state strRef
        internCommon true (makeNullRef String mtd) mtd state strStruct

    let isInternedLiteral mtd time state = internCommon true (makeNullRef String mtd) mtd state << makeConcreteStringStruct mtd time

    let private makeAndInternIfNeed mtd state stringLiteral =
        let makeAndIntern state strStruct =
            let strRef, state = allocateInHeap mtd state strStruct
            intern mtd state strRef
        let strStruct = makeConcreteStringStruct mtd (tick()) stringLiteral
        Common.statedConditionalExecution state
            (fun state k -> k (keyInitialized mtd strStruct state poolOf, state))
            (fun state k -> k (Nop, state))
            (fun state k -> k (makeAndIntern state strStruct))
            Merging.merge Merging.merge2Terms id snd

    let internLiterals mtd = List.fold (makeAndInternIfNeed mtd)
