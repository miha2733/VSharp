﻿namespace VSharp

open VSharp.State
open Types
open Types.Constructor

module internal Memory =

// ------------------------------- Primitives -------------------------------

    let private pointer = ref 0
    let internal ZeroTime : Timestamp = System.UInt32.MinValue
    let internal NullRefOf typ = MakeNull typ Metadata.empty ZeroTime
    let private infiniteTime : Timestamp = System.UInt32.MaxValue
    let private timestamp = ref ZeroTime
    let private freshAddress () =
        pointer := !pointer + 1
        !pointer
    let internal tick () =
        timestamp := !timestamp + 1u
        !timestamp
    let public reset () =
        pointer := 0
        timestamp := ZeroTime

    type private LazyInstantiation(location : Term) =
        inherit SymbolicConstantSource()
        override this.SubTerms = Seq.singleton location
        member this.Location = location

    let internal (|LazyInstantiation|_|) (src : SymbolicConstantSource) =
        match src with
        | :? LazyInstantiation as li -> Some(LazyInstantiation li.Location)
        | _ -> None

    let private isStaticLocation = function
        | StaticRef _ -> true
        | _ -> false

    let rec internal defaultOf time metadata = function
        | Bool -> MakeFalse metadata
        | Numeric t when t.IsEnum -> CastConcrete (System.Activator.CreateInstance(t)) t metadata
        | Numeric t -> CastConcrete 0 t metadata
        | String -> Terms.MakeNull String metadata ZeroTime
        | PointerType t -> Terms.MakeNull t metadata ZeroTime
        | ClassType _ as t ->Terms.MakeNull t metadata ZeroTime
        | ArrayType _ as t -> Terms.MakeNull t metadata ZeroTime
        | SubType(dotNetType, _, _,  _) as t when dotNetType.IsValueType -> Struct Heap.empty t metadata
        | SubType _ as t -> Terms.MakeNull t metadata ZeroTime
        | Func _ -> Terms.MakeNull (FromGlobalSymbolicDotNetType typedefof<System.Delegate>) metadata ZeroTime
        | StructType(dotNetType, _, _) as t ->
            let fields = Types.GetFieldsOf dotNetType false in
            let contents = Seq.map (fun (k, v) -> (Terms.MakeConcreteString k metadata, (defaultOf time metadata v, time, time))) (Map.toSeq fields) |> Heap.ofSeq in
            Struct contents t metadata
        | _ -> __notImplemented__()

    let internal mkDefault metadata typ =
        defaultOf (tick()) metadata typ

    let internal mkDefaultStatic metadata qualifiedTypeName =
        let t = qualifiedTypeName |> System.Type.GetType |> FromConcreteDotNetType  in
        let time = tick() in
        let fields = DecompilerServices.getDefaultFieldValuesOf true false qualifiedTypeName in
        let contents =
            fields
                |> List.map (fun (n, (t, _)) ->
                                let key = Terms.MakeConcreteString n metadata in
                                let value = mkDefault metadata (FromConcreteMetadataType t) in
                                (key, (value, time, time)))
                |> Heap.ofSeq
        fields, t, Struct contents t metadata

    //let rec internal makeSymbolicStruct metadata time source t dotNetType =
    //    let fields = Types.GetFieldsOf dotNetType false
    //                    |> Map.toSeq
    //                    |> Seq.map (fun (name, typ) ->
    //                                    let key = MakeStringKey name in
    //                                    (key, makeSymbolicInstance metadata time (source key) name typ))
    //                    |> Heap.ofSeq
    //    in Struct fields t metadata

    let rec makeSymbolicInstance metadata time source name = function
        | PointerType t ->
            let constant = Constant name source pointerType metadata in
            HeapRef ((constant, t), []) time metadata
        | t when Types.IsPrimitive t || Types.IsFunction t -> Constant name source t metadata
        | StructType _
        | SubType _
        | ClassType _ as t -> Struct Heap.empty t metadata
        | ArrayType(e, d) as t -> VSharp.Array.makeSymbolic metadata source d t name
        | _ -> __notImplemented__()

    let internal genericLazyInstantiator =
        let instantiator metadata time fullyQualifiedLocation typ () =
            makeSymbolicInstance metadata time (LazyInstantiation fullyQualifiedLocation) (nameOfLocation fullyQualifiedLocation) typ
        in
        State.genericLazyInstantiator <- instantiator
        instantiator

    let rec private referenceSubLocation location term =
        match term.term with
        | Error _ -> term
        | StackRef(var, path) -> StackRef var (List.append path [location]) term.metadata
        | StaticRef(var, path) -> StaticRef var (List.append path [location]) term.metadata
        | HeapRef((addr, path), t) -> HeapRef (addr, List.append path [location]) t term.metadata
        | Terms.GuardedValues(gs, vs) -> vs |> List.map (referenceSubLocation location) |> List.zip gs |> Union term.metadata
        | _ -> internalfailf "expected reference, but got %O" term

// ------------------------------- Comparison -------------------------------

    let npe mtd state = State.activator.CreateInstance mtd typeof<System.NullReferenceException> [] state

    // TODO: make generic terms equality operator!
    let private locationEqual mtd addr1 addr2 =
        match TypeOf addr1, TypeOf addr2 with
        | String, String -> Strings.simplifyEquality mtd addr1 addr2
        | Numeric _, Numeric _ -> eq mtd addr1 addr2
        | _ -> __notImplemented__()

    let private canPoint mtd pointerAddr pointerType pointerTime locationAddr locationValue locationTime =
        // TODO: what if locationType is Null?
        if locationTime > pointerTime then Terms.MakeFalse mtd
        else
            let addrEqual = locationEqual mtd locationAddr pointerAddr in
            let typeSuits v =
                let locationType = TypeOf v in
                Common.is mtd locationType pointerType &&& Common.is mtd pointerType locationType
            in
            let typeEqual =
                match locationValue.term with
                | Union gvs ->
                    gvs |> List.map (fun (g, v) -> (g, typeSuits v)) |> Merging.merge
                | _ -> typeSuits locationValue
            in
            addrEqual &&& typeEqual

    let rec private refToInt term =
        match term.term with
        | Error _ -> term
        | Concrete(_, TermType.Null) -> Concrete 0 pointerType term.metadata
        | HeapRef(((addr, _), _), t) -> addr
        | Union gvs -> Merging.guardedMap refToInt gvs
        | _ -> term

    let rec internal referenceEqual mtd p1 p2 =
        let addr1 = refToInt p1 in
        let addr2 = refToInt p2 in
        if not(Terms.IsInteger addr1 || Terms.IsInteger addr2) then
            internalfail "reference comparing non-reference types"
        Arithmetics.simplifyEqual mtd addr1 addr2 id

    let rec internal isNull metadata term =
        match term.term with
        | Error _ -> term
        | Concrete(_, TermType.Null) -> MakeTrue metadata
        | HeapRef(((addr, _), _), t) when Terms.IsInteger addr ->
            Arithmetics.simplifyEqual metadata addr (Concrete 0 pointerType metadata) id
        | Union gvs -> Merging.guardedMap (isNull metadata) gvs
        | _ -> Terms.MakeFalse metadata

// ------------------------------- Dereferencing/mutation -------------------------------

    let private stackDeref time instantiateLazy state location =
        if isAllocatedOnStack state location then
            (readStackLocation state location, state)
        else
            let lazyInstance = instantiateLazy(), time, time in
            (lazyInstance, writeStackLocation state location lazyInstance)

    let private findSuitableLocations mtd h ptr ptrType ptrTime =
        let filterMapKey (k, ((v, created, modified) as cell)) =
            let guard = canPoint mtd ptr ptrType ptrTime k v created in
            match guard with
            | False -> None
            | _ -> Some(guard, k, cell)
        in
        let gvs = h |> Heap.toSeq |> List.ofSeq |> List.filterMap filterMapKey in
        let baseGvs, restGvs = gvs |> List.partition (fst3 >> IsTrue) in
        assert(List.length baseGvs <= 1)
        List.tryHead baseGvs, restGvs

    let rec private heapDeref metadata time instantiateLazy h ptr ptrType ptrTime =
        let exists = Heap.contains ptr h in
        if IsConcrete ptr && exists then
            [(MakeTrue metadata, ptr, Heap.find ptr h)], h
        else
            let baseGav, restGavs = findSuitableLocations metadata h ptr ptrType ptrTime in
            let baseGuard = restGavs |> List.map (fst3 >> (!!)) |> conjunction metadata in
            let baseAddr, baseValue, h' =
                match baseGav with
                | None ->
                    let lazyValue = instantiateLazy() in
                    let baseCell = lazyValue, time, time in
                    let h' = h.Add(ptr, baseCell) in
                    ptr, baseCell, h'
                | Some(_, a, v) -> a, v, h
            (baseGuard, baseAddr, baseValue)::restGavs, h'

    let private mutateHeap metadata time guard h addr newValue =
        assert(Heap.contains addr h)
        let (oldValue, created, modified) as oldCell = Heap.find addr h in
        let cell = Merging.merge2Cells guard !!guard (newValue, created, time) oldCell in
        Heap.add addr cell h

    let private structLazyInstantiator metadata fullyQualifiedLocation field fieldType () =
        makeSymbolicInstance metadata ZeroTime (LazyInstantiation fullyQualifiedLocation) (toString field) fieldType

    let private arrayElementLazyInstantiator metadata time fullyQualifiedLocation idx typ = function
        | None -> fun () -> defaultOf time metadata typ
        | Some constant -> fun () ->
            let id = sprintf "%s[%s]" (toString constant) (toString idx) |> IdGenerator.startingWith in
            makeSymbolicInstance metadata time (LazyInstantiation fullyQualifiedLocation) id typ

    let private staticMemoryLazyInstantiator metadata t location () =
        Struct Heap.empty (FromConcreteDotNetType t) metadata

    let rec private accessTerm metadata guard update created modified ptrTime ctx path term =
        match path with
        | [] ->
            let newTerm, newTime = update term modified in
            newTerm, newTerm, newTime
        | ((key, typ) as location)::path' ->
            match term.term with
            | Error _ -> term, term, modified
            | Struct(fields, t) ->
                let ctx' = referenceSubLocation location ctx in
                let instantiator = structLazyInstantiator term.metadata ctx' key typ in
                let result, newFields, newTime =
                    accessHeap metadata guard update fields created (fun loc -> referenceSubLocation (loc, typ) ctx) instantiator key t ptrTime path'
                in result, Struct newFields t term.metadata, newTime
            | Array(lower, constant, contents, lengths, (ArrayType(t, _) as typ)) ->
                let ctx' = referenceSubLocation location ctx in
                let instantiator = arrayElementLazyInstantiator term.metadata modified ctx' key t constant in
                let result, newContents, newTime =
                    accessHeap metadata guard update contents created (fun loc -> referenceSubLocation (loc, t) ctx) instantiator key t ptrTime path'
                in result, Array lower constant newContents lengths typ term.metadata, newTime
            | Union gvs ->
                internalfail "unexpected union of complex types! Probably merge function implemented incorrectly."
            | t ->
                internalfailf "expected complex type, but got %O" t

    and private accessHeap metadata guard update h time mkCtx lazyInstantiator ptr ptrType ptrTime path =
        let gvas, h = heapDeref metadata time lazyInstantiator h ptr ptrType ptrTime in
        let gvs, (h', newTime) = gvas |> ((h, ZeroTime) |> List.mapFold (fun (h, maxTime) (guard', addr, (baseValue, created, modified)) ->
            let ctx = mkCtx addr in
            let guard'' = guard &&& guard' in
            let accessedValue, newBaseValue, newTime = accessTerm metadata guard update created modified ptrTime ctx path baseValue in
            let h' = if baseValue = newBaseValue then h else mutateHeap metadata newTime guard'' h addr newBaseValue
            ((guard, accessedValue), (h', max maxTime newTime))))
        in (Merging.merge gvs, h', newTime)

    let rec private commonHierarchicalAccess actionNull update metadata state term =
        match term.term with
        | Error _ -> (term, state)
        | StackRef(location, path) ->
            let firstLocation = stackLocationToReference state location in
            let time = frameTime state location in
            let (baseValue, created, modified), h' = stackDeref time (fun () -> stackLazyInstantiator state time location |> fst3) state location in
            let accessedValue, newBaseValue, newTime = accessTerm metadata (Terms.MakeTrue metadata) update created modified time firstLocation path baseValue in
            let newState = if baseValue = newBaseValue then state else writeStackLocation state location (newBaseValue, created, newTime) in
            accessedValue, newState
        | StaticRef(location, path) ->
            let firstLocation = Terms.term >> function
                | Concrete(location, String) -> StaticRef (location :?> string) [] term.metadata
                | _ -> __notImplemented__()
            in
            let addr = Terms.MakeStringKey location in
            let dnt = System.Type.GetType(location) in
            let t = FromConcreteDotNetType dnt in
            let result, m', _ = accessHeap metadata (MakeTrue metadata) update (staticsOf state) ZeroTime firstLocation (staticMemoryLazyInstantiator Metadata.empty dnt location) addr t infiniteTime path in
            result, withStatics state m'
        | HeapRef(((addr, t) as location, path), time) ->
            let mkFirstLocation location = HeapRef ((location, t), []) time term.metadata in
            let firstLocation = HeapRef (location, []) time term.metadata in
            let isNull = Arithmetics.simplifyEqual metadata addr (Concrete 0 pointerType metadata) id in
            match isNull with
            | Terms.True -> actionNull metadata state t
            | Terms.False ->
                let result, h', _ = accessHeap metadata (Terms.MakeTrue metadata) update (heapOf state) ZeroTime mkFirstLocation (genericLazyInstantiator Metadata.empty time firstLocation t) addr t time path in
                result, withHeap state h'
            | _ ->
                let result, h', _ = accessHeap metadata (Terms.MakeTrue metadata) update (heapOf state) ZeroTime mkFirstLocation (genericLazyInstantiator Metadata.empty time firstLocation t) addr t time path in
                let notNullCaseState = withHeap state h' in
                let nullCaseResult, state' = actionNull metadata state t in
                Merging.merge2Terms isNull !!isNull nullCaseResult result, Merging.merge2States isNull !!isNull state' notNullCaseState
        | Union gvs -> Merging.guardedStateMap (commonHierarchicalAccess actionNull update metadata state) gvs state
        | t -> internalfailf "expected reference, but got %O" t

    let private hierarchicalAccess = commonHierarchicalAccess (fun m s _ ->
        let res, state = npe m s
        Error res m, state)

    let internal deref = hierarchicalAccess makePair

    let internal derefWith actionNull = commonHierarchicalAccess actionNull makePair

    let internal mutate metadata state reference value =
        assert(value <> Nop)
        let time = tick() in
        hierarchicalAccess (fun _ _ -> (value, time)) metadata state reference

    let rec private derefPathIfInstantiated term = function
        | [] -> Some term
        | (loc, _)::path' ->
            match term.term with
            | Struct(contents, _)
            | Array(_, _, contents, _, _) ->
                if Heap.contains loc contents then derefPathIfInstantiated (fst3 contents.[loc]) path' else None
            | _ -> internalfailf "expected complex type, but got %O" term

    let internal derefIfInstantiated state = term >> function
        | StackRef(addr, path) ->
            if isAllocatedOnStack state addr then
                derefPathIfInstantiated (readStackLocation state addr |> fst3) path
            else None
        | HeapRef(((addr, _), path), _) ->
            if isAllocatedInHeap state addr then
                derefPathIfInstantiated (readHeapLocation state addr) path
            else None
        | StaticRef(addr, path) ->
            if staticMembersInitialized state addr then
                derefPathIfInstantiated (readStaticLocation state (MakeStringKey addr)) path
            else None
        | term -> internalfailf "expected reference, but %O got" term

// ------------------------------- Referencing -------------------------------

    let rec private referenceTerm state name followHeapRefs term =
        match term.term with
        | Error _
        | StackRef _
        | StaticRef _
        | HeapRef _ when followHeapRefs -> term
        | Union gvs -> Merging.guardedMap (referenceTerm state name followHeapRefs) gvs
        | _ -> StackRef name [] term.metadata

    let internal referenceLocalVariable metadata state location followHeapRefs =
        let reference = StackRef location [] metadata in
        let term, state = deref metadata state reference in
        referenceTerm state location followHeapRefs term

    let rec private referenceFieldOf state field parentRef reference =
        match reference.term with
        | Error _ -> reference, state
        | HeapRef((addr, path), t) ->
            assert(List.isEmpty path) // TODO: will this really be always empty?
            HeapRef (addr, [field]) t reference.metadata, state
        | Struct _ -> referenceSubLocation field parentRef, state
        | Union gvs -> Merging.guardedStateMap (referenceFieldOf state field parentRef) gvs state
        | Concrete(_, TermType.Null) ->
            let term, state = State.activator.CreateInstance reference.metadata typeof<System.NullReferenceException> [] state in
            Error term reference.metadata, state
        | t -> internalfailf "expected reference or struct, but got %O" t, state

    let rec private followOrReturnReference metadata state reference =
        let term, state = deref metadata state reference in
        match term.term with
        | Error _
        | StackRef _
        | StaticRef _
        | HeapRef _ -> term, state
        | Union gvs when List.forall (fun (_, t) -> IsError t || IsRef t) gvs ->
            Merging.guardedStateMap (followOrReturnReference metadata state) gvs state
        | _ -> reference, state

    let internal referenceField metadata state followHeapRefs name typ parentRef =
        let typ = Types.PointerFromReferenceType typ in
        let term, state = deref metadata state parentRef in
        let reference, newState = referenceFieldOf state (Terms.MakeStringKey name, typ) parentRef term in
        if followHeapRefs then followOrReturnReference metadata newState reference
        else (reference, newState)

    let internal referenceStaticField metadata state followHeapRefs fieldName typ typeName =
        let typ = Types.PointerFromReferenceType typ in
        let reference = StaticRef typeName [(Terms.MakeStringKey fieldName, typ)] metadata in
        if followHeapRefs then followOrReturnReference metadata state reference
        else (reference, state)

    let internal referenceArrayIndex metadata state arrayRef indices =
        // TODO: what about followHeapRefs?
        let array, state = deref metadata state arrayRef in
        match array.term with
        | Error _ -> (array, state)
        | Array(lowerBounds, _, _, dimensions, t) ->
            let inBounds = VSharp.Array.checkIndices metadata lowerBounds dimensions indices in
            let physicalIndex = VSharp.Array.physicalIndex metadata lowerBounds dimensions indices in
            let reference = referenceSubLocation (physicalIndex, t) arrayRef in
            match inBounds with
            | True -> (reference, state)
            | _ ->
                let exn, state' = State.activator.CreateInstance metadata typeof<System.IndexOutOfRangeException> [] state in
                Merging.merge2Terms inBounds !!inBounds reference (Error exn metadata), Merging.merge2States inBounds !!inBounds state state'
        | t -> internalfail ("accessing index of non-array term " + toString t)

    let internal derefLocalVariable metadata state id =
        referenceLocalVariable metadata state id false |> deref metadata state

// ------------------------------- Allocation -------------------------------

    let internal newStackFrame state metadata funcId frame = State.newStackFrame (tick()) metadata state funcId frame
    let internal newScope state frame = State.newScope (tick()) state frame

    let internal freshHeapLocation metadata =
        Concrete (freshAddress()) pointerType metadata

    let internal allocateOnStack metadata ((s, h, m, (f, sh), p) as state : state) key term : state =
        let time = tick() in
        let frameMetadata, oldFrame, frameTime = Stack.peak f in
        (pushToCurrentStackFrame state key (term, time, time), h, m, (Stack.updateHead f (frameMetadata, (key, metadata, None)::oldFrame, frameTime), sh), p)

    let internal allocateInHeap metadata ((s, h, m, f, p) : state) term : Term * state =
        let address = freshHeapLocation metadata in
        let time = tick() in
        let pointer = HeapRef ((address, Terms.TypeOf term), []) time metadata in
        (pointer, (s, h.Add(address, (term, time, time)), m, f, p))

    let internal allocateInStaticMemory metadata ((s, h, m, f, p) : state) typeName term =
        let time = tick() in
        let address = Terms.MakeConcreteString typeName metadata in
        (s, h, m.Add(address, (term, time, time)), f, p)

    let internal allocateSymbolicInstance metadata state t =
        match t with
        | TermType.ClassType(tp, arg, interfaces) ->
            let contents = makeSymbolicInstance metadata ZeroTime (LazyInstantiation Nop) "" (StructType(tp, arg, interfaces)) in
            allocateInHeap metadata state contents
        | StructType _ ->
            makeSymbolicInstance metadata ZeroTime (LazyInstantiation Nop) "" t, state
        | _ -> __notImplemented__()

// ------------------------------- Comparison -------------------------------

    type internal StateDiff =
        | Mutation of Term * Term
        | Allocation of Term * Term

    let private compareHeaps h1 h2 =
        assert(Heap.size h1 <= Heap.size h2)
        let oldValues, newValues = Heap.partition (fun (k, _) -> Heap.contains k h1) h2 in
        let changedValues = List.filter (fun (k, v) -> (fst3 h1.[k]) <> v) oldValues in
        changedValues, newValues

    let rec private addrLess addr1 addr2 =
        match addr1.term, addr2.term with
        | Concrete(a1, t1), Concrete(a2, t2) ->
            match t1, t2 with
            | _ when t1 = t2 && t1 = pointerType -> compare (a1 :?> int) (a2 :?> int)
            | String, String -> compare (a1 :?> string) (a2 :?> string)
            | _, String when t1 = pointerType -> -1
            | String, _ when t2 = pointerType -> 1
            | _ -> __notImplemented__()
        | Constant(name1, _, _), Constant(name2, _, _) -> compare name1 name2
        | Concrete _, _ -> -1
        | _, Concrete _ -> 1
        | _ -> __notImplemented__()

    let private comparePaths = List.compareWith (fun (a1, _) (a2, _) -> addrLess a1 a2)

    let rec internal compareRefs ref1 ref2 =
        match ref1, ref2 with
        | _ when ref1 = ref2 -> 0
        | HeapRef(((addr1, _), path1), _), HeapRef(((addr2, _), path2), _) ->
            let h = addrLess addr1 addr2 in
            if h = 0 then comparePaths path1 path2 else h
        | StackRef(name1, path1), StackRef(name2, path2) ->
            let h = compare name1 name2 in
            if h = 0 then comparePaths path1 path2 else h
        | StaticRef(name1, path1), StaticRef(name2, path2) ->
            let h = compare name1 name2 in
            if h = 0 then comparePaths path1 path2 else h
        | StackRef _, _ -> -1
        | _, StackRef _ -> 1
        | HeapRef _, _ -> -1
        | _, HeapRef _ -> 1
        | _ -> internalfail "compareRefs called with non-reference terms"

    let private sortMap mapper = List.sortWith (fun (k1, _) (k2, _) -> addrLess k1 k2) >> List.map mapper

    let rec private isDefaultValue reference cell =
        match (fst3 cell).term with
        | Constant(_, LazyInstantiation reference', _) -> reference = reference'
        | Struct(contents, _)
        | Array(_, _, contents, _, _) ->
            contents |> Heap.toSeq |> Seq.forall (fun (k, cell) -> isDefaultValue (referenceSubLocation (k, Terms.TypeOf (fst3 cell)) reference) cell)
        | _ -> false

    let rec private isFreshLocation startTime (_, time, _) =
        time > startTime

    let rec private inspectLocation startTime ctx goDeep (fresh, mutated) (k, cell) =
        let reference = ctx (k, cell) in
        if isFreshLocation startTime cell then ((reference, cell)::fresh, mutated)
        elif isDefaultValue reference cell then (fresh, mutated)
        else
            let subFresh, subMutated = goDeep startTime reference cell in
            (List.append subFresh fresh, List.append subMutated mutated)

    let rec private affectedStackLocations startTime ctx s =
        s |> stackFold (fun s k v -> inspectLocation startTime ctx affectedSubLocations s (k, v)) ([], [])

    and private affectedHeapLocations startTime ctx h =
        h |> Heap.toSeq |> Seq.fold (inspectLocation startTime ctx affectedSubLocations) ([], [])

    and private affectedSubLocations startTime ctx cell =
        match (fst3 cell).term with
        | Struct(contents, _)
        | Array(_, _, contents, _, _) ->
            affectedHeapLocations startTime (fun (loc, (v, _, _)) -> referenceSubLocation (loc, TypeOf v) ctx) contents
        | _ -> ([], [(ctx, cell)])

    let rec internal affectedLocations startTime ((s, h, m, _, _) as state : state) =
        let freshStack,   mutatedStack    = affectedStackLocations startTime (fst >> stackLocationToReference state) s in
        let freshHeap,    mutatedHeap     = affectedHeapLocations startTime (fun (loc, (v, t, _)) -> HeapRef((loc, TypeOf v), []) t loc.metadata) h in
        let freshStatics, mutatedStatics  = affectedHeapLocations startTime (fst >> staticLocationToReference) m in
        List.append3 freshStack freshHeap freshStatics, List.append3 mutatedStack mutatedHeap mutatedStatics
