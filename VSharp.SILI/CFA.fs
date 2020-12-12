namespace VSharp.Analyzer

open System
open System.Reflection
open System.Collections.Generic

open System.Reflection.Emit
open VSharp.Interpreter.IL
open CFG
open FSharpx.Collections
open VSharp
open VSharp.Core


type opStackSource =
    {shift : uint32; typ : symbolicType; time : vectorTime}
    interface IMemoryAccessConstantSource with
        override x.SubTerms = Seq.empty
        override x.Time = x.time
        override x.TypeOfLocation = x.typ
        override x.Compose state =
            let result = List.item (int x.shift) state.opStack
            assert(CanWrite result x.typ) // TODO: what if (0:int) is assigned to reference?
            result


module Properties =
    let internal exitVertexOffset = -1
    let internal exitVertexNumber = -1

    let internal initialVertexOffset = 0
    let internal initialVertexNumber = 0

module public CFA =
    let mutable stepItp : ILInterpreter option = None
    let configureInterpreter itp = stepItp <- Some itp

    let withState = InstructionsSet.withState
    let withOpStack = InstructionsSet.withOpStack
    let withIp = InstructionsSet.withIp
    let pushToOpStack = InstructionsSet.pushToOpStack

    let pushNewObjResultOnOpStack state reference (calledMethod : MethodBase) =
        let valueOnStack =
            if calledMethod.DeclaringType.IsValueType then
                  Memory.ReadSafe state reference
            else reference
        {state with opStack = valueOnStack :: state.opStack}

    type vertexLabel =
        | FromCFG of offset
        | PreCatchVertex of offset * ExceptionHandlingClause
        | MethodCommonExit
        with
        member x.Foo() = ()

    // TODO: use vertexLabel instead of offset
//    [<AllowNullLiteral;CustomEquality;CustomComparison>]
//    [<CustomEquality;CustomComparison>]
    type Vertex private(id, m : MethodBase, offset, opStack : operationalStack) =
        static let ids : Dictionary<MethodBase, int> = Dictionary<_,_>()
        let lemmas = Lemmas(m, offset)
        let paths = Paths(m, offset)
//        let paths : Paths persistent = let r = new persistent<_>(always (Paths(m, offset)), id) in r.Reset(); r
        let queries = Queries(m, offset)
        let solver = null //Solver.SolverPool.mkSolver()
        let errors = List<cilState>()
        let incomingEdges: List<Edge> = List<_>()
        let outgoingEdges: List<Edge> = List<_>()

        override x.GetHashCode() = (m, offset).GetHashCode()
        override x.Equals(o : obj) =
            match o with
            | :? Vertex as other -> x.Method = other.Method && x.Ip = other.Ip
            | _ -> false
        interface System.IComparable with
            override x.CompareTo(other) =
                match other with
                | :? Vertex as other when x.Method.Equals(other.Method) -> x.GetHashCode().CompareTo(other.GetHashCode())
                | :? Vertex as other -> x.Method.MetadataToken.CompareTo(other.Method.MetadataToken)
                | _ -> -1
        member x.AddErroredStates (newErrors : cilState list) =
            errors.AddRange(newErrors)

        member x.Id with get() = id
        member x.Lemmas = lemmas
//        member x.Reset () = paths.Reset()
//        member x.Restore () = paths.Restore()
        member x.Paths with get () = paths
//        member x.Paths with get() =
//            paths.Value
        member x.Queries = queries
        member x.Solver = solver
        member x.IncomingEdges = incomingEdges
        member x.OutgoingEdges = outgoingEdges
        member x.Ip with get() = if x.IsMethodExitVertex then Exit else Instruction offset
        member x.OpStack with get() = opStack
        member x.Method with get() = m
        member x.IsMethodStartVertex with get() = offset = 0
        member x.IsMethodExitVertex with get() = offset = -1
        override x.ToString() =
            sprintf "(Method = %O, Offset = %x, id = %d)\n" m offset id +
            sprintf "Edges count = %d\n" x.OutgoingEdges.Count +
            "Edges: \n" + Seq.fold (fun acc edge -> acc + edge.ToString() + "\n") "" x.OutgoingEdges
        static member CreateVertex method offset opStack =
            let id = Dict.getValueOrUpdate ids method (always 0)
            ids.[method] <- id + 1
            Vertex(id, method, offset, opStack)
    and
        [<AbstractClass>]
        Edge(src : Vertex, dst : Vertex) =
        abstract member Type : string
        abstract member PropagatePath : path -> bool
        member x.PrintLog msg obj = Logger.printLog Logger.Trace "[%s]\n%s: %O" (x.commonToString()) msg obj
        member x.Src = src
        member x.Dst = dst
        member x.Method = x.Src.Method
        member x.CommonPropagatePath lvl state =
            let newPc = PC.squashPC state.pc
            if newPc <> Terms.False then
                if dst.IsMethodExitVertex then ()
                dst.Paths.Add {lvl = lvl; state = state}
                true
            else false
        override x.ToString() = x.commonToString()
        member x.commonToString() =
            sprintf "%s ID:[%d --> %d] IP:[%O --> %O] Method:%s"
                x.Type x.Src.Id x.Dst.Id x.Src.Ip x.Dst.Ip (Reflection.GetFullMethodName x.Method)


    type 'a unitBlock =
        {
            entity : 'a
            entryPoint : Vertex
            exitVertex : Vertex
            vertices : Dictionary<int, Vertex>
        }
        with
        member x.AddVertex (v : Vertex) =
            if not <| x.vertices.ContainsKey v.Id then
                x.vertices.Add(v.Id, v)
        member x.WithEntryPoint v = {x with entryPoint = v}
        member x.WithExitVertex v = {x with exitVertex = v}
        member x.FindCallVertex id = x.vertices.[id]

        static member private CreateEmpty entity method entryOffset exitOffset =
            let entry = Vertex.CreateVertex method entryOffset []
            let exit = Vertex.CreateVertex method exitOffset []
            let vertices = Dictionary<int, Vertex>()
            vertices.Add(entry.Id, entry)
            vertices.Add(exit.Id, exit)
            {
                entity = entity
                entryPoint = entry
                exitVertex = exit
                vertices = vertices
            }
        static member CreateEmptyForMethod (method : MethodBase) =
            unitBlock<'a>.CreateEmpty method method Properties.initialVertexOffset Properties.exitVertexOffset
        static member CreateEmptyForFinallyClause (m : MethodBase) (ehc : ExceptionHandlingClause) =
            let entryOffset = ehc.HandlerOffset
            // TODO: check if this formula is forever true
            let exitOffset = ehc.HandlerOffset + ehc.HandlerLength - 1
            unitBlock<'a>.CreateEmpty ehc m entryOffset exitOffset
        override x.ToString() =
            Seq.fold (fun acc vertex -> acc + "\n" + vertex.ToString()) "" x.vertices.Values

    type cfa =
            {
              cfg : cfgData
              body : MethodBase unitBlock
              finallyHandlers : Dictionary<offset, ExceptionHandlingClause unitBlock>
            }
        with
            member x.Method = x.cfg.methodBase
            member x.FindOrCreateFinallyHandler (ehc : ExceptionHandlingClause) =
                let offset = ehc.HandlerOffset
                if x.finallyHandlers.ContainsKey offset then x.finallyHandlers.[offset]
                else
                    let block = unitBlock<ExceptionHandlingClause>.CreateEmptyForFinallyClause x.Method ehc
                    x.finallyHandlers.Add(offset, block)
                    block

            member x.WithEntryPoint v = {x with body = x.body.WithEntryPoint v}
            member x.WithExitVertex v = {x with body = x.body.WithExitVertex v}
            member x.WithFinallyEntryPoint offset v = x.finallyHandlers.[offset].WithEntryPoint v |> ignore; x
            member x.WithFinallyExitVertex offset v = x.finallyHandlers.[offset].WithExitVertex v |> ignore; x
            override x.ToString() =
                let separator = "\n"
                "Method: " + x.Method.ToString() + separator +
                x.body.ToString() + separator +
                Seq.fold (fun acc block -> acc + block.ToString() + separator) "" (x.finallyHandlers.Values)



    type StepEdge(src : Vertex, dst : Vertex, effect : state) =
        inherit Edge(src, dst)
        do
            Prelude.releaseAssert(Map.isEmpty effect.callSiteResults)
        override x.Type = "StepEdge"
        override x.PropagatePath (path : path) =

            Memory.ComposeStates path.state effect (fun states ->
                x.PrintLog "composition left:\n"  <| Memory.Dump path.state
                x.PrintLog "composition right:\n" <| Memory.Dump effect
                x.PrintLog (sprintf "composition resulted in %d states:\n" <| List.length states) <| (List.map Memory.Dump states |> join "\n")
                assert(List.forall (fun state -> path.state.frames = state.frames) states)
                // Do NOT turn this List.fold into List.exists to be sure that EVERY returned state is propagated
                List.fold (fun acc state -> acc || x.CommonPropagatePath (path.lvl + 1u) state) false states)

        member x.Effect = effect
        member x.VisibleVariables() = __notImplemented__()

        override x.ToString() =
            sprintf "%s\neffect = %O\npc = %O\n" (base.ToString()) (API.Memory.Dump effect) effect.pc

    type CallEdge(src : Vertex, dst : Vertex, callSite : callSite, stateWithArgsOnFrameAndAllocatedType : state, numberToDrop) =
        inherit Edge(src, dst)
        do
           assert(List.length stateWithArgsOnFrameAndAllocatedType.frames = 2)
        override x.Type = "Call"
        override x.PropagatePath (path : path) =
            let k states =
                let propagateStateAfterCall acc state =
                    assert(path.state.frames = state.frames)
                    x.PrintLog "propagation through callEdge:\n" callSite
                    x.PrintLog "call edge: composition left:\n" (Memory.Dump path.state)
                    x.PrintLog "call edge: composition result:\n" (Memory.Dump state)
                    let stateAfterCall =
                        let opStack = List.skip numberToDrop path.state.opStack
                        if callSite.HasNonVoidResult then
                            assert(Option.isSome state.returnRegister)
                            { state with
                                callSiteResults = Map.add callSite state.returnRegister path.state.callSiteResults
                                returnRegister = None
                                opStack = Option.get state.returnRegister :: opStack }
                        elif callSite.opCode = OpCodes.Newobj then
                            let reference = Memory.ReadThis stateWithArgsOnFrameAndAllocatedType callSite.calledMethod
                            let state = pushNewObjResultOnOpStack {state with opStack = opStack} reference callSite.calledMethod
                            { state with callSiteResults = path.state.callSiteResults}
                        else { state with callSiteResults = path.state.callSiteResults; opStack = opStack}

                    let result' = x.CommonPropagatePath (path.lvl + 1u) stateAfterCall
                    acc || result'
                List.fold propagateStateAfterCall false states
//            let k1 results = results |> List.unzip |> snd |> k
            Prelude.releaseAssert (Option.isSome stepItp)
            let interpreter = stepItp |> Option.get
            let states = Memory.ComposeStates path.state stateWithArgsOnFrameAndAllocatedType id
            match states with
            | [state] ->
                match callSite.opCode with
                | Instruction.NewObj   when Reflection.IsDelegateConstructor callSite.calledMethod -> k [Memory.PopStack state]
                | Instruction.NewObj   when Reflection.IsArrayConstructor callSite.calledMethod -> k [Memory.PopStack state]
                | Instruction.Call
                | Instruction.NewObj   ->
                    interpreter.CommonCall callSite.calledMethod state k
                | Instruction.CallVirt -> interpreter.CommonCallVirt callSite.calledMethod state k
                | _ ->  __notImplemented__()
            | _ -> internalfailf "Calling %s: composition with frames unexpectedly forked!" callSite.calledMethod.Name
        member x.ExitNodeForCall() = __notImplemented__()
        member x.CallVariables() = __notImplemented__()
        override x.ToString() =
            sprintf "%s\nstate = %O\n" (base.ToString()) (API.Memory.Dump stateWithArgsOnFrameAndAllocatedType)

    module cfaBuilder =
        let private alreadyComputedCFAs = Dictionary<MethodBase, cfa>()

        type bypassConcreteInfo =
            {
              allocatedTypes : pdict<concreteHeapAddress, symbolicType>
              lengths : pdict<arrayType, vectorRegion>
              lowerBounds : pdict<arrayType, vectorRegion>
            }
            with
            static member fromState (s : state) = {allocatedTypes = s.allocatedTypes; lengths = s.lengths; lowerBounds = s.lowerBounds}
        type reachableConcreteInfo = {map : pdict<offset, pset<bypassConcreteInfo>> }
            with
            static member find offset (reachable : reachableConcreteInfo) =
                if PersistentDict.contains offset reachable.map then PersistentDict.find reachable.map offset
                else internalfail "reachableConcreteInfo does not contain key %d " offset
            static member append offset (s : state) (reachable : reachableConcreteInfo) =
                let info = bypassConcreteInfo.fromState s
                let set = if PersistentDict.contains offset reachable.map
                          then PersistentDict.find reachable.map offset
                          else PersistentSet.empty

                if PersistentSet.contains info set then reachable
                else { map = PersistentDict.add offset (PersistentSet.add set info) reachable.map }

            static member empty = { map = PersistentDict.empty }

        [<CustomEquality; CustomComparison>]
        type bypassDataForEdges =
            { u : ip
              srcVertex : Vertex
              v : ip
              uOut : int
              vOut : int
              minSCCs : int
              opStack : operationalStack
            }
            override x.ToString() =
                let emptyState = {API.Memory.EmptyState with opStack = x.opStack }
                sprintf "u = %O, id = %d; v = %O; state = %s" x.u x.srcVertex.Id x.v (Memory.Dump emptyState)
            override x.GetHashCode() = (x.u, x.v, x.srcVertex).GetHashCode()
            override x.Equals y =
                match y with
                | :? bypassDataForEdges as y -> x.u = y.u && x.srcVertex.Equals(y.srcVertex) && x.v = y.v && x.opStack = y.opStack
                | _ -> false
            interface IComparable with
                override x.CompareTo (obj : obj) =
                    let rec compareData a1 a2 a3 b1 b2 b3 =
                        if a1 > b1 || a1 = b1 && a2 > b2 || a1 = b1 && a2 = b2 && a3 > b3 then -1
                        elif a1 = b1 && a2 = b2 && a3 = b3 then 0
                        else 1

                    match obj with
                    | :? bypassDataForEdges as other -> compareData x.minSCCs x.uOut x.vOut other.minSCCs other.uOut other.vOut
                    | _ -> -1


        let private createEmptyCFA cfg method =
            {
              cfg = cfg
              body = unitBlock<MethodBase>.CreateEmptyForMethod method
              finallyHandlers = Dictionary<_,_>()
            }

        let private addEdge (edge : Edge) =
            edge.Src.OutgoingEdges.Add edge
            edge.Dst.IncomingEdges.Add edge

        let makeSymbolicOpStack time (opStack : term list) : term list=
            let mkSource (index : int) typ =
                {shift = uint32 index; typ = typ; time = time}

            let makeSymbolic index (v : term) =
                if Terms.IsIdempotent v then v
                else
                    let shift = index
                    let typ = TypeOf v
                    let source = mkSource shift typ
                    let name = sprintf "opStack.[%d] from top" shift
                    Memory.MakeSymbolicValue source name typ

            let symbolicOpStack = List.mapi makeSymbolic opStack
            symbolicOpStack

        let private executeSeparatedOpCode (ilintptr : ILInterpreter) (cfg : cfg) (opCode : System.Reflection.Emit.OpCode) (cilState : cilState) =
            let offset = cilState.ip.Offset()
            let calledMethod = InstructionsSet.resolveMethodFromMetadata cfg (offset + opCode.Size)
            let callSite = { sourceMethod = cfg.methodBase; offset = offset; calledMethod = calledMethod; opCode = opCode }
            let pushFunctionResultOnOpStackIfNeeded (cilState : cilState) (methodInfo : System.Reflection.MethodInfo) =
                if methodInfo.ReturnType = typeof<System.Void> then cilState
                else pushToOpStack (Terms.MakeFunctionResultConstant cilState.state callSite) cilState

            let args, cilStateWithoutArgs = InstructionsSet.retrieveActualParameters calledMethod cilState
            let this, cilState =
                match calledMethod with
                | _ when opCode = OpCodes.Newobj ->
                    let states = ilintptr.CommonNewObj false (calledMethod :?> ConstructorInfo) cilStateWithoutArgs.state args id // TODO: what if newobj returns a lot of references and states?
                    let state = List.head states
                    assert(Option.isSome state.returnRegister)
                    let reference = Option.get state.returnRegister
                    let state = pushNewObjResultOnOpStack state reference calledMethod
                    Some reference, cilStateWithoutArgs |> withState {state with returnRegister = None}
                | :? ConstructorInfo -> InstructionsSet.popOperationalStack cilStateWithoutArgs
                | :? MethodInfo as methodInfo when not calledMethod.IsStatic || opCode = System.Reflection.Emit.OpCodes.Callvirt -> // TODO: check if condition `opCode = OpCodes.Callvirt` needed
                    let this, cilState = InstructionsSet.popOperationalStack cilStateWithoutArgs
                    this, pushFunctionResultOnOpStackIfNeeded cilState methodInfo
                | :? MethodInfo as methodInfo ->
                    None, pushFunctionResultOnOpStackIfNeeded cilStateWithoutArgs methodInfo
                | _ -> internalfailf "unknown methodBase %O" calledMethod

            let nextOffset =
                assert(cfg.graph.[offset].Count = 1)
                cfg.graph.[offset].[0]
            this, args, {cilState with ip = Instruction nextOffset}

        let createVertexIfNeeded (block : unitBlock<'a>) createVertex symbolicOpStack (d : bypassDataForEdges) (vertices : pdict<ip * operationalStack, int>)  =
            if PersistentDict.contains (d.v, symbolicOpStack) vertices then
                block.vertices.[PersistentDict.find vertices (d.v, symbolicOpStack)], vertices
            else
                let dstVertex : Vertex = createVertex d.v symbolicOpStack
                block.AddVertex dstVertex
                dstVertex, PersistentDict.add (d.v, symbolicOpStack) dstVertex.Id vertices

        let renewQ (cfg : cfg) newU newData (q : IPriorityQueue<bypassDataForEdges>, used : pset<bypassDataForEdges>) =
            let changeData w (d : bypassDataForEdges) =
                match w with
                | Instruction wOffset -> {d with v = w; vOut = cfg.dfsOut.[wOffset]; minSCCs = min cfg.sccOut.[wOffset] cfg.sccOut.[wOffset]}
                | Exit -> {d with v = Exit; vOut = -1; minSCCs = -1}
                | _ -> __notImplemented__()

            let updateQ (q, used : pset<bypassDataForEdges>) d =
                if PersistentSet.contains d used then q,used
                else PriorityQueue.insert d q, PersistentSet.add used d

            match newU with
            | Exit -> q, used
            | Instruction offset when cfg.graph.[offset].Count = 0 -> newData |> List.map (changeData Exit) |> List.fold updateQ (q, used)
            | Instruction vOffset -> cfg.graph.[vOffset] |> Seq.fold (fun acc wOffset -> newData |> List.map (changeData <| Instruction wOffset) |> List.fold updateQ acc) (q, used)
            | _ -> __notImplemented__()

        let addStepEdge (d : bypassDataForEdges) createVertex (data, reachable : reachableConcreteInfo, currentTime, vertices : pdict<ip * operationalStack, int>) (cilState' : cilState) =
            assert(cilState'.ip = d.v)
            let s' = cilState'.state
            let symbolicOpStack = makeSymbolicOpStack currentTime s'.opStack
            let dstVertex, vertices = createVertex symbolicOpStack d vertices
            addEdge <| StepEdge(d.srcVertex, dstVertex, cilState'.state)

            // TODO: handle cilState'.leaveInstructionExecuted
            let bypassData = {d with u = d.v; srcVertex = dstVertex; uOut = d.vOut; opStack = symbolicOpStack}

            let newReachable =
                match d.v with
                | Exit -> reachable // TODO: substitute ``offset'' to ``ip'' for reachableConcreteInfo
                | Instruction offset -> reachableConcreteInfo.append offset cilState'.state reachable
                | _ -> __notImplemented__()
            let data = if d.v <> Exit then bypassData :: data else data
            data, newReachable, VectorTime.max currentTime cilState'.state.currentTime, vertices

        let rec getConcreteInfo (cfg : cfg) (reachable : reachableConcreteInfo) (offset : offset) =
            if cfg.reverseGraph.[offset].Count <= 1 then
                let set = reachableConcreteInfo.find offset reachable
                assert (PersistentSet.cardinality set = 1)
                set |> PersistentSet.toSeq |> Seq.head
            else
                getConcreteInfo cfg reachable (PersistentHashMap.find offset cfg.idoms)

        let private computeCFAForBlock (ilintptr : ILInterpreter) (initialState : state) (cfa : cfa) (block : unitBlock<'a>) =
            let cfg = cfa.cfg
            // TODO: change offset to ip for Vertex
            let ip2Offset (ip : ip) =
                match ip with
                | Instruction i -> i
                | Exit -> -1
                | _ -> __notImplemented__()

            // note: entry point and exit vertex must be added to unit block
            let rec bypass (q : IPriorityQueue<bypassDataForEdges>) (used : pset<bypassDataForEdges>) (reachable : reachableConcreteInfo) (vertices : pdict<ip * operationalStack, int>) currentTime =
                let d, q = PriorityQueue.pop q
                Logger.trace "CFA.Bypass: %O; currentTime = %O" d currentTime
                assert(PersistentSet.contains d used)
                let srcVertex = d.srcVertex
                assert(d.u <> Exit)
                let offset = d.u.Offset()


                let bci : bypassConcreteInfo = getConcreteInfo cfg reachable offset
                let modifiedState = {initialState with currentTime = currentTime; startingTime = currentTime; opStack = d.opStack
                                                       allocatedTypes = bci.allocatedTypes; lengths = bci.lengths; lowerBounds = bci.lowerBounds}

                let initialCilState = cilState.MakeEmpty d.u modifiedState
                if cfg.offsetsDemandingCall.ContainsKey offset then
                    let opCode, calledMethod = cfg.offsetsDemandingCall.[offset]
                    let callSite = {sourceMethod = cfg.methodBase; offset = offset; calledMethod = calledMethod; opCode = opCode}
                    let this, args, (cilState' : cilState) = executeSeparatedOpCode ilintptr cfg opCode initialCilState
                    let symbolicOpStack = makeSymbolicOpStack currentTime cilState'.state.opStack
                    let dstVertex, newVertices = createVertexIfNeeded block (fun ip -> Vertex.CreateVertex cfg.methodBase (ip2Offset ip)) symbolicOpStack d vertices
                    let stateWithArgsOnFrame = ilintptr.ReduceFunctionSignature cilState'.state calledMethod this (Specified args) false id
                    let newCurrentTime = VectorTime.max currentTime stateWithArgsOnFrame.currentTime
                    let numberToDrop = List.length args + if Option.isNone this || opCode = OpCodes.Newobj then 0 else 1
                    addEdge <| CallEdge(srcVertex, dstVertex, callSite, stateWithArgsOnFrame, numberToDrop)

                    let newD = { d with u = d.v; srcVertex = dstVertex; uOut = d.vOut; opStack = symbolicOpStack }


                    let q, used = renewQ cfg d.v [newD] (q, used)
                    let newReachable = reachableConcreteInfo.append (dstVertex.Ip.Offset()) stateWithArgsOnFrame reachable
                    if not <| PriorityQueue.isEmpty q then bypass q used newReachable newVertices newCurrentTime
                else
                    let newStates = ilintptr.ExecuteAllInstructions cfg initialCilState
                    let goodStates = newStates |> List.filter (fun (cilState : cilState) -> cilState.isCompleted && not cilState.HasException && cilState.ip = d.v)
                    let erroredStates = newStates |> List.filter (fun (cilState : cilState) -> cilState.HasException)
                    srcVertex.AddErroredStates erroredStates
                    let incompleteStates = newStates |> List.filter (fun (cilState : cilState) -> not <| cilState.isCompleted && not <| cilState.HasException)

                    let addEdgeToCFA = addStepEdge d (createVertexIfNeeded block (fun ip -> Vertex.CreateVertex cfg.methodBase (ip2Offset ip)))
                    let newData, newReachable, newTime, newVertices = goodStates |> List.fold addEdgeToCFA ([], reachable, currentTime, vertices)

                    let q, used = renewQ cfg d.v newData (q, used)
                    if not <| PriorityQueue.isEmpty q then bypass q used newReachable newVertices newTime

            let offset = block.entryPoint.Ip.Offset()
            let firstData = { u = block.entryPoint.Ip; srcVertex = block.entryPoint; uOut = cfg.dfsOut.[offset]
                              v = block.entryPoint.Ip; vOut = 0; minSCCs = 0; opStack = initialState.opStack }

            let q, used = renewQ cfg block.entryPoint.Ip [firstData] (PriorityQueue.empty false, PersistentSet.empty)
            let reachable = reachableConcreteInfo.empty |> reachableConcreteInfo.append offset initialState
            let vertices = PersistentDict.empty
                           |> PersistentDict.add (block.entryPoint.Ip, initialState.opStack) block.entryPoint.Id
                           |> PersistentDict.add (block.exitVertex.Ip, block.exitVertex.OpStack) block.exitVertex.Id
            bypass q used reachable vertices initialState.currentTime
//        and addEdgesToFinallyBlocks (ilintptr : ILInterpreter) used (cfa : cfa) emptyEffect (srcVertex : Vertex) (dstVertex : Vertex) =
//            let cfg = cfa.cfg
//            let method = cfg.methodBase
//            let ehcs = method.GetMethodBody().ExceptionHandlingClauses
//                       |> Seq.filter Instruction.isFinallyClause
//                       |> Seq.filter (Instruction.shouldExecuteFinallyClause srcVertex.Ip dstVertex.Ip)
//                       |> Seq.sortWith (fun ehc1 ehc2 -> ehc1.HandlerOffset - ehc2.HandlerOffset)
//            let chainSequentialFinallyBlocks prevVertex (ehc : ExceptionHandlingClause) =
//                let finallyBlock = cfa.FindOrCreateFinallyHandler ehc
//                addEdge <| StepEdge(prevVertex, finallyBlock.entryPoint, emptyEffect)
//                computeCFAForBlock ilintptr emptyEffect used cfa finallyBlock
//                finallyBlock.exitVertex
//            let lastVertex = ehcs |> Seq.fold chainSequentialFinallyBlocks srcVertex
//            addEdge <| StepEdge (lastVertex, dstVertex, emptyEffect)

        let computeCFA (ilintptr : ILInterpreter) (ilmm: ILMethodMetadata) : cfa =
            let methodBase = ilmm.methodBase
            match alreadyComputedCFAs.ContainsKey methodBase with
            | true -> alreadyComputedCFAs.[methodBase]
            | _ ->
                let initialState, this, _ = ilintptr.FormInitialStateWithoutStatics ilmm
                Prelude.releaseAssert(Map.isEmpty initialState.callSiteResults && Option.isNone initialState.returnRegister)

//                Logger.printLog Logger.Trace "emptyState for method %O = %O" methodBase emptyState
                let cfg = CFG.build methodBase
                let cfa = createEmptyCFA cfg methodBase

                computeCFAForBlock ilintptr initialState cfa cfa.body
                alreadyComputedCFAs.[methodBase] <- cfa
                Logger.printLog Logger.Trace "Computed cfa: %O" cfa
                cfa

type StepInterpreter() =
    inherit ILInterpreter()
    let visitedVertices : persistent<Map<CFA.Vertex, uint32>> =
        let r = new persistent<_>(always Map.empty, id) in r.Reset(); r
    override x.ReproduceEffect codeLoc state k = x.ExploreAndCompose codeLoc state k
    override x.CreateInstance exceptionType arguments state : state list =
        let error = Nop
        {state with exceptionsRegister = Unhandled error} |> List.singleton
    member x.ForwardExploration (cfa : CFA.cfa) codeLoc initialState (k : (term * state) list -> 'a) =
        let k =
            visitedVertices.Save()
            let k x = visitedVertices.Restore(); k x
            k

        let maxBorder = 50u
        let used (vertex : CFA.Vertex) =
            if vertex.IsMethodExitVertex then true
            elif visitedVertices.Value.ContainsKey vertex then
                visitedVertices.Value.[vertex] >= maxBorder
            else visitedVertices.Mutate (Map.add vertex 1u visitedVertices.Value)
                 false

        let visit vertex =
            match visitedVertices.Value.ContainsKey vertex with
            | true ->
                let cnt = Map.find vertex visitedVertices.Value
                visitedVertices.Mutate (Map.add vertex (cnt + 1u) visitedVertices.Value)
            | _ -> visitedVertices.Mutate (Map.add vertex 1u visitedVertices.Value)
        let rec dfs lvl (vertex : CFA.Vertex) =
            if used vertex then ()
            else
                visit vertex
                let edges = vertex.OutgoingEdges
                let paths : path list = vertex.Paths.OfLevel lvl
                let newDsts = edges |> Seq.fold (fun acc (edge : CFA.Edge) ->
                    let propagated = Seq.map edge.PropagatePath paths |> Seq.fold (||) false
                    if propagated then (edge.Dst :: acc) else acc) []
                List.iter (dfs (lvl + 1u)) newDsts
        cfa.body.entryPoint.Paths.Add {lvl = 0u; state = initialState}
        Logger.trace "starting Forward exploration for %O" codeLoc
        dfs 0u cfa.body.entryPoint
        let resultStates = List.init (maxBorder |> int) (fun lvl -> cfa.body.exitVertex.Paths.OfLevel (lvl |> uint32) |> List.ofSeq)
                         |> List.concat
                         |> List.map (fun (path : path) ->
                             let state = path.state
                             match state.returnRegister with
                             | None -> Nop, state
                             | Some res -> res, state)
        if List.length resultStates = 0 then internalfailf "No states were obtained. Most likely such a situation is a bug. Check it!"
        k resultStates

    override x.Invoke codeLoc =
        match codeLoc with
        | :? ILMethodMetadata as ilmm ->
            CFA.configureInterpreter x

            try
                let cfa = CFA.cfaBuilder.computeCFA x ilmm
                x.ForwardExploration cfa codeLoc
            with
            | :? InsufficientInformationException -> base.Invoke codeLoc

        | _ -> internalfail "unhandled ICodeLocation instance"

