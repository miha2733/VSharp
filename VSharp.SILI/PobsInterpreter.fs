namespace VSharp.Interpreter.IL

open System.Collections.Generic
open System.Reflection
open System.Reflection.Emit
open System.Text
open FSharpx.Collections

open VSharp
open VSharp.Core
open ipOperations
open CilStateOperations
open Instruction

type public PobsInterpreter(searcher : INewSearcher) =
    inherit ExplorerBase()
    let infty = System.Int32.MaxValue
    let GlobalBound = 20
    let initialOpStackSize = 0u
    let qFront = List<cilState>()
    let qBack = List<pob * cilState>()
    let transition = Dictionary<ip, cilState list>()
    let witnesses = Dictionary<pob, cilState list>()
    let blockedLocs = Dictionary<pob, ip list>()
    let possiblePobsLocs = List<ip>()
    let sPobs = Dictionary<cilState, pob list>()
    let mutable curLvl = 0

    let mainPobs = List<pob>()
    let currentPobs = List<pob>()
    let answeredPobs = Dictionary<pob, pobStatus>()
    let parents = Dictionary<pob, pob>()
    let canReach state (loc : ip) (blockedLocs : ip list) =
        //TODO: use CFG-reachability analysis
        searcher.CanReach (state, loc, blockedLocs)
    let addToDictionaryWithListValue (d : Dictionary<'a, 'b list>) k elem =
        let v = d.GetValueOrDefault(k, [])
        d.[k] <- (elem :: v)

    let removeFromDictionaryWithListValue (d : Dictionary<'a, 'b list>) k elem =
        let v = d.GetValueOrDefault(k, [])
        let v' = List.filter (fun x -> x <> elem) v
        d.[k] <- v'

    let removeFromGlobalVars (s : cilState, p : pob) =
        removeFromDictionaryWithListValue sPobs s p
        removeFromDictionaryWithListValue witnesses p s

    let addWitness(s : cilState, p : pob) =
        let sLvl = levelToInt s.level
        try
            if sLvl <= p.lvl && canReach s p.loc blockedLocs.[p] then
                addToDictionaryWithListValue witnesses p s
                addToDictionaryWithListValue sPobs s p
        with
        | :? KeyNotFoundException -> __notImplemented__()
    let rec blockWitness(s', p') =
        removeFromGlobalVars(s', p')
        match List.exists (fun (s : cilState) -> s.startingIP = s'.startingIP) witnesses.[p'] with
        | true -> ()
        | false ->
            addToDictionaryWithListValue blockedLocs p' s'.startingIP
            List.iter (fun (s : cilState) ->
                if not <| canReach s p'.loc blockedLocs.[p'] then blockWitness(s, p')) witnesses.[p']

    let addPob(parent : pob, child : pob) =
        assert(parents.ContainsKey(child) |> not)
        parents.Add(child, parent)
        currentPobs.Add child
        Seq.iter (fun s -> addWitness(s, child)) qFront
        Seq.iter (fun s -> qBack.Add(child, s)) transition.[child.loc]

    let rec answerYes (s' : cilState, p' : pob) =
        if Seq.contains p' mainPobs then mainPobs.Remove(p') |> ignore
        if answeredPobs.ContainsKey p' |> not then answeredPobs.Add(p', Witnessed s')
        else answeredPobs.[p'] <- Witnessed s'
        removeFromGlobalVars(s', p')
        if parents.ContainsKey p' then answerYes(s', parents.[p'])

    member x.MakeCilStateForIp (ip : ip) =
        let m = methodOf ip
        let cilStates = x.FormInitialState (x.MakeMethodIdentifier m)
        match cilStates with
        | [cilState] -> {cilState with startingIP = ip} |> withIpStack [ip]
        | _ -> __notImplemented__()

    //NOTE: Must be called for ip with empty evaluation stack!
    member x.Start (loc : ip) =
        possiblePobsLocs.Add(loc)
        let start = x.MakeCilStateForIp loc
        qFront.Add(start)
        Seq.iter (fun p -> addWitness(start, p)) currentPobs

    member x.Forward (s : cilState) =
        assert(sPobs.ContainsKey s)
//        if (not <| sPobs.ContainsKey s) then ()
        let removed = qFront.Remove(s) in assert(removed)
        let goodStates, incompleteStates, errors = ILInterpreter(x).ExecuteOnlyOneInstruction s
        qFront.AddRange (goodStates @ incompleteStates @ errors)
        goodStates |> List.iter (fun (s' : cilState) ->
            if not <| sPobs.ContainsKey(s') then sPobs.Add(s', [])
            let ip' = currentIp s'
            if Seq.contains ip' possiblePobsLocs then addToDictionaryWithListValue transition ip' s'
            sPobs.[s] |> List.iter (fun p ->
                addWitness(s', p)
                assert(sPobs.ContainsKey s')
                if p.loc = currentIp s' then qBack.Add(p, s')                )
        )
        List.iter (fun (p : pob) -> if p.loc <> currentIp s then removeFromGlobalVars(s, p)) sPobs.[s]

    member x.OverApproximate(_ : ip, _ : int) : term = Terms.True

    member x.Backward (p' : pob, s' : cilState, EP : ip) =
        let removed = qBack.Remove(p',s') in assert(removed)
        assert(currentIp s' = p'.loc)
        let lvl = p'.lvl - (levelToInt s'.level)
        let fml = Memory.WLP s'.state p'.fml
        match Memory.IsSAT fml with
        | true when s'.startingIP = EP ->
            let removed = currentPobs.Remove(p') in assert(removed)
            let removed = witnesses.Remove(p') in assert(removed)
            let removed = blockedLocs.Remove(p') in assert(removed)
            qBack.RemoveAll(fun (p, _) -> p = p') |> ignore
            answerYes(s', p')
        | true ->
            let p = {loc = s'.startingIP; lvl = lvl; fml = fml}
            addPob(p', p)
        | false ->
            Logger.info "UNSAT for pob = %O and s' = %O" p' s'
    member x.PropDirSymExec (mainId : IFunctionIdentifier) (EP : ip) mainPobsList =
        List.iter (fun p -> mainPobs.Add p) mainPobsList
//        let clearStructures () =
//            currentPobs.Clear()
//            witnesses.Clear()
        let createPobs () =
            mainPobs |> Seq.iter (fun (mp : pob) ->
                let p = {mp with lvl = curLvl}
                blockedLocs.Add(p, [])
                witnesses.Add(p, [])
                parents.Add(p, mp)
                currentPobs.Add p)
        while mainPobs.Count > 0 && curLvl <= GlobalBound do
//            clearStructures()
            createPobs()
//            let pobs : List<pob> = List(createPobs ())
            while currentPobs.Count > 0 do
                match searcher.ChooseAction(List.ofSeq qFront, List.ofSeq qBack, mainId) with
                | Stop -> currentPobs.Clear()
                | Start loc -> x.Start(loc)
                | GoForward s ->
                    Logger.info "GoForward: ip = %O" (currentIp s)
                    x.Forward(s)
                | GoBackward(p', s') -> x.Backward(p',s', EP)
            curLvl <- curLvl + 1

//    member x.FindPobs (t : System.Type) : pob list =
//        let methods = t.GetMethods(Reflection.staticBindingFlags) |> List.ofSeq
//        let cfgs = methods |> List.map CFG.build
//        let rec bypassCfg (cfg : cfg) (pobs : pob list, used : codeLocation list) (v : offset)  =
//            let m = cfg.methodBase
//            let loc = {offset = v; method = m}
//            if List.contains loc used then (pobs, used)
//            else
//                let used = loc :: used
//                let opCode = parseInstruction m v
//                let pobs =
//                    if opCode <> OpCodes.Throw then pobs
//                    else {loc = Instruction(v, m); lvl = infty; fml = Terms.True} :: pobs
//                Seq.fold (bypassCfg cfg) (pobs, used) (cfg.graph.[v])
//        //TODO: what about EHC?
//        let pobs, _ = Seq.fold (fun acc cfg -> bypassCfg cfg acc 0) ([], []) cfgs
//        pobs

    member x.ClearStructures () =
        mainPobs.Clear()
        qFront.Clear()
        qBack.Clear()
        transition.Clear()
        witnesses.Clear()
        blockedLocs.Clear()
        possiblePobsLocs.Clear()
        sPobs.Clear()
        mainPobs.Clear()
        currentPobs.Clear()
        answeredPobs.Clear()
        parents.Clear()
    override x.Invoke _ _ _ = __notImplemented__()
    override x.AnswerPobs entryMethodId codeLocations k =
        x.ClearStructures()
        let mainPobs = List.map (fun (loc : codeLocation) ->
            {loc = Instruction(loc.offset, loc.method); lvl = infty; fml = Terms.True}) codeLocations
        List.iter (fun p -> answeredPobs.Add(p, Unknown)) mainPobs
        let EP = Instruction(0, entryMethodId.Method)
        curLvl <- 10
        x.PropDirSymExec entryMethodId EP mainPobs
        let showResultFor (mp : pob) =
            match answeredPobs.[mp] with
            | Unreachable -> Logger.info "NO: MainPob = %O" mp
            | Witnessed s -> Logger.info "YES: MainPob = %O, witness = %O" mp s
            | Unknown -> Logger.info "Unknown: MainPoob = %O" mp
        List.iter showResultFor mainPobs
        let addLocationStatus (acc : Dictionary<codeLocation, string>) (loc : codeLocation) =
            let pob = {loc = Instruction(loc.offset, loc.method); lvl = infty; fml = Terms.True}
            let result =
                match answeredPobs.[pob] with
                | Witnessed _ -> "Witnessed"
                | status -> status.ToString()
            acc.Add(loc, result)
            acc
        let result = codeLocations |> Seq.fold addLocationStatus (new Dictionary<codeLocation, string>())
        k result

    override x.MakeMethodIdentifier m = { methodBase = m } :> IMethodIdentifier

type TargetedSearcher(entryMethod : MethodBase) =
    let reachableLocations = Dictionary<codeLocation, codeLocation list>()
    let reachableMethods = Dictionary<codeLocation, MethodBase list>()
    let methodsReachability = Dictionary<MethodBase, MethodBase list>()
//    let reversedMethodsReachability = Dictionary<MethodBase, MethodBase list>()
    let methodsReachabilityTransitiveClosure = Dictionary<MethodBase, MethodBase list>()

    let appendReachableInfo (cfg : cfg) (reachableLocsForSCC : HashSet<codeLocation>) (reachableMethodsForSCC : HashSet<MethodBase>) (current : offset) =
        let currentLoc = {offset = current; method = cfg.methodBase}
        reachableLocsForSCC.Add(currentLoc) |> ignore
        if cfg.offsetsDemandingCall.ContainsKey current then
           let _, calledMethod = cfg.offsetsDemandingCall.[current]
           if calledMethod.DeclaringType.Assembly = entryMethod.DeclaringType.Assembly then
            reachableMethodsForSCC.Add(calledMethod) |> ignore

        let helper (target : offset) =
            let loc = {offset = target; method = cfg.methodBase}
            if not <| reachableLocations.ContainsKey loc then ()
            List.iter (reachableLocsForSCC.Add >> ignore) reachableLocations.[loc]
            List.iter (reachableMethodsForSCC.Add >> ignore) reachableMethods.[loc]
        let targets = cfg.graph.[current]
        Seq.iter helper targets

    let commitReachableInfo (cfg : cfg) (reachableLocsForSCC : HashSet<codeLocation>) (reachableMethodsForSCC : HashSet<MethodBase>) (current : offset) =
        let currentLoc = {offset = current; method = cfg.methodBase}
        reachableLocations.[currentLoc] <-  List.ofSeq (reachableLocsForSCC)
        reachableMethods.[currentLoc] <- List.ofSeq (reachableMethodsForSCC)

    let initReachableInfo (cfg : cfg) (current : offset) =
        let currentLoc = {offset = current; method = cfg.methodBase}
        reachableLocations.Add(currentLoc, [])
        reachableMethods.Add(currentLoc, [])
    let buildReachabilityInfo (currentMethod : MethodBase) : MethodBase list =
        let cfg = CFG.build currentMethod

        let rec dfsSCC (usedSCC : int list) (v : offset) : int list =
            let currentSCC = cfg.sccOut.[v]
            if List.contains currentSCC usedSCC then usedSCC
            else
                let usedSCC = currentSCC :: usedSCC
                let currentSCCOffsets = Seq.filter (fun offset -> currentSCC = cfg.sccOut.[offset]) cfg.sortedOffsets
                let newUsed = Seq.fold (fun acc u1 -> Seq.fold (fun acc u2 -> dfsSCC acc u2) acc cfg.graph.[u1]) usedSCC currentSCCOffsets
                let reachableLocsForSCC = HashSet<codeLocation>()
                let reachableMethodsForSCC = HashSet<MethodBase>()
                Seq.iter (initReachableInfo cfg) currentSCCOffsets
                Seq.iter (appendReachableInfo cfg reachableLocsForSCC reachableMethodsForSCC) currentSCCOffsets
                Seq.iter (commitReachableInfo cfg reachableLocsForSCC reachableMethodsForSCC) currentSCCOffsets
                newUsed
        let _ = dfsSCC [] 0 //TODO: what about EHC?
        reachableMethods.[{offset = 0; method = currentMethod}]

    let addCall (current : MethodBase) (calledMethods : MethodBase list) =
        let add m (ms : MethodBase list) (d : Dictionary<_, MethodBase list >) =
            if d.ContainsKey m then d.[m] <- ms @ d.[m]
            else d.Add(m, ms)

        add current calledMethods methodsReachability
//        List.iter (fun k -> add k [current] reversedMethodsReachability) calledMethods

    let buildReachability () =
        let rec exit processedMethods = function
                | [] -> ()
                | m :: q' -> findFixPoint (processedMethods, q') m
        and findFixPoint (processedMethods : MethodBase list, methodsQueue : MethodBase list) (current : MethodBase) =
            if List.contains current processedMethods then exit processedMethods methodsQueue
            else
                let processedMethods = current :: processedMethods
                let calledMethods = buildReachabilityInfo current
                addCall current calledMethods

                exit processedMethods (methodsQueue @ calledMethods)
        findFixPoint ([],[]) entryMethod

    let makeTransitiveClosure () =
        let findReachableMethodsForMethod (current : MethodBase) =
            Logger.info "Iterating for %O" current
            methodsReachabilityTransitiveClosure.Add(current, [])
            let used = HashSet<MethodBase>()
            let rec dfs (v : MethodBase) =
                if used.Contains v then ()
                else
                    used.Add(v) |> ignore
                    methodsReachabilityTransitiveClosure.[current] <- v :: methodsReachability.[current]
                    List.iter dfs (methodsReachability.[v])
            List.iter dfs methodsReachability.[current]
        Seq.iter findReachableMethodsForMethod (methodsReachability.Keys)
//        let tOut = Dictionary<MethodBase, int>()
//        let used = HashSet<MethodBase>()
//        let rec dfs (methods : MethodBase list, t) (current : MethodBase) =
//            if used.Contains current then methods, t
//            else
//                used.Add(current) |> ignore
//                let methods, t = List.fold (fun acc m -> dfs acc m) (methods, t) methodsReachability.[current]
//                tOut.Add(current, t) |> ignore
//                let usedMethods = current :: usedMethods
//                newUsed, t + 1
//
//        dfs ([],0) entryMethod

//        let rec exit processedMethods = function
//                | [] -> ()
//                | m :: q' -> findFixPoint (processedMethods, q') m
//        and findFixPoint (processedMethods : MethodBase list, methodsQueue : MethodBase list) (current : MethodBase) =
//            if List.contains current processedMethods then exit processedMethods methodsQueue
//            else
//                let processedMethods = current :: processedMethods
//                let calledMethods = buildReachabilityInfo current
//                addCall current calledMethods
//                exit processedMethods (methodsQueue @ calledMethods)
//        findFixPoint ([],[]) entryMethod

    let print () =
        Logger.info "Calculated CFG Reachability\n"
        methodsReachabilityTransitiveClosure |> Seq.iter (fun kvp ->
            let value = List.fold (fun (sb : StringBuilder) m -> sb.AppendFormat("{0}; ", m.ToString())) (StringBuilder()) kvp.Value
//            List.jo
            Logger.info "key = %O; Value = %s" kvp.Key (value.ToString()))

    do
        buildReachability ()
        makeTransitiveClosure ()
        print ()
    interface INewSearcher with
        override x.CanReach(cilState,ip : ip, blocked : ip list) = true
        override x.ChooseAction(_,_,_) = Stop

//        let cfg = CFG.build currentMethod
//        __notImplemented__()

//        let appendComponent acc newOffset =
//            cfg.sccOut.[newOffset] :: acc
//
//        let addReachabilityInfo s t =
//            if reachableLocations.ContainsKey s |> not then reachableLocations.Add(s, [t])
//            else
//                let old = reachableLocations.[s]
//                reachableLocations.[s] <- t :: old

//        let rec dfsSCC (cfg : cfg) (usedSCC : int list) (v : offset) : int list =
//            let currentSCC = cfg.sccOut.[v]
//            if List.contains currentSCC usedSCC then usedSCC
//            else
//                let usedSCC = currentSCC :: usedSCC
//                let currentSCCOffsets = Seq.filter (fun offset -> currentSCC = cfg.sccOut.[offset]) cfg.sortedOffsets
//                let newUsed = Seq.fold (fun acc u1 -> Seq.fold (fun acc u2 -> dfsSCC cfg acc u2) acc cfg.graph.[u1]) usedSCC currentSCCOffsets
//                let reachableLocsForSCC = HashSet<codeLocation>()
//                Seq.iter (appendReachableLocations cfg reachableLocsForSCC) currentSCCOffsets
//                Seq.iter (commitReachableLocations cfg reachableLocsForSCC) currentSCCOffsets
//                newUsed


//    override x.PickNext fq =
//        let canBePropagated (s : cilState) =
//            not (isIIEState s || isUnhandledError s) && isExecutable s && not (x.Used s)
//        let states = fq |> List.filter canBePropagated
//        match states with
//        | _ :: _ -> List.last states |> Some
//        | [] -> None


//type TargetedSearcher() =
//    interface INewSearcher with
//        override x.CanReach(_,_,_) = true
//        override x.ChooseAction(_,_,_) = Stop
