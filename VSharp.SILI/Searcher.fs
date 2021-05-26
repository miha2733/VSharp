namespace VSharp.Interpreter.IL

open System.Collections.Generic
open VSharp
open CilStateOperations
open VSharp.Core



type IndexedQueue() =
    let q = List<cilState>()
//    let isRecursiveEffect (s : cilState) =
//        let isEffect = (Seq.last s.state.frames).isEffect
//        if isEffect then
//            let effectsMethod = (Seq.last s.state.frames).func.Method
//            match currentIp s with
//            | {label = Instruction offset; method = m} when Instruction.isDemandingCallOpCode (Instruction.parseInstruction m offset)->
//                let callSite = Instruction.parseCallSite m offset
//                callSite.calledMethod.Equals(effectsMethod)
//            | _ -> false
//        else false
    member x.Add (s : cilState) =
        if List.length s.ipStack <> Memory.CallStackSize s.state then __unreachable__() // TODO: change to assert; this falls in factAgain #do
        q.Add s

    member x.Remove s =
        let removed = q.Remove s
        if not removed then Logger.trace "CilState was not removed from IndexedQueue:\n%O" s
    member x.GetStates () = List.ofSeq q

type SearchDirection =
    | Stop
    | Start of ip
    | GoForward of cilState
    | GoBackward of pob * cilState

type INewSearcher =
    abstract member ChooseAction : list<cilState> * list<pob * cilState> * IFunctionIdentifier -> SearchDirection

[<AbstractClass>]
type ForwardSearcher() = // TODO: max bound is needed, when we are in recursion, but when we go to one method many time -- it's okay #do
    let maxBound = 10u // 10u is caused by number of iterations for tests: Always18, FirstEvenGreaterThen7
    interface INewSearcher with
        override x.ChooseAction(fq, bq, mainId) =
            match fq, bq with
            | _, ps :: _ -> GoBackward ps
            | [], [] -> Start <| Instruction(0, mainId.Method)
            | _, [] ->
                match x.PickNext fq with
                | None -> Stop
                | Some s -> GoForward s

    abstract member PickNext : cilState list -> cilState option
    default x.PickNext (qf : cilState list) = None
    member x.Used (cilState : cilState) =
        match currentIp cilState with
        | Instruction(offset, m) ->
            let codeLocation = {offset = offset; method = m}
            match PersistentDict.tryFind cilState.level codeLocation with
            | Some current -> current >= maxBound
            | None -> false
        | _ -> false


type DummySearcher() =
    inherit ForwardSearcher() with
        override x.PickNext fq =
            let canBePropagated (s : cilState) =
                not (isIIEState s || isUnhandledError s) && isExecutable s && not <| x.Used s
            let states = fq |> List.filter canBePropagated
            match states with
            | x :: _ -> Some x
            | [] -> None

