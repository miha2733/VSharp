namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.String -------------------------------

module internal String =

    let ctorOfCharArray (state : State.state) (args : Term list) =
        applyToFst Return <| ExternSDK.Strings.ctorOfCharArray state (args.Head) (args.Item(1))

    let InternalMarvin32HashString (state : State.state) (args : Term list) =
        applyToFst Return <| ExternSDK.Strings.getHashCode state (args.Head) in

    let IsStringInterned (state : State.state) (args : Term list) =
        applyToFst Return <| ExternSDK.Strings.stringIsInterned state (args.Item(1))

    let InternalIsInterned (state : State.state) (args : Term list) =
        applyToFst Return <| ExternSDK.Strings.stringIsInterned state (args.Head)

    let InternString (state : State.state) (args : Term list) =
        applyToFst Return <| ExternSDK.Strings.internString state (args.Item(1)) in
