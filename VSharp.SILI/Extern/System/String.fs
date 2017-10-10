namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.String -------------------------------

module String =

    let ctorOfCharArray (state : State.state) (args : Term list) =
        Return <| __notImplemented__(), state

    let InternalMarvin32HashString (state : State.state) (args : Term list) =
        (Return <| ExternSDK.Strings.getHashCode state (args.Head), state)

    let IsStringInterned (state : State.state) (args : Term list) =
        (Return <| ExternSDK.Strings.stringIsInterned state (args.Item(1)), state)
