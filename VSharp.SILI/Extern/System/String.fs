namespace VSharp.System

open global.System
open VSharp
open VSharp.Core

// ------------------------------- mscorelib.System.Array -------------------------------

module internal String =

<<<<<<< HEAD
    let ctorOfCharArray (state : State.state) (args : Term list) =
        applyToFst Return <| ExternSDK.Strings.ctorOfCharArray state (args.Head) (args.Item(1))

    let InternalMarvin32HashString (state : State.state) (args : Term list) =
        applyToFst Return <| ExternSDK.Strings.getHashCode state (args.Head)

    let IsStringInterned (state : State.state) (args : Term list) =
        applyToFst Return <| ExternSDK.Strings.stringIsInterned state (args.Item(1))

    let InternalIsInterned (state : State.state) (args : Term list) =
        applyToFst Return <| ExternSDK.Strings.stringIsInterned state (args.Head)

    let InternString (state : State.state) (args : Term list) =
        applyToFst Return <| ExternSDK.Strings.internString state (args.Item(1))

    let InternalIntern (state : State.state) (args : Term list) =
        applyToFst Return <| ExternSDK.Strings.internString state (args.Head)
=======
    let ctorOfCharArray (state : state) (args : term list) =
        Return <| __notImplemented__(), state
>>>>>>> 23c11e2bcd2e4e247a2548c3448ddd9ea4bbb335
