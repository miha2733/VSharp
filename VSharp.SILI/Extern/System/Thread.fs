namespace VSharp.System

open global.System
open VSharp

// --------------------------------------------------------------

module Thread =

    let internal GetFastDomainInternal (state : State.state) (args : Term list) =
        (Return <| ExternSDK.Thread.getDomainInternal (Memory.tick()) (SymbolicConstantSource()) "AppDomain" (PointerType Void), state)

    let internal GetDomainInternal (state : State.state) (args : Term list) =
        (Return <| ExternSDK.Thread.getDomainInternal (Memory.tick()) (SymbolicConstantSource()) "AppDomain" (PointerType Void), state)
