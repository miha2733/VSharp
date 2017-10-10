namespace VSharp.System

open global.System
open VSharp

// --------------------------------------------------------------

module Thread =

    let GetFastDomainInternal (state : State.state) (args : Term list) =
        (Return <| ExternSDK.Thread.getDomainInternal (Memory.tick()) (SymbolicConstantSource()) "crutch" (PointerType Void), state)

    let GetDomainInternal (state : State.state) (args : Term list) =
        (Return <| ExternSDK.Thread.getDomainInternal (Memory.tick()) (SymbolicConstantSource()) "crutch" (PointerType Void), state)
