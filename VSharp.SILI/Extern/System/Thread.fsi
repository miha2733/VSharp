namespace VSharp.System

open global.System
open VSharp

// --------------------------------------------------------------

module Thread =

    [<Implements("System.AppDomain System.Threading.Thread.GetFastDomainInternal()")>]
    val GetFastDomainInternal : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.AppDomain System.Threading.Thread.GetDomainInternal()")>]
    val GetDomainInternal : State.state -> Term list -> StatementResult * State.state
