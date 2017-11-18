namespace VSharp.System

open global.System
open VSharp

// --------------------------------------------------------------

module Thread =

    [<Implements("System.AppDomain System.Threading.Thread.GetFastDomainInternal()")>]
    val internal GetFastDomainInternal : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.AppDomain System.Threading.Thread.GetDomainInternal()")>]
    val internal GetDomainInternal : State.state -> Term list -> StatementResult * State.state
