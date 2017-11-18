namespace VSharp.System

open global.System
open VSharp

// ------------------------------- mscorelib.System.String -------------------------------

module internal String =

    [<Implements("System.Void System.String..ctor(this, System.Char[])")>]
    val ctorOfCharArray : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Int32 System.String.InternalMarvin32HashString(System.String, System.Int32, System.Int64)")>]
    val InternalMarvin32HashString : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.String System.AppDomain.IsStringInterned(this, System.String)")>]
    val IsStringInterned : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.String System.AppDomain.StringIntern(this, System.String)")>]
    val InternString : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.String System.String.InternalIsInterned(System.String)")>]
    val InternalIsInterned : State.state -> Term list -> StatementResult * State.state
