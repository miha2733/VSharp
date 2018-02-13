namespace VSharp.System

open global.System
open VSharp
open VSharp.Core
open VSharp.Interpreter

// ------------------------------- mscorelib.System.Array -------------------------------

module internal String =

    [<Implements("System.Void System.String..ctor(this, System.Char[])")>]
<<<<<<< HEAD
    val ctorOfCharArray : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.Int32 System.String.InternalMarvin32HashString(System.String, System.Int32, System.Int64)")>]
    val InternalMarvin32HashString : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.String System.AppDomain.IsStringInterned(this, System.String)")>]
    val IsStringInterned : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.String System.AppDomain.GetOrInternString(this, System.String)")>]
    val InternString : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.String System.String.InternalIsInterned(System.String)")>]
    val InternalIsInterned : State.state -> Term list -> StatementResult * State.state

    [<Implements("System.String System.String.InternalIntern(System.String)")>]
    val InternalIntern : State.state -> Term list -> StatementResult * State.state
=======
    val ctorOfCharArray : state -> term list -> statementResult * state
>>>>>>> 23c11e2bcd2e4e247a2548c3448ddd9ea4bbb335
