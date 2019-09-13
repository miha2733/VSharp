namespace VSharp

[<CustomEquality;NoComparison>]
type public commonMemoryCell<'a, 'fql, 'typ> when 'a : equality =
    { key : 'a; FQL : 'fql option; typ : 'typ }  // Key * Fully qualified location * termType
    override x.GetHashCode() = x.key.GetHashCode()
    override x.Equals(o) =
        match o with
        | :? commonMemoryCell<'a, 'fql, 'typ> as other -> x.key.Equals(other.key)
        | _ -> false
