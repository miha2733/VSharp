namespace VSharp

open JetBrains.Decompiler.Ast
open Memory
open Arrays

module internal Strings =

    let makeString metadata (str : string) =
        let fields =
            let time = tick() in
            let length = String.length str in
            let stringTermLength = Concrete length lengthTermType metadata in
            let arraySource = (str + "\000").ToCharArray() in
            let valMaker i = MakeNumber arraySource.[i] metadata in
            let keyMaker i mtd = makeIntegerArray metadata (fun _ -> MakeNumber i mtd) 1 in
            let array = makeLinearConreteArray metadata keyMaker valMaker (length + 1) (Numeric typedefof<char>) in
            Heap.ofSeq (seq [ MakeStringKey "System.String.m_StringLength", { value = stringTermLength; created = time; modified = time };
                              MakeStringKey "System.String.m_FirstChar", { value = array; created = time; modified = time } ])
        in
        Struct fields VSharp.String metadata

    let internal simplifyEquality mtd x y =
        match x.term, y.term with
        | Concrete(x, String), Concrete(y, String) -> MakeBool ((x :?> string) = (y :?> string)) mtd
        | _ -> __notImplemented__()

    let internal simplifyConcatenation mtd x y =
        match x.term, y.term with
        | Concrete(xval, _), Concrete(yval, _) ->
            let mtd' = Metadata.combine3 mtd x.metadata y.metadata in
            MakeConcreteString (VSharp.CSharpUtils.Calculator.Add(xval, yval, typedefof<string>) :?> string) mtd'
        | _ -> Terms.MakeBinary OperationType.Add x y false String mtd

    let internal simplifyOperation mtd op x y =
        match op with
        | OperationType.Add -> simplifyConcatenation mtd x y
        | OperationType.Equal -> simplifyEquality mtd x y
        | OperationType.NotEqual -> !! (simplifyEquality mtd x y)
        | OperationType.NullCoalescing
        | _ -> __notImplemented__()

    let internal isStringOperation op t1 t2 =
        Types.IsString t1 && Types.IsString t2 &&
        match op with
        | OperationType.Add
        | OperationType.Equal
        | OperationType.NotEqual
        | OperationType.NullCoalescing -> true
        | _ -> false
