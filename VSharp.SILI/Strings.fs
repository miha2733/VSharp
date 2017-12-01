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

    let internal getKeyOfString mtd state term k =
        deref mtd state term
        ||> Common.unionHandler k

    let (|ConcreteStringArray|_|) = function
        | Array({term = Concrete(one, _)}, {term = Concrete (length, _)}, lower, [_, DefaultInstantiator(_, termType)], contents, _, ArrayType (typex, ConcreteDimension 1))
            when
                let zero = MakeZeroAddress Metadata.empty in
                one :?> int = 1 && lower.[zero].value = zero && termType = Numeric typedefof<char> && typex = Numeric typedefof<char> ->
              Some(ConcreteStringArray(unbox length, contents))
        | _ -> None
