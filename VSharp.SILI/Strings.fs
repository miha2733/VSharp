namespace VSharp

open JetBrains.Decompiler.Ast
open MemoryCell
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

    let internal getHashCode metadata state term =

        let contentIsConcrete xs =
            Cps.Seq.foldlk (fun a (key, cell) k ->
                match key.term, cell.value.term with
                | Array({term = Concrete(one, _)}, {term = Concrete (length, _)}, _, [_, DefaultInstantiator _], contents, _, ArrayType (_, ConcreteDimension 1)), Concrete(y, _)
                    when length :?> int = 1 && one :?> int = 1 ->
                        match Terms.term contents.[MakeZeroAddress Metadata.empty].value with
                        | Concrete(num, _) -> k <| Set.add (unbox num, unbox y) a
                        | _ -> None
                | _ -> None)
                Set.empty
                xs
                (fun x -> Some x)

        let fullArray length value xs : seq<int * char> =
            let indices : Set<int> = Set.map fst xs |> Set.difference (Set.ofSeq [0 .. length - 1])
            Seq.append <| Set.toSeq xs <| Seq.map (fun k -> k, value) indices

        getKeyOfString metadata state term (fun state ->
            Terms.term >> function
            | Struct(fields, String) ->
                match Terms.term fields.[MakeStringKey "System.String.m_FirstChar"].value with
                //Almost concrete string
                | ConcreteStringArray(length, contents) ->
                    Heap.toSeq contents
                    |> contentIsConcrete
                    |> function
                        //Concrete string
                        | Some set ->
                            let discardLast = Seq.rev >> Seq.tail >> Seq.rev in
                            let time = tick() in
                            fullArray length (unbox <| System.Activator.CreateInstance(typedefof<char>)) set
                            |> Seq.sortBy (fun (k, v) -> k)
                            |> discardLast
                            |> Seq.map snd
                            |> Seq.toArray
                            |> System.String
                            |> fun string -> MakeNumber (string.GetHashCode()) metadata, state
                        //Partially-symbolic string
                        //Can contain symbolic values
                        | None -> refToInt term, state
                //Full-symbolic string
                //Can be made by "makeSymbolicInstance" function
                | Array(_, _, _, _, _, _, _) -> refToInt term, state
                | _ -> internalfail "wrong string!"
            | _ -> internalfail "not a string!")
