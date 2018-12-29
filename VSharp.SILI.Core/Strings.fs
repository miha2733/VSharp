namespace VSharp.Core

open VSharp
open Arrays
open Types
open Common

module internal Strings =

    let makeStringOfFields metadata time length array =
        let fields = Heap.ofSeq (seq [ makeStringKey "System.String.m_StringLength", { value = length; created = time; modified = time; typ = lengthTermType };
                                       makeStringKey "System.String.m_FirstChar", { value = array; created = time; modified = time; typ = String } ])
        Struct metadata fields String

    let makeConcreteStringStruct metadata time (str : string) =
        let length = Concrete metadata str.Length lengthTermType
        let arraySource = (str + "\000").ToCharArray()
        let valMaker i = makeNumber arraySource.[i] metadata
        let keyMaker i mtd = makeIntegerArray metadata (fun _ -> makeNumber i mtd) 1
        let array = makeLinearConcreteArray metadata keyMaker valMaker (str.Length + 1) (Numeric typedefof<char>)
        makeStringOfFields metadata time length array

    let makeStringArray metadata time length instor (contents : symbolicHeap) elType =
        let arrLength = add metadata length <| makeNumber 1 metadata
        let indexLength = makeIntegerArray metadata (always <| length) 1
        let contentsWithZero = Heap.add indexLength { value = makeNumber '\000' metadata; created = time; modified = time; typ = char } contents
        makeArray metadata arrLength contentsWithZero instor elType

    let ctorOfCharArray metadata time = Merging.map (function
        | VectorT(length, instor, contents, Char) ->
            let stringArray = makeStringArray metadata time length instor contents Types.char
            makeStringOfFields metadata time length stringArray
        | t -> internalfailf "expected char array, but got %O" t)

    let length = Merging.map (term >> function
        | Struct(fields, StringType) -> fields.[makeStringKey "System.String.m_StringLength"].value
        | t -> internalfailf "expected string struct, but got %O" t)

    let (|ConcreteStringArray|_|) = function
        | VectorT (ConcreteT(length, _), _, contents, Char) -> Some(ConcreteStringArray(unbox length, contents))
        | _ -> None

    let private contentIsConcrete xs =
        Cps.Seq.foldlk (fun acc (key, cell) k ->
            match key, cell.value with
            | Index (ConcreteT(x, _)), ConcreteT(y, _) -> k <| (unbox x, unbox y) :: acc
            | _ -> None)
            List.empty xs Some

    let private complementArray length xs =
        let value = unbox <| System.Activator.CreateInstance(typedefof<char>)
        let indices = List.except (List.map fst xs) [0 .. length - 1]
        xs @ List.map (withSnd value) indices

    let private contentArrayToString xs =
        List.sortBy fst xs |> List.discardLast |> List.map snd |> List.toArray |> System.String

    let getHashCode metadata addr = Merging.map (term >> function
        | Struct(fields, StringType) ->
            match fields.[makeStringKey "System.String.m_FirstChar"].value with
            | ConcreteStringArray(length, contents) ->
                let content = contentIsConcrete <| Heap.toSeq contents
                Option.fold (fun _ xs -> complementArray length xs |> contentArrayToString |> hash |> makeNumber <| metadata) addr content
            | {term = Array _} -> addr
            | t -> internalfailf "expected char array, but got %O" t
        | t -> internalfailf "expected string struct, but got %O" t)

    //TODO: string comparasion

    let private simplifyConcreteEquality mtd _ state _ (xval:obj) (yval:obj) =
        makeBool ((xval :?> string) = (yval :?> string)) mtd, state

    let private simplifyStructEq mtd x y k =
        match x.term, y.term with
        | Struct(fieldsOfX, StringType), Struct(fieldsOfY, StringType) ->
            let str1Len = fieldsOfX.[makeStringKey "System.String.m_StringLength"].value
            let str2Len = fieldsOfY.[makeStringKey "System.String.m_StringLength"].value
            let str1Arr = fieldsOfX.[makeStringKey "System.String.m_FirstChar"].value
            let str2Arr = fieldsOfY.[makeStringKey "System.String.m_FirstChar"].value
            simplifyEqual mtd str1Len str2Len (fun lengthEq ->
            simplifyAnd mtd lengthEq (Arrays.equalsStringArrays mtd (__notImplemented__()) str1Arr str2Arr) k)
        | _ -> __notImplemented__()

    let rec simplifyEquality mtd x y k =
        simplifyGenericBinary "equality" State.empty x y (fst >> k)
            (simplifyConcreteBinary simplifyConcreteEquality mtd false Bool)
            (fun x y state k -> simplifyStructEq mtd x y (withSnd state >> k))
            (fun x y state k -> simplifyEquality mtd x y (withSnd state >> k))

    let private simplifyConcreteConcatenation mtd _ state _ xval yval =
        makeConcreteString (VSharp.CSharpUtils.Calculator.Add(xval, yval, typedefof<string>) :?> string) mtd, state

    let rec simplifyConcatenation mtd x y k =
        simplifyGenericBinary "concatenation" State.empty x y (fst >> k)
            (simplifyConcreteBinary simplifyConcreteConcatenation mtd false typedefof<string>)
            (fun x y state k -> k (Terms.makeBinary OperationType.Add x y false String mtd, state))
            (fun x y state k -> simplifyConcatenation mtd x y (withSnd state >> k))

    let simplifyOperation mtd op x y k =
        match op with
        | OperationType.Add -> simplifyConcatenation mtd x y k
        | OperationType.Equal -> simplifyEquality mtd x y k
        | OperationType.NotEqual -> simplifyEquality mtd x y ((!!) >> k)
        | _ -> __notImplemented__()

    let isStringOperation op t1 t2 =
        isString t1 && isString t2 &&
        match op with
        | OperationType.Add
        | OperationType.Equal
        | OperationType.NotEqual -> true
        | _ -> false
