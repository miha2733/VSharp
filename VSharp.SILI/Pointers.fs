namespace VSharp

open JetBrains.Decompiler.Ast
open VSharp.Common
open Types

module internal Pointers =

    let internal simplifyStringKeyEquality simplifyArraysEq mtd x y =
        match x.term, y.term with
        | Concrete(x, StringType), Concrete(y, StringType) -> MakeBool ((x :?> string) = (y :?> string)) mtd
        | Struct(fieldsOfX, StringType), Struct(fieldsOfY, StringType) ->
            let stringLength1 = fieldsOfX.[MakeStringKey "System.String.m_StringLength"].value in
            let stringLength2 = fieldsOfY.[MakeStringKey "System.String.m_StringLength"].value in
            let string1Array  = fieldsOfX.[MakeStringKey "System.String.m_FirstChar"].value in
            let string2Array  = fieldsOfY.[MakeStringKey "System.String.m_FirstChar"].value in
            simplifyEqual mtd stringLength1 stringLength2 (fun lengthEq ->
            simplifyAnd mtd lengthEq (simplifyArraysEq mtd string1Array string2Array) id)
        | _ -> __notImplemented__()

    let internal locationEqual simplifyArraysEq mtd addr1 addr2 =
        match TypeOf addr1, TypeOf addr2 with
        | StringType, StringType -> simplifyStringKeyEquality simplifyArraysEq mtd addr1 addr2
        | Numeric _, Numeric _ -> Arithmetics.eq mtd addr1 addr2
        | ArrayType _, ArrayType _ -> Arrays.equalsArrayIndices mtd addr1 addr2 |> fst
        | _ -> __notImplemented__()

    let internal comparePath mtd path1 path2 =
        if List.length path1 <> List.length path2 then
            Terms.MakeFalse mtd
        else
            List.map2 (fun (x, _) (y, _) -> locationEqual (fun _ _ -> __unreachable__()) mtd x y) path1 path2 |> conjunction mtd

    let rec internal simplifyReferenceEquality mtd x y k =
        simplifyGenericBinary "reference comparison" State.empty x y (fst >> k)
            (fun _ _ _ _ -> __unreachable__())
            (fun x y s k ->
                let k = withSnd s >> k in
                match x.term, y.term with
                | _ when x = y -> MakeTrue mtd |> k
                | HeapRef(xpath, _, None), HeapRef(ypath, _, None) ->
                    comparePath mtd (NonEmptyList.toList xpath) (NonEmptyList.toList ypath) |> k
                | StackRef(key1, path1, None), StackRef(key2, path2, None) ->
                    MakeBool (key1 = key2) mtd &&& comparePath mtd path1 path2 |> k
                | StaticRef(key1, path1, None), StaticRef(key2, path2, None) ->
                    MakeBool (key1 = key2) mtd &&& comparePath mtd path1 path2 |> k
                | _ -> MakeFalse mtd |> k)
            (fun x y state k -> simplifyReferenceEquality mtd x y (withSnd state >> k))

    let internal isNull mtd ptr =
        simplifyReferenceEquality mtd ptr (MakeNullRef Null mtd) id

    let internal simplifyBinaryOperation metadata op state x y k =
        match op with
        | OperationType.Add
        | OperationType.Subtract -> __notImplemented__()
        | OperationType.Equal -> simplifyReferenceEquality metadata x y (withSnd state >> k)
        | OperationType.NotEqual ->
            simplifyReferenceEquality metadata x y (fun e ->
            Propositional.simplifyNegation metadata e (withSnd state >> k))
        | _ -> internalfailf "%O is not a binary arithmetical operator" op

    let internal isPointerOperation op t1 t2 =
        (Types.IsPointer t1 || Types.IsReference t1 || Types.IsBottom t1) &&
        (Types.IsPointer t2 || Types.IsReference t2 || Types.IsBottom t2) &&
        match op with
        | OperationType.Add
        | OperationType.Subtract
        | OperationType.Equal
        | OperationType.NotEqual -> true
        | _ -> false
