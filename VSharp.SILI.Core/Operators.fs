namespace VSharp.Core

open VSharp

[<AutoOpen>]
module internal Operators =

    let simplifyBinaryDotNetTypesOperation mtd op x y k =
        let simplify isEqual =
            match x.term, y.term with
            | Concrete(:? System.Type as t1, _), Concrete(:? System.Type as t2, _) ->
                if t1 = t2 then
                    (if isEqual then makeTrue else makeFalse) <| mtd
                elif not (t1.IsGenericParameter) && not (t2.IsGenericParameter) then
                    (if isEqual then makeFalse else makeTrue) <| mtd
                else makeBin mtd op x y
            | _ -> makeBin mtd op x y
        match op with
        | OperationType.Equal    -> simplify true |> k
        | OperationType.NotEqual -> simplify false |> k
        | _ -> __notImplemented__()
    let simplifyBinaryOperation mtd op isChecked state left right k =
        let isDotNetType typ =
            let t = Types.toDotNetType typ
            let typeOfType = typedefof<System.Type>
            typeOfType.IsAssignableFrom t
        let t1 = Terms.typeOf left
        let t2 = Terms.typeOf right
        match op with
        | _ when Types.isBottom t1 -> k (left, state)
        | _ when Types.isBottom t2 -> k (right, state)
//        | _ when isDotNetType t1 && isDotNetType t2 ->
//            simplifyBinaryDotNetTypesOperation mtd op left right (withSnd state >> k) // TODO: check this hack
        | op when Propositional.isLogicalOperation op t1 t2 ->
            Propositional.simplifyBinaryConnective mtd op left right (withSnd state >> k)
        | op when Arithmetics.isArithmeticalOperation op t1 t2 ->
            Arithmetics.simplifyBinaryOperation mtd op state left right isChecked k
        | op when Pointers.isPointerOperation op t1 t2 ->
            Pointers.simplifyBinaryOperation mtd op state left right k
        | _ -> internalfailf "simplifyBinary of: %O %O %O" left op right

    let ksimplifyEquality mtd x y k =
        simplifyBinaryOperation mtd OperationType.Equal false State.empty x y (fst >> k)

    let simplifyEquality mtd x y =
        ksimplifyEquality mtd x y id

    let (===) x y = ksimplifyEquality Metadata.empty x y id
    let (!==) x y = ksimplifyEquality Metadata.empty x y (!!)

    let simplifyUnaryOperation mtd op isChecked state t arg k =
        match t with
        | Bool -> Propositional.simplifyUnaryConnective mtd op arg (withSnd state >> k)
        | Numeric t -> Arithmetics.simplifyUnaryOperation mtd op state arg isChecked t k
        | Types.StringType -> __notImplemented__()
        | _ -> __notImplemented__()

    let simplifyOperation mtd op isChecked t args k =
        let arity = Operations.operationArity op
        match arity with
        | 1 ->
            assert(List.length args = 1)
            simplifyUnaryOperation mtd op isChecked State.empty t (List.head args) (fst >> k)
        | 2 ->
            assert(List.length args >= 2)
            Cps.List.reducek (fun x y k -> simplifyBinaryOperation mtd op isChecked State.empty x y (fst >> k)) args k
        | _ -> internalfailf "unknown operation %O" op
