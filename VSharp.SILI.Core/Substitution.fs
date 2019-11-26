namespace VSharp.Core

open VSharp

module Substitution =

    let rec substitute subst addressSubst typeSubst term =
        match term.term with
        | Ref(topLevel, path) ->
            substituteRef subst addressSubst typeSubst topLevel path (Ref term.metadata) |> Merging.merge |> subst
        | Ptr(topLevel, path, typ, shift) ->
            let ctor =
                match shift with
                | None -> fun tl path -> Ptr term.metadata tl path typ
                | Some shift ->
                    fun tl path ->
                        shift
                        |> substitute subst addressSubst typeSubst
                        |> Merging.guardedErroredApply (IndentedPtr term.metadata tl path typ)
            substituteRef subst addressSubst typeSubst topLevel path ctor |> Merging.merge
        | Error e ->
            e |> substitute subst addressSubst typeSubst |> Merging.guardedErroredApply (fun e' ->
            if e' = e then term else Error term.metadata e')
        | Expression(op, args, t) ->
            let t = typeSubst t
            substituteMany subst addressSubst typeSubst args (fun args' ->
            if args = args' then term
            else
                match op with
                | Operator(op, isChecked) -> Operators.simplifyOperation term.metadata op isChecked t args' id
                // TODO: this is temporary hack, support normal substitution cast expression
                | Cast _ -> Expression term.metadata op args' t
                | Application _ -> __notImplemented__())
            |> Merging.merge
        | Union gvs ->
            let gvs' = gvs |> List.collect (fun (g, v) ->
                let ges, ggs = substitute subst addressSubst typeSubst g |> Merging.erroredUnguard
                if isFalse ggs then ges else (ggs, substitute subst addressSubst typeSubst v)::ges)
            if gvs' = gvs then term else Merging.merge gvs'
        | Block(contents, typ) ->
            let contents' = substituteHeap id subst addressSubst typeSubst contents
            let typ' = Option.map typeSubst typ
            Block term.metadata contents' typ'
        | Array(dim, len, lower, inst, contents, lengths) ->
            let dimerrs, dim' = dim |> substitute subst addressSubst typeSubst |> Merging.erroredUnguard
            let lenerrs, len' = len |> substitute subst addressSubst typeSubst |> Merging.erroredUnguard
            let lower' = substituteHeap subst subst addressSubst typeSubst lower
            let contents' = substituteHeap subst subst addressSubst typeSubst contents
            let lengths' = substituteHeap subst subst addressSubst typeSubst lengths
            let getErrorsAndInstors (ges, gis) (g, i) =
                let ges', g' = g |> substitute subst addressSubst typeSubst |> Merging.erroredUnguard
                let gis' = Merging.genericSimplify [(g', i)]
                List.append ges ges', List.append gis gis'
            let insterrs, inst' = List.fold getErrorsAndInstors ([], []) inst
            let errs = List.concat [dimerrs; lenerrs; insterrs]
            let guard = errs |> List.fold (fun d (g, _) -> d ||| g) False
            let result = Array term.metadata dim' len' lower' inst' contents' lengths'
            (!!guard, result)::errs |> Merging.merge
        | _ -> subst term

    and private substituteMany subst addressSubst typeSubst terms ctor =
        Merging.guardedCartesianProduct (substitute subst addressSubst typeSubst >> Merging.unguard) terms ctor

    and private substituteAndMap subst addressSubst typeSubst mapper =
        substitute subst addressSubst typeSubst >> Merging.unguard >> Merging.guardedMapWithoutMerge mapper

    and substituteHeap<'a when 'a : equality> (keySubst : 'a -> 'a) (subst : term -> term) (addressSubst : term -> termType -> termType -> (term * topLevelAddress) list) (typeSubst : termType -> termType) (heap : 'a heap) : ('a heap) = // TODO: collect errors?
        Heap.mapFQL (fun (k, v) -> substituteHeapKey keySubst subst addressSubst typeSubst k, substitute subst addressSubst typeSubst v) heap

    and substituteHeapKey<'a when 'a : equality> (keySubst : 'a -> 'a) (subst : term -> term) (addressSubst : term -> termType -> termType -> (term * topLevelAddress) list) (typeSubst : termType -> termType) (key : 'a memoryCell) : 'a memoryCell = // TODO: do better (effective)! #do
        let key' : 'a = keySubst key.key
        let FQL' = Option.map (substituteFQL subst addressSubst typeSubst) key.FQL // TODO: be careful about this #do
        let typ' = typeSubst key.typ
        match FQL' with
        | None -> {key = key'; FQL = None; typ = typ'}
        | Some [True, fql] -> {key = key'; FQL = Some fql; typ = typ'}
        | _ -> internalfail "substitution of heap key has failed"

    and private substituteTopLevel addressSubst typeSubst = function
        | TopLevelHeap(addr, bt, st) -> addressSubst addr bt st
        | TopLevelStatics typ -> [True, typ |> typeSubst |> TopLevelStatics]
        | NullAddress
        | TopLevelStack _ as tl -> [True, tl]

    and private substituteSegment subst addressSubst typeSubst = function
        | BlockField(f, t) -> [True, BlockField(f, typeSubst t)]
        | ArrayIndex(i, t) ->
            let t' = typeSubst t
            substituteAndMap subst addressSubst typeSubst (fun i' -> ArrayIndex(i', t')) i
        | ArrayLowerBound i ->
            substituteAndMap subst addressSubst typeSubst ArrayLowerBound i
        | ArrayLength i ->
            substituteAndMap subst addressSubst typeSubst ArrayLength i

    and private substitutePath subst addressSubst typeSubst path ctor =
        Merging.genericGuardedCartesianProduct (substituteSegment subst addressSubst typeSubst) path ctor

    and private substituteFQL subst addressSubst typeSubst (topLevel, path) = // TODO: mb use genericGuardedCartesianProduct? #do
        let tls = substituteTopLevel addressSubst typeSubst topLevel
        let paths = substitutePath subst addressSubst typeSubst path id
        let createFQL (g, tl) = List.map (fun (g', path) -> g &&& g', (tl, path)) paths
        List.collect createFQL tls

    and private substituteRef subst addressSubst typeSubst topLevel path ctor =
        let FQL' = substituteFQL subst addressSubst typeSubst (topLevel, path)
        List.map (fun (g, (tl, path)) -> g, ctor tl path) FQL'
