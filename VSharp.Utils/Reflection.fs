namespace VSharp

open System
open System.Reflection
open VSharp.CSharpUtils

module public Reflection =

    // ----------------------------- Binding Flags ------------------------------

    let staticBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.IgnoreCase ||| BindingFlags.DeclaredOnly |||
        BindingFlags.Static ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let instanceBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        BindingFlags.IgnoreCase ||| BindingFlags.DeclaredOnly |||
        BindingFlags.Instance ||| BindingFlags.NonPublic ||| BindingFlags.Public
    let allBindingFlags =
        let (|||) = Microsoft.FSharp.Core.Operators.(|||)
        staticBindingFlags ||| instanceBindingFlags

    // --------------------------- Metadata Resolving ---------------------------

    let private retrieveMethodsGenerics (method : MethodBase) =
        match method with
        | :? MethodInfo as mi -> mi.GetGenericArguments()
        | :? ConstructorInfo -> null
        | _ -> __notImplemented__()

    let resolveField (method : MethodBase) fieldToken =
        let methodsGenerics = retrieveMethodsGenerics method
        let typGenerics = method.DeclaringType.GetGenericArguments()
        method.Module.ResolveField(fieldToken, typGenerics, methodsGenerics)

    let resolveType (method : MethodBase) typeToken =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveType(typeToken, typGenerics, methodGenerics)

    let resolveMethod (method : MethodBase) methodToken =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveMethod(methodToken, typGenerics, methodGenerics)

    let resolveToken (method : MethodBase) token =
        let typGenerics = method.DeclaringType.GetGenericArguments()
        let methodGenerics = retrieveMethodsGenerics method
        method.Module.ResolveMember(token, typGenerics, methodGenerics)

    // --------------------------------- Methods --------------------------------

    // TODO: what if return type is generic?
    let public GetMethodReturnType : MethodBase -> Type = function
        | :? ConstructorInfo -> typeof<System.Void>
        | :? MethodInfo as m -> m.ReturnType
        | _ -> internalfail "unknown MethodBase"

    let public GetFullMethodName (methodBase : MethodBase) =
        let returnType = GetMethodReturnType methodBase
        methodBase.GetParameters()
        |> Seq.map (fun param -> param.ParameterType.FullName)
        |> if methodBase.IsStatic then id else Seq.cons "this"
        |> join ", "
        |> sprintf "%s %s.%s(%s)" returnType.FullName methodBase.DeclaringType.FullName methodBase.Name

    let public IsArrayConstructor (methodBase : MethodBase) =
        methodBase.IsConstructor && methodBase.DeclaringType.IsArray

    let public IsDelegateConstructor (methodBase : MethodBase) =
        methodBase.IsConstructor && methodBase.DeclaringType.IsSubclassOf typedefof<System.Delegate>

    let public IsGenericOrDeclaredInGenericType (methodBase : MethodBase) =
        methodBase.IsGenericMethod || methodBase.DeclaringType.IsGenericType

    // --------------------------------- Substitute generics ---------------------------------

    let private substituteMethod argSubst methodType (m : MethodBase) getMethod =
        let concreteParameters = m.GetParameters() |> Array.map (fun p -> argSubst p.ParameterType)
        let method = getMethod methodType m.Name concreteParameters
        assert(method <> null)
        method

    let private substituteMethodInfo argSubst methodType (mi : MethodInfo) groundK genericK =
        let getMethod genericArgsNumber (t : Type) (name : String) (parameters : Type[]) =
            t.GetMethod(name, genericArgsNumber, allBindingFlags, null, parameters, null)
        let substituteGeneric (mi : MethodInfo) =
            let args = mi.GetGenericArguments()
            let num = Array.length args
            let genericMethod = mi.GetGenericMethodDefinition()
            let mi = substituteMethod argSubst methodType genericMethod (getMethod num)
            genericK mi args
        if mi.IsGenericMethod then substituteGeneric mi
        else groundK (substituteMethod argSubst methodType mi (getMethod 0) :> MethodBase)

    let private substituteCtorInfo argSubst methodType ci k =
        let getCtor (t : Type) _ parameters = t.GetConstructor(parameters)
        k (substituteMethod methodType argSubst ci getCtor :> MethodBase)

    let private substituteMethodBase<'a> (argSubst : Type -> Type) methodType (m : MethodBase) (groundK : MethodBase -> 'a) genericK =
        match m with
        | _ when not <| IsGenericOrDeclaredInGenericType m -> groundK m
        | :? MethodInfo as mi ->
            substituteMethodInfo argSubst methodType mi groundK genericK
        | :? ConstructorInfo as ci ->
            substituteCtorInfo methodType argSubst ci groundK
        | _ -> __unreachable__()

    // --------------------------------- Generalization ---------------------------------

    let private getGenericTypeDefinition (typ : Type) =
        if typ.IsGenericType then
            let args = typ.GetGenericArguments()
            let genericType = typ.GetGenericTypeDefinition()
            let parameters = genericType.GetGenericArguments()
            genericType, args, parameters
        else typ, [||], [||]

    let generalizeMethodBase (methodBase : MethodBase) =
        let genericType, tArgs, tParams = getGenericTypeDefinition methodBase.DeclaringType
        let subst (x : Type) =
            match Array.tryFindIndex ((=) x) tArgs with
            | Some idx -> tParams.[idx]
            | None -> x
        let genericCase m args = m :> MethodBase, args, m.GetGenericArguments()
        let groundCase m = m, [||], [||]
        let genericMethod, mArgs, mParams = substituteMethodBase subst genericType methodBase groundCase genericCase
        let genericArgs = Array.append mArgs tArgs
        let genericDefs = Array.append mParams tParams
        genericMethod, genericArgs, genericDefs

    // --------------------------------- Concretization ---------------------------------

    let rec public concretizeType (subst : Type -> Type) (typ : Type) =
        if typ.IsGenericParameter then subst typ
        elif typ.IsGenericType then
            let args = typ.GetGenericArguments()
            typ.GetGenericTypeDefinition().MakeGenericType(Array.map (concretizeType subst) args)
        else typ

    let concretizeMethodBase (m : MethodBase) (subst : Type -> Type) =
        let concreteType = concretizeType subst m.DeclaringType
        let substGenericTypeArg (parameterType : Type) =
            if parameterType.IsGenericTypeParameter then subst parameterType else parameterType
        let substArgsIntoMethod (mi : MethodInfo) args =
            mi.MakeGenericMethod(args |> Array.map subst) :> MethodBase
        substituteMethodBase substGenericTypeArg concreteType m id substArgsIntoMethod

    let concretizeParameter (p : ParameterInfo) (subst : Type -> Type) =
        assert(p.Member :? MethodBase)
        (concretizeMethodBase (p.Member :?> MethodBase) subst).GetParameters() |> Array.find (fun pi -> pi.Name = p.Name)

    let concretizeLocalVariable (l : LocalVariableInfo) (m : MethodBase) (subst : Type -> Type) =
        let m = concretizeMethodBase m subst
        let mb = m.GetMethodBody()
        assert(mb <> null)
        mb.LocalVariables.[l.LocalIndex], m

    let concretizeField (f : fieldId) (subst : Type -> Type) =
        let declaringType = concretizeType subst f.declaringType
        {declaringType = declaringType; name = f.name; typ = concretizeType subst f.typ}

    // --------------------------------- Fields ---------------------------------

    let wrapField (field : FieldInfo) =
        // TODO: why safeGenericTypeDefinition? #Dima
        {declaringType = field.DeclaringType; name = field.Name; typ = field.FieldType}

    let rec private retrieveFields isStatic f (t : System.Type) =
        let staticFlag = if isStatic then BindingFlags.Static else BindingFlags.Instance
        let flags = BindingFlags.Public ||| BindingFlags.NonPublic ||| staticFlag
        let fields = t.GetFields(flags)
        let ourFields = f fields
        if isStatic || t.BaseType = null then ourFields
        else Array.append (retrieveFields false f t.BaseType) ourFields

    let retrieveNonStaticFields t = retrieveFields false id t

    let fieldsOf isStatic (t : System.Type) =
        let extractFieldInfo (field : FieldInfo) =
            // Events may appear at this point. Filtering them out...
            if field.FieldType.IsSubclassOf(typeof<MulticastDelegate>) then None
            else Some (wrapField field, field.FieldType)
        retrieveFields isStatic (FSharp.Collections.Array.choose extractFieldInfo) t

    // Returns pair (valueFieldInfo, hasValueFieldInfo)
    let fieldsOfNullable typ =
        let fs = fieldsOf false typ
        match fs with
        | [|(f1, _); (f2, _)|] when f1.name.Contains("value", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("hasValue", StringComparison.OrdinalIgnoreCase) -> f1, f2
        | [|(f1, _); (f2, _)|] when f1.name.Contains("hasValue", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("value", StringComparison.OrdinalIgnoreCase) -> f2, f1
        | _ -> internalfailf "%O has unexpected fields {%O}! Probably your .NET implementation is not supported :(" typ.FullName (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")

    let stringLengthField, stringFirstCharField =
        let fs = fieldsOf false typeof<string>
        match fs with
        | [|(f1, _); (f2, _)|] when f1.name.Contains("length", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("firstChar", StringComparison.OrdinalIgnoreCase) -> f1, f2
        | [|(f1, _); (f2, _)|] when f1.name.Contains("firstChar", StringComparison.OrdinalIgnoreCase) && f2.name.Contains("length", StringComparison.OrdinalIgnoreCase) -> f2, f1
        | _ -> internalfailf "System.String has unexpected fields {%O}! Probably your .NET implementation is not supported :(" (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")

    let emptyStringField =
        let fs = fieldsOf true typeof<string>
        match fs |> Array.tryFind (fun (f, _) -> f.name.Contains("empty", StringComparison.OrdinalIgnoreCase)) with
        | Some(f, _) -> f
        | None -> internalfailf "System.String has unexpected static fields {%O}! Probably your .NET implementation is not supported :(" (fs |> Array.map (fun (f, _) -> f.name) |> join ", ")
