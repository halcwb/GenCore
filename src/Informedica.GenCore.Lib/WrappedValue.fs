namespace Informedica.GenCore.Lib

/// Interface definition and functions to
/// canonicalize, validate and construct 
/// wrapped values.
module WrappedValue =

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.NullCheck

    open Result.Operators

    /// Exception when the primitive type is a null value
    exception NullException
    
    /// Represents a primitive type value wrapped
    /// in a canonicalized and validated type
    type IWrappedValue<'T when 'T:> System.IComparable> =
        abstract member Value: 'T

    /// Create a wrapped value with continuations
    ///
    /// 1. canonicalize the input, if the results differ pass the en result to warn
    /// 2. validate the input, if validation fails pass the value to err
    /// 3. create the `Result`, and pass the result to the succ function
    ///
    /// Null values are never valid and will be passed to an exc function
    /// Also, when unexpected exceptions occur the exception will be
    /// passed to a exc function.
    let create succ warn err exc ctor canon valid value =
        try
            if value |> box |> isNull then 
                NullException |> Message.exc |> exc
            else 
                let value' = 
                    value 
                    |> canon
                match value' = value, value' |> valid with
                | true,  true -> value' |> ctor |> succ
                | false, true -> value' |> ctor |> warn
                | _, false    -> value' |> err
        with e -> e |> Message.Except |> exc

    /// Create a wrapped value `Option`
    ///
    /// 1. canonicalize the input first
    /// 2. If the validation succeeds, return Some of success
    /// 3. If the validation fails, return None
    ///
    /// Null values are never valid and will return None
    let createSome ctor canon valid = 
        create Some Some Option.none Option.none ctor canon valid

    /// Create a wrapped value `Result`
    ///
    /// 1. canonicalize the input, 
    /// 2. validate the input,
    /// 3. create the `Result` 
    ///
    /// Null values are never valid and will be passed to an fail `Result`
    /// Also, when unexpected exceptions occur the exception will be
    /// passed to a fail result.
    let createResult succ canon valid value =
        try
            if value |> box |> isNull then 
                NullException |> raise
            else 
                value 
                |> canon
                >>= valid
                >>= succ 
        with e -> e |> Message.Except |> Result.fail

    /// Apply the given function to the wrapped value
    let apply f (v:IWrappedValue<_>) = 
        v.Value |> f 

    /// Get the wrapped value
    let value s = apply id s

    /// Equality 
    let equals left right = 
        (value left) = (value right)

    /// Equality
    let equalsValue (wrapped: IWrappedValue<_>)  (value: _)= 
        wrapped.Value = value

    /// Comparison
    let compareTo left right = 
        (value left).CompareTo (value right)    

    /// Bind for composition
    let bind (value: IWrappedValue<_>) (f: _ -> IWrappedValue<_>) =
        value.Value |> f

