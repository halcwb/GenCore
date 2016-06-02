namespace Informedica.GenCore.Lib

/// Types and functions to deal with
/// value primitives
module WrappedString =

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL


    /// Functions to canonicalize a `string`
    module Canon =
        
        /// Make **s** a single line string
        let singleLine s =
            ("\s" |> Regex.regex) |> Regex.replace s " " 

        // Make a single line trimmed string
        let singleLineTrimmed = singleLine >> String.trim

        /// Make a single line trimmed capitalized string
        let singleLineTrimmedCapitalized = singleLineTrimmed >> String.capitalize
        
        /// Make a single line trimmed lower case string
        let singleLineTrimmedLower = singleLineTrimmed >> String.toLower
        
        /// Make a single line trimmed upper case string
        let singleLineTrimmedUpper = singleLineTrimmed >> String.toUpper

        /// Check whether string is changed and return
        /// a `Result`
        let liftR f msg s = 
            let s' = s |> f
            if s' = s then s |> Result.succNoMsg
            else s'          |> Result.succWithMsg (s' |> msg)

        /// Return value as `Result`
        let inline singleLineR msg s = 
            liftR singleLine msg s 

        /// Return value as `Result`
        let inline singleLineTrimmedR msg s = liftR singleLineTrimmed msg s
        
        /// Return value as `Result`
        let inline singleLineTrimmedCapitalizedR msg s = 
            liftR singleLineTrimmedCapitalized msg s 

        /// Return value as `Result`
        let inline singleLineTrimmedLowerR msg s = liftR singleLineTrimmedLower msg s 

        /// Return value as `Result`
        let inline singleLineTrimmedUpperR msg s = liftR singleLineTrimmedUpper msg s 


    /// Functions to validate a `string` 
    module Validate =

        /// Check `string` **s** is larger or equal to **min**
        let checkMinLength min s = s |> String.length >= min

        /// Check `string` **s** is smaller or equal to **min**
        let checkMaxLength max s = s |> String.length <= max

        /// Check first character in string is a letter
        let firstIsLetter = String.firstStringChar >> String.isLetter

        /// Check whether string only contains letters
        let onlyLetters = String.forall String.charIsLetter

        /// Check whether string is valid and return
        /// a `Result`
        let inline liftR f msg s = 
            if s |> f then s |> Result.succNoMsg
            else s |> msg    |> Result.fail

        /// Lift to `Result`
        let checkMinLengthR min msg s = 
            liftR (checkMinLength min) msg s 

        /// Lift to `Result`
        let checkMaxLengthR max msg s = liftR (checkMaxLength max) msg s 

        /// Lift to `Result`
        let firstIsLetterR msg s = liftR firstIsLetter msg s 

        /// Lift to `Result`
        let onlyLettersR msg s = liftR onlyLetters msg s 


    /// Type and functions that 
    /// deal with an identifier
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Id = 

        open Informedica.GenCore.Lib.Result.Operators

        /// Messsages for `Id`
        module Message =

            type Info = Info of string

            type Warning = 
                | Changed   of string * string
                
            type Error =
                | MinLength of string * int
                | MaxLength of string * int

            let info s = s |> Info |> Message.Info

            let changed s1 s2 = (s1, s2) |> Changed |> Message.warn
                        
            let minL l s = (s, l) |> MinLength |> Message.err
            
            let maxL l s = (s, l) |> MaxLength |> Message.err
            
        
        /// Type to represent an identifier
        /// can be any sinlge line string without
        /// trailing or preceding spaces.
        type Id = Id of string with
            interface WrappedValue.IWrappedValue<string> with
                member x.Value = let (Id s) = x in s

        /// Create an `Id` and return the `Result`
        let create min max = 
            let succ s  = s |> Id |> Result.succWithMsg (Message.info s)
            let canon s = s |> Canon.singleLineTrimmedR (Message.changed s)
            let valid s = 
                s
                |>  Validate.checkMinLengthR min (Message.minL min)
                >>= Validate.checkMaxLengthR max (Message.maxL max)

            WrappedValue.createResult succ canon valid

        /// Turn an `Id` to a string
        let toString id : string = id |> WrappedValue.value

    
    /// Type and functions that represent and deal 
    /// with the `Name` type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Name =
        
        open Informedica.GenCore.Lib.Result.Operators
        
        /// Messsages for `Name`
        module Message =

            type Info = Info of string

            type Warning = 
                | Changed   of string * string
                
            type Error =
                | NoLetters of string
                | MinLength of string * int
                | MaxLength of string * int

            let info s = s |> Info |> Message.Info

            let changed s1 s2 = (s1, s2) |> Changed |> Message.warn
                        
            let minL l s = (s, l) |> MinLength |> Message.err
            
            let maxL l s = (s, l) |> MaxLength |> Message.err

            let noLetters s = s |> NoLetters |> Message.err

        /// Type to represent an identifier
        /// can be any sinlge line string without
        /// trailing or preceding spaces.
        type Name = Name of string with
            interface WrappedValue.IWrappedValue<string> with
                member x.Value = let (Name s) = x in s

        /// Create an `Name` and return the `Result`
        let create min max = 
            let succ s  = s |> Name |> Result.succWithMsg (Message.info s)
            let canon s = s |> Canon.singleLineTrimmedR (Message.changed s)
            let valid s = 
                s
                |>  Validate.checkMinLengthR min (Message.minL min)
                >>= Validate.checkMaxLengthR max (Message.maxL max)
                >>= Validate.onlyLettersR Message.noLetters

            WrappedValue.createResult succ canon valid

        /// Turn an `Name` to a string
        let toString n : string = n |> WrappedValue.value


