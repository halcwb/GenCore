namespace Informedica.GenCore.Lib

/// Types and functions to deal with
/// value primitives
module WrappedString =

    open Informedica.GenUtils.Lib
    open Informedica.GenUtils.Lib.BCL

    /// Messsages for `WrappedString`
    module Messages =

        /// Create a success message
        let succMsg s = 
            sprintf "Created %s" s
            |> Message.info

        /// Create a warnig message
        let warnMsg s1 s2 = 
            sprintf "Changed original %s to %s" s1 s2 
            |> Message.warn

        /// Create an error message
        let errMsg s msg = 
            msg 
            |> sprintf "Couldn't create with %s: %s" s
            |> Message.err


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
        let liftR f s = 
            let s' = s |> f
            if s' = s then s |> Result.succNoMsg
            else s'          |> Result.succWithMsg (Messages.warnMsg s s')

        /// Return value as `Result`
        let singleLineR = liftR singleLine

        /// Return value as `Result`
        let singleLineTrimmedR = liftR singleLineTrimmed
        
        /// Return value as `Result`
        let singleLineTrimmedCapitalizedR = liftR singleLineTrimmedCapitalized

        /// Return value as `Result`
        let singleLineTrimmedLowerR = liftR singleLineTrimmedLower

        /// Return value as `Result`
        let singleLineTrimmedUpperR = liftR singleLineTrimmedUpper


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
        let liftR f msg s = 
            if s |> f then s             |> Result.succNoMsg
            else (Messages.errMsg s msg) |> Result.fail

        /// Lift to `Result`
        let checkMinLengthR min t msg = liftR (checkMinLength min) t msg

        /// Lift to `Result`
        let checkMaxLengthR max t msg = liftR (checkMaxLength max) t msg

        /// Lift to `Result`
        let firstIsLetterR msg = liftR firstIsLetter msg

        /// Lift to `Result`
        let onlyLettersR msg = liftR onlyLetters msg


    /// Type and functions that 
    /// deal with an identifier
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Id = 

        open Informedica.GenCore.Lib.Result.Operators

        /// Min length of an `Id`
        let minLength = 1

        /// Max length of an `Id`
        let maxLength = 1000

        let succMsg = sprintf "Id with %s"

        let minMsg = sprintf "Id should be minimal %i" minLength
        
        let maxMsg = sprintf "Id should be maximal %i" maxLength
        
        /// Type to represent an identifier
        /// can be any sinlge line string without
        /// trailing or preceding spaces.
        type Id = Id of string with
            interface WrappedValue.IWrappedValue<string> with
                member x.Value = let (Id s) = x in s

        /// Create an `Id` and return the `Result`
        let create = 
            let succ s = s |> Id |> Result.succWithMsg (s |> succMsg |> Messages.succMsg) 
            let canon s = s |> Canon.singleLineTrimmedR
            let valid s = 
                s
                |>  Validate.checkMinLengthR minLength minMsg
                >>= Validate.checkMaxLengthR maxLength maxMsg

            WrappedValue.createResult succ canon valid

        /// Turn an `Id` to a string
        let toString id : string = id |> WrappedValue.value

    /// Type and functions that represent and deal 
    /// with the `Name` type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Name =
        
        open Informedica.GenCore.Lib.Result.Operators

        /// Min length of an `Id`
        let minLength = 1

        /// Max length of an `Id`
        let maxLength = 1000
        
        let succMsg = sprintf "Name with %s"

        let minMsg = sprintf "Name should be minimal %i" minLength
        
        let maxMsg = sprintf "Name should be maximal %i" maxLength
        
        let lettrMsg = sprintf "Name shouod only contain letters, not %s"

        /// Type to represent an identifier
        /// can be any sinlge line string without
        /// trailing or preceding spaces.
        type Name = Name of string with
            interface WrappedValue.IWrappedValue<string> with
                member x.Value = let (Name s) = x in s

        /// Create an `Name` and return the `Result`
        let create = 
            let succ s = s |> Name |> Result.succWithMsg (s |> succMsg |> Messages.succMsg) 
            let canon s = s |> Canon.singleLineTrimmedR
            let valid s = 
                s
                |>  Validate.checkMinLengthR minLength minMsg
                >>= Validate.checkMaxLengthR maxLength maxMsg
                >>= Validate.onlyLettersR (lettrMsg s)

            WrappedValue.createResult succ canon valid

        /// Turn an `Name` to a string
        let toString n : string = n |> WrappedValue.value

