namespace Informedica.GenUtils.Lib

/// Module with `Regex` utiltity functions
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Regex =

    open System.Text.RegularExpressions

    /// Create a regular expression with **s**.
    let regex s = new Regex(s)

    /// Apply **f** to `Regex` **a**.
    let apply f (a: Regex) =
        a.ToString() |> f

    /// Utitility function to facilitate type inference. 
    let get s = apply id s

    /// Replace all instances that match **re** in **s**
    /// with the replacement string **rs**
    let replace s (rs: string) (re: Regex) = re.Replace(s, rs)

    /// Apply **f** to the string value of `Regex and 
    /// return an `Regex`
    let bind rx (f: string -> Regex) = rx |> get |> f

    // Ensure string has one uppercase letters
    let oneUpperCase = "(?=.*[A-Z])" |> regex

    // Ensure string has two uppercase letters.
    let twoUpperCase = "(?=.*[A-Z].*[A-Z])" |> regex

    // Ensure string has one special case letter.  
    let oneSpecialCase = "(?=.*[!@#$&*])" |> regex

    // Ensure string has one digit.
    let oneDigit = "(?=.*[0-9])" |> regex

    // Ensure string has two digits.
    let twoDigits = "(?=.*[0-9].*[0-9])" |> regex

    // Ensure string has three lowercase letters.
    let threeLowerCase = "(?=.*[a-z].*[a-z].*[a-z])" |> regex

    /// Look for pattern `Regex` **regex** in `string` **s**
    let regexMatch (regex: Regex) s =
        (s, regex.Match(s).Success)

    /// Apply **f** to a match result **sm**
    let bindMatch sm f = 
        match sm with
        | (s, m) when m -> f s
        | _ -> sm

    /// Get all matches of a `Regex` list **regexs**
    /// in a `string` **s**.
    let matchRegexList regexs s  =
        let (>>=) sm f = bindMatch sm f

        regexs |> List.fold(fun sm m -> sm >>= regexMatch m) (s, true) 


