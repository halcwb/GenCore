namespace Informedica.GenCore.Lib

/// Types and functions to deal with
/// value primitives
module WrappedString =

    type WrappedString = WrappedString of string

    /// Type and functions that 
    /// deal with an identifier
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Id = 

        type Id = Id of string

        let create s = s |> Id

        let lift f = fun (Id s) -> s |> f |> create

        let toString (Id s) = s

    /// Helper functions for `Informedica.GenSolver.Variable.Name` type
    [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
    module Name =
        
        type Name = Name of string

        /// Create a `Name` from a list of strings that 
        let create s = s |> Name

        let apply f (n: Name) = n |> f

        let lift f = fun (Name s) -> s |> f |> Name

        let toString (Name s) = s


