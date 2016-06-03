(*** hide ***)
#I "../../src/Informedica.GenCore.Lib/Scripts"
#load "load-project-release.fsx"

(** 
# Use of the Result monad

To use the `Result` monad open up th following library
*)

open Informedica.GenCore.Lib

(** 
Open up the `Result` operators
*)
open Informedica.GenCore.Lib.Result.Operators

(** 
Sample domain object `Person`
*)

module Person =

    open WrappedString

    // Dto to create a Person
    module Dto =

        type Dto = { Id: string; Name: string }

        let create id nm = { Id = id; Name = nm }

    // Person messages mapped from
    // Id and Name messages
    module Message = 

        type IdMessage =
            | IdInfo of Id.Message.Info
            | NameInfo of Name.Message.Info

        type WarnMessage =
            | IdWarn of Id.Message.Warning
            | NameWarn of Name.Message.Warning

        type ErrMessage =
            | IdErr of Id.Message.Error
            | NameErr of Name.Message.Error


    type Person = { Id: WrappedString.Id.Id; Name: WrappedString.Name.Name }

    // Create a Person using Id and Name domain types
    let create id nm = { Id = id; Name = nm }

    // Create a Person from a Dto
    let fromDto (dto: Dto.Dto) =
        let ctId id =
            // Map Id messages to Person messages
            let map msg = 
                match msg with
                | Message.Info i    -> i |> Message.IdInfo |> Message.Info
                | Message.Warning w -> w |> Message.IdWarn |> Message.Warning
                | Message.Error e   -> e |> Message.IdErr |> Message.Error
                | Message.Except e  -> e |> Message.Except

            id 
            |> WrappedString.Id.create 1 10
            |> Result.mapMessagesR map

        let ctNm nm = 
            // Map Id messages to Person messages
            let map msg = 
                match msg with
                | Message.Info i    -> i |> Message.NameInfo |> Message.Info
                | Message.Warning w -> w |> Message.NameWarn |> Message.Warning
                | Message.Error e   -> e |> Message.NameErr |> Message.Error
                | Message.Except e  -> e |> Message.Except

            nm
            |> WrappedString.Name.create 1 20
            |> Result.mapMessagesR map

        create
        <!> ctId dto.Id
        <*> ctNm dto.Name


(**
Creating a `Person`, the happy path 
*)
Person.Dto.create "1" "Frank"
|> Person.fromDto

 //[fsi:  Succ]
 //[fsi:    ({Id = Id "1";]
 //[fsi:      Name = Name "Frank";},]
 //[fsi:     [[Info (IdInfo (Info "1"))]; [Info (NameInfo (Info "Frank"))]])]

(** 
Trying to create a `Person` with empty strings fails
*)

Person.Dto.create "" ""
|> Person.fromDto

//[fsi:  Fail]
//[fsi:    [[Error (IdErr (MinLength ("",1)))]; [Error (NameErr (MinLength ("",1)))]]]

(** 
Also, the `Id` cannot be too large, but creating a `Name` succeeded
*)

Person.Dto.create "11111111111111111111" "Frank"
|> Person.fromDto

//[fsi:  Fail]
//[fsi:    [[Error (IdErr (MaxLength ("11111111111111111111",10)))];]
//[fsi:     [Info (NameInfo (Info "Frank"))]]]


(**
A `Name` cannot contain non-letter characters
*)

Person.Dto.create "1" "1"
|> Person.fromDto

//[fsi:  Fail [[Error (NameErr (NoLetters "1"))]; [Info (IdInfo (Info "1"))]]]

(**
Creating `Person` succeeded, but both `Id` and `Name` had trailing spaces
and where trimmed
*)

Person.Dto.create "1 " "  Frank  "
|> Person.fromDto

//[fsi:  Succ]
//[fsi:    ({Id = Id "1";]
//[fsi:      Name = Name "Frank";},]
//[fsi:     [[Warning (IdWarn (Changed ("1 ","1"))); Info (IdInfo (Info "1"))];]
//[fsi:      [Warning (NameWarn (Changed ("  Frank  ","Frank")));]
//[fsi:       Info (NameInfo (Info "Frank"))]])]
