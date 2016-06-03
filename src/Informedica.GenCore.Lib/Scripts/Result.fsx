#load "load-project-release.fsx"

open Informedica.GenUtils.Lib
open Informedica.GenCore.Lib

// Define your library scripting code here
open Informedica.GenCore.Lib.Result.Operators


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


Person.Dto.create "1" "Frank"
|> Person.fromDto

Person.Dto.create "" ""
|> Person.fromDto

Person.Dto.create "11111111111111111111" "Frank"
|> Person.fromDto

Person.Dto.create "1" "1"
|> Person.fromDto

Person.Dto.create "1 " "  Frank  "
|> Person.fromDto
