#load "load-project-release.fsx"

open Informedica.GenUtils.Lib
open Informedica.GenCore.Lib

// Define your library scripting code here
open Informedica.GenCore.Lib.Result.Operators

let test = null |> box
test |> NullCheck.isNull

"1" 
|> WrappedString.Id.create
>>= (fun id -> let s = id |> WrappedString.Id.value in printfn "%s" s; id |> Result.succNoMsg)
