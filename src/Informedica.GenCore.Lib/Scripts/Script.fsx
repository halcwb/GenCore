#load "load-project-release.fsx"

open Informedica.GenUtils.Lib
open Informedica.GenCore.Lib

// Define your library scripting code here

let test = null |> box
test |> NullCheck.isNull
