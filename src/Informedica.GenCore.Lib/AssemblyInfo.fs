namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Informedica.GenCore.Lib")>]
[<assembly: AssemblyProductAttribute("Informedica.GenCore.Lib")>]
[<assembly: AssemblyCompanyAttribute("halcwb")>]
[<assembly: AssemblyDescriptionAttribute("Library with core functionality for Informedica.Gen* libraries")>]
[<assembly: AssemblyVersionAttribute("0.0.1")>]
[<assembly: AssemblyFileVersionAttribute("0.0.1")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.0.1"
    let [<Literal>] InformationalVersion = "0.0.1"
