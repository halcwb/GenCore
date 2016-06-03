namespace System
open System.Reflection

[<assembly: AssemblyTitleAttribute("Informedica.GenCore.Lib")>]
[<assembly: AssemblyProductAttribute("Informedica.GenCore.Lib")>]
[<assembly: AssemblyCompanyAttribute("halcwb")>]
[<assembly: AssemblyDescriptionAttribute("Library with core functionality for Informedica.Gen* libraries")>]
[<assembly: AssemblyVersionAttribute("0.1.0")>]
[<assembly: AssemblyFileVersionAttribute("0.1.0")>]
do ()

module internal AssemblyVersionInformation =
    let [<Literal>] Version = "0.1.0"
    let [<Literal>] InformationalVersion = "0.1.0"
