namespace Informedica.GenCore.Lib
    

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Request =

    type Request<'P, 'R> =
        {
            Param: 'P
            Capability: 'P -> 'R
        }

    let create p c = { Param = p; Capability = c }


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Response =


    type Response<'P, 'R>  =
        {
            Result: 'R
            Requests: Request.Request<'P, 'R>  list
        }
    
    let create (res, reqs) = { Result = res; Requests = reqs }

    let procReq caps (r: Request.Request<_, _>) = 
        let res =
            r.Param 
            |> r.Capability
        res
        |> caps
        |> create

