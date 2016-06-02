namespace Informedica.GenCore.Lib

/// Type and functions that model and process
/// a `Message` to be used with the `Result` monad
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =

    open System

    /// A `Message` can be used to convey
    ///
    /// * Information in case of success
    /// * Warning when there is a success but might not be what is expected
    /// * Error when there is no success but the user can do something about it
    /// * Exception when there is no success and only a programmer can fix it
    type Message<'TInfo, 'TWarn, 'TErr> = 
        | Info    of 'TInfo
        | Warning of 'TWarn
        | Error   of 'TErr
        | Except  of Exception

    /// Create an info `Message`
    let info s = s |> Info

    /// Create a warning `Message`
    let warn s = s |> Warning

    /// Create an error `Message`
    let err s = s |> Error

    /// Create an exception `Message`
    let exc e = e |> Except


[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Result =        

    open Informedica.GenUtils.Lib

    /// A function `Result` is either a success (Succ) or failure (Fail).
    /// The Success case has a success value, plus a list of messages,
    /// The Failure case has just a list of messages
    type Result<'TSucc, 'TMesg> =
        | Succ of 'TSucc * 'TMesg list
        | Fail of 'TMesg list

    /// Create a `Succ` with no messages.
    let succNoMsg x = (x, []) |> Succ

    /// Create a `Succ` with a message.
    let succWithResult x msg = (x, [msg]) |> Succ

    /// Create a `Succ` with a message.
    let succWithMsg msg x = msg |> succWithResult x

    /// Create a `Fail` with a message.
    let fail msg = [msg] |> Fail

    /// A function that applies either **fSucc** or **fFail** 
    /// depending on the case.
    let either fSucc fFail = function
        | Succ (x, msgs) -> (x, msgs) |> fSucc
        | Fail errs      -> errs      |> fFail 

    /// Merge messages with a `Result`.
    let mergeMsgs msgs result =
        let fSucc (x, msgs') = (x, msgs @ msgs') |> Succ
        let fFail errs       = (errs @ msgs)     |> Fail

        either fSucc fFail result

    /// Given a function that generates a new `Result`
    /// apply it only if the result is on the `Succ` branch
    /// merge any existing messages with the new result.
    let bindR f result =
        let fSucc (x, msgs) = f x |> mergeMsgs msgs
        let fFail errs = Fail errs 

        either fSucc fFail result

    /// Switched parameter from `bindR`.
    let bindL result f = bindR f result

    /// Given a function wrapped in a `Result`
    /// and a value wrapped in a `Result`,
    /// apply the function to the value only if both are `Succ`.
    let applyR f result =
        match f, result with
        // Both succeed
        | Succ (f, msgs1), Succ (x, msgs2) -> (f x, msgs1 @ msgs2) |> Succ
        // Either one or both fail 
        | Fail errs,       Succ (_, msgs) 
        | Succ (_, msgs),  Fail errs  -> errs @ msgs   |> Fail
        | Fail errs1,      Fail errs2 -> errs1 @ errs2 |> Fail 

    /// Given a function that transforms a value
    /// apply it only if the `Result` is on the Success branch.
    let liftR f result =
        let f' = f |> succNoMsg
        applyR f' result 

    /// Given two values wrapped in results apply a function to both.
    let lift2R f result1 result2 =
        let f' = liftR f result1
        applyR f' result2 

    /// Given three values wrapped in results apply a function to all.
    let lift3R f result1 result2 result3 =
        let f' = lift2R f result1 result2 
        applyR f' result3

    /// Given four values wrapped in results apply a function to all.
    let lift4R f result1 result2 result3 result4 =
        let f' = lift3R f result1 result2 result3 
        applyR f' result4

    /// synonym for liftR.
    let mapR = liftR

    /// Given an `Result`, call a unit function on the success branch
    /// and pass thru the result.
    let successTee f result = 
        let fSucc (x, msgs) = f x msgs; (x, msgs) |> Succ
        let fFail errs      = errs                |> Fail 

        either fSucc fFail result

    /// Given an `Result`, call a unit function on the failure branch
    /// and pass thru the result.
    let failureTee f result = 
        let fSucc (x, msgs) = (x, msgs)    |> Succ
        let fFail errs      = f errs; errs |> Fail

        either fSucc fFail result

    /// Given an `Result`, map the messages to a different error type.
    let mapMessagesR f result = 
        match result with 
        | Succ (x, msgs) -> msgs   |> List.map f |> succWithResult x
        | Fail errors    -> errors |> List.map f |> fail 

    /// Given an `Result`, in the success case, return the value.
    /// In the failure case, determine the value to return by 
    /// applying a function to the errors in the failure case
    let valueOrDefault f result = 
        match result with 
        | Succ (x, _) -> x
        | Fail errs   -> f errs

    /// lift an option to a `Result`.
    /// Result Success if Some
    /// or the Given message if None
    let failIfNone msg = function
        | Some x -> x   |> succNoMsg
        | None   -> msg |> fail 

    /// Given an `Result` option, return it
    /// or the Given message if None
    let failIfNoneR msg = function
        | Some x -> x
        | None   -> msg |> fail 

    /// Helper function to check a `Result`
    /// with a function **f**
    let resultIs f = function 
        | Fail _ -> f |> not
        | Succ _ -> f

    /// Check whether a `Result` is on the
    /// failure branch.
    let isFailure result = resultIs false result

    /// Check whether a `Result` is on the
    /// success branch.
    let isSuccess result = resultIs true result

    /// Get the messages from a `Result`
    let getMessages = function
        | Succ(_, msgs) | Fail msgs -> msgs

    /// Wrap a function in a try catch block
    /// and pass the exception to a the fail 
    /// branch.
    let tryCatch f x =  try x |> f with e -> e |> Message.exc |> fail

    /// Contains the infix operators for
    /// 
    /// * `>>=` bindL: bind a `Result` to a function that processes the contents of the result
    /// * `<<=` bindR: bind a function that processes the contents of a result to a `Result`
    /// * `<!>` liftR: lift a normal function to process and return a `Result`, note the function
    /// should be a 'non failing' function without messages
    /// * `<*>` applyR: apply a `Result` to a function that processes and returns a `Result`
    module Operators =
    
        /// Infix version of `bindL`.
        let (>>=) = bindL

        /// Infix version of `bindR`.
        let (<<=) = bindR

        /// infix version of liftR.
        let (<!>) = liftR

        /// Infix version of `applyR`.
        let (<*>) = applyR
