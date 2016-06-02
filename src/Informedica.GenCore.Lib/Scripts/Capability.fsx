#load "load-project-release.fsx"    

open Informedica.GenUtils.Lib
open Informedica.GenUtils.Lib.BCL

type App = App of (unit -> UserLogin)
and UserLogin = UserLogin of (Login * Register)
and Login = Login of (User -> LoggedIn)
and Register = Register of (User -> Registered)
and User = User of string * string
and LoggedIn =
| Succ of LoginSucc
| Fail of UserLogin
and Registered =
| Succ of UserLogin
| Fail of UserLogin
and LoginSucc = LoginSucc of LogOut * PatientActions
and LogOut = LogOut of UserLogin
and PatientActions =
| All of GetAllPatients
| Add of AddPatient
| Get of GetPatient
| Rem of RemPatient
and GetAllPatients = unit -> (Patient list * LoginSucc)
and GetPatient = unit -> (Patient * LoginSucc)
and AddPatient = Patient -> LoginSucc
and RemPatient = Patient -> LoginSucc
and Patient = Patient of string


