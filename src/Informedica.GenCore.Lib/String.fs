namespace Informedica.GenUtils.Lib.BCL

module String =
    
    let charLetters = ['a'..'z'] @ ['A'..'Z']

    let charIsLetter c = charLetters |> List.exists ((=) c)