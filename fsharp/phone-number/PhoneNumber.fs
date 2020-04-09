module PhoneNumber
open System

let validateInput s = 
    let hasLetters s = String.exists (Char.IsLetter) s
    let allowedChars = ['0'..'9'] @ ['(';')';'+';'-';' ';'.']
    let removeAllowed s = String.filter (fun c -> not (List.contains c allowedChars)) s
    let hasPunctuation s = s |> removeAllowed |> String.exists (Char.IsPunctuation)

    match s with 
    | s when hasLetters s -> Error "alphanumerics not permitted"
    | s when hasPunctuation s -> Error "punctuations not permitted"
    | _ -> Ok s

let filterDigits (s:string) = String.filter Char.IsDigit s
let addCountryCode (s:string) = if s.Length = 10 then "1" + s else s

let validateLength (s:string) =
    match s with
    | s when s.Length > 11 -> Error "more than 11 digits"
    | s when s.Length <> 11 -> Error "incorrect number of digits"
    | _ -> Ok s

let validateCountryCode (s:string) =
    if s.StartsWith("1") then Ok s else Error "11 digits must start with 1" 

let validateAreaCode (s:string) =
    let areaIndex = 1
    match s with 
    | s when s.[areaIndex] = '0' -> Error "area code cannot start with zero"
    | s when s.[areaIndex] = '1' -> Error "area code cannot start with one"
    | s -> Ok s

let validateExchangeCode (s:string) = 
    let exchangeIndex = 4
    match s with
    | s when s.[exchangeIndex] = '0' -> Error "exchange code cannot start with zero" 
    | s when s.[exchangeIndex] = '1' -> Error "exchange code cannot start with one" 
    | s -> Ok s

let toPhoneNumber (s:string) =
    s.[1..] |> Convert.ToUInt64

let clean (input:string) =
    Ok input
    |> Result.bind validateInput
    |> Result.map filterDigits
    |> Result.map addCountryCode
    |> Result.bind validateLength
    |> Result.bind validateCountryCode
    |> Result.bind validateAreaCode
    |> Result.bind validateExchangeCode
    |> Result.map toPhoneNumber
