module Acronym

let abbreviate (phrase:string) = 
    phrase.Split ([|' '; '-'|], System.StringSplitOptions.RemoveEmptyEntries)
        |> Array.map (fun (e:string) -> e.Substring(0,1).ToUpper())
        |> System.String.Concat