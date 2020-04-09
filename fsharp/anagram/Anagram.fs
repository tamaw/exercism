module Anagram
open System.Collections.Generic

let toLower (s:string) = s.ToLower()
let toCharArray (s:string) = s.ToCharArray()
let toSortedString s = s |> toLower |> toCharArray |> Array.sort |> System.String

let findAnagrams (sources: string list) (target:string) = 
    let compare w t =
        match (toLower w, toLower t) with
            | (w,t) when w = t -> false
            | _ -> toSortedString w = toSortedString t

    List.filter (fun a -> compare a target) sources