module RnaTranscription

let toRna (dna: string): string = 
    let transform (s: string) =
        let convert c =
            match c with
            | 'G' -> 'C'
            | 'C' -> 'G'
            | 'T' -> 'A'
            | 'A' -> 'U'
            | _ -> failwith "no match"
        Array.map convert (s.ToCharArray ()) |> System.String

    transform dna 