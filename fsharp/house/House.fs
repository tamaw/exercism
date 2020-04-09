module House

// let nouns = ["house";"malt";"rat";"cat";"dog";"cow";"maiden";"man";"priest";"rooster";"farmer";"horse"]
// let premodifiers = ["lay in";"ate";"killed";"worried";"tossed";"milked";"kissed";"married";"woke";"kept";"belonged to"]
// let postmodifiers = ["with the crumpled horn"; "all forlorn"; "all tattered and torn"; "all shaven and shorn"; "that crowed in the morn"; "sowing his corn"; "and the hound and the horn" ]

let nouns = ["house";"malt";"rat";"cat";"dog";"cow";"maiden";"man";"priest";"rooster";"farmer";"horse"]
let premodifiers = ["";"lay in";"ate";"killed";"worried";"tossed";"milked";"kissed";"married";"woke";"kept";"belonged to"]
let postmodifiers = ["";"";"";"";"";"with the crumpled horn"; "all forlorn"; "all tattered and torn"; "all shaven and shorn"; "that crowed in the morn"; "sowing his corn"; "and the hound and the horn" ]


// recurison and embedding

// let a = List.zip ["malt";"rat";"cat"] ["lay in";"ate";"killed"];;
// let (x,y) = a.[0];;
// List.map (fun (x,y) -> sprintf "%s that %s the" x y) a;;

(*

 {This is the} <horse> [and the hound and the horn]
 that [belonged to] the <farmer> [sowing his corn]
 that [kept] the <rooster> [that crowed in the morn]
 that [woke] the <priest> [all shaven and shorn]
 that [married] the <man> [all tattered and torn]
 that [kissed] the <maiden> [all forlorn]
 that [milked] the <cow> [with the crumpled horn]
 that [tossed] the <dog>
 that [worried] the <cat>
 that [killed] the <rat>
 that [ate] the <malt>
 that [lay in] the <house>
 {that Jack built.}

*)

// List.init 3 (fun e -> "") ;;

//    [ "This is the cat that killed the rat that ate the malt that lay in the house that Jack built.";
//       "This is the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.";
//       "This is the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.";
//       "This is the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built.";
//       "This is the man all tattered and torn that kissed the maiden all forlorn that milked the cow with the crumpled horn that tossed the dog that worried the cat that killed the rat that ate the malt that lay in the house that Jack built." ]
// recite 4 8 |> should equal expected

let padLeft (s: string list) (n: int) = 
    List.init n (fun _ -> "") @ s

let recite startVerse endVerse: string list = 
    let realsong = [[]]
    let song2 = List.zip3 nouns premodifiers postmodifiers

    let embed s =
        match s with
        | (noun, "", "") -> sprintf "%s" noun
        | (noun, premodifier, "") -> sprintf "%s that %s the" noun premodifier
        | (noun, premodifier, postmodifier) -> sprintf "%s %s that %s the" noun postmodifier premodifier

    // let rec recurison s c =
    //     match c with
    //     | 0 -> s
    //     | _ -> recurison (s @ [embed song2.[c - 1]]) (c - 1)

    let rec recurison2 s sv ev i =
        match (sv,ev) with
        | (_, 0) -> s
        | (_, _) when sv = ev -> recurison (s @ [embed song2.[c - 1]]) (c - 1)

    let song = ["This is the"]
    let song = recurison song startVerse 
    let song = song @ ["that Jack built."]

    [String.concat " " song]



    // let embed2 c =
    //     match c with
    //     | c when c - 2 >= 4 -> sprintf "%s %s that %s the" nouns.[c - 1] postmodifiers.[c - 6] premodifiers.[c - 2]
    //     | c when c - 2 >= 0 -> sprintf "%s that %s the" nouns.[c - 1]  premodifiers.[c - 2]
    //     | c -> sprintf "%s" nouns.[c - 1]

    // let rec recurison s c =
        // if c = 0 then s else recurison (s @ [embed2 c]) (c - 1)
    // let song = recurison song startVerse

    // let rec recurison s c =
    //     if c = 0 then s else recurison (s @ [embed song2.[c - 1]]) (c - 1)
   