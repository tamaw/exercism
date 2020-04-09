module TwelveDays

let days = ["first";"second";"third";"fourth";"fifth";"sixth";"seventh";"eighth";"ninth";"tenth";"eleventh";"twelfth"]
let items = [
    "twelve Drummers Drumming"; "eleven Pipers Piping"; "ten Lords-a-Leaping"; "nine Ladies Dancing";
    "eight Maids-a-Milking"; "seven Swans-a-Swimming"; "six Geese-a-Laying"; "five Gold Rings";
    "four Calling Birds"; "three French Hens"; "two Turtle Doves";
]

let recite start stop =
    let singItems (i:string list) n = String.concat ", " i.[12 - n.. 10]
    let line n = match n with
                    | 1 -> "On the " + days.[n - 1] + " day of Christmas my true love gave to me: a Partridge in a Pear Tree."
                    | _ -> "On the " + days.[n - 1] + " day of Christmas my true love gave to me: " + singItems items n + ", and a Partridge in a Pear Tree."
    let rec sing start stop song = 
        let song = song @ [line start]
        if start = stop then song else sing (start + 1) stop song 

    sing start stop []