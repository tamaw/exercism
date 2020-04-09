module Gigasecond

open System

let add (beginDate: DateTime) =
    beginDate.Ticks + int64(pown 10 9) * 10000000L |> DateTime


