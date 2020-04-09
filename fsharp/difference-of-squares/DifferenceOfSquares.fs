module DifferenceOfSquares
let squareOfSum number: int = 
    pown (List.sum [1 .. number]) 2

let sumOfSquares number: int = 
    List.sumBy (fun e -> pown e 2) [1 .. number]

let differenceOfSquares number: int = 
    squareOfSum number - sumOfSquares number
