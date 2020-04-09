module RobotName
open System
open System.Security.Cryptography

type Robot = { Name:string }

let rnd = new RNGCryptoServiceProvider()
let getBytes n =
    let b = Array.zeroCreate<byte> n
    rnd.GetBytes b
    b

let getLetters n =
    getBytes n |> Array.map (fun x -> char ((x % 24uy) + 65uy))
let getNumbers n =
    getBytes n |> Array.map (fun x -> char ((x % 10uy) + 48uy))
let getRobotName() =
    ((getLetters 2) |> System.String) + ((getNumbers 3) |> System.String)

let mkRobot() = { Name = getRobotName()}
let name robot = robot.Name
let reset robot = mkRobot()
