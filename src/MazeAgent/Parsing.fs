module Parsing

open Maze

let parseMazeSquare = function 
    | '%' -> Wall
    | ' ' -> Empty
    | 'S' -> Start
    | 'G' -> Goal
    | _ -> failwith "unrecognized maze character"

let parseMaze (str: string): Maze2D =
    str.Split('\n')
    |> Array.map (fun s -> s.ToCharArray())
    |> Array.map (Array.map (parseMazeSquare))

