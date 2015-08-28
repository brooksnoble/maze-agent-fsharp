module Printing

open Maze

let mazeSquareToString = function
    | Wall -> "X"
    | Empty -> " "
    | Start -> "S"
    | Goal -> "G"

let mazeToString (maze: Maze2D) = 
    maze
    |> Array.map (Array.map mazeSquareToString)
    |> Array.map (String.concat "")
    |> String.concat "\n"


let sprintAction = function
    | MoveUp -> "U"
    | MoveDown -> "D"
    | MoveLeft -> "L"
    | MoveRight -> "R"

// figure out where to insert position marker in unrolled string
let markPosition size position (label: char) (str: string) = 
    let position = ((snd size) + 1) * (fst position) + (snd position)
    str.ToCharArray() |> Array.mapi (fun i x -> if i = position then label else x) |> fun x -> System.String x

let sprintMazeState (maze: Maze2D) (mazeState: MazeState) =
    let mazeStr = mazeToString maze
    let gridStr = mazeStr |> markPosition (maze.Length, maze.[0].Length) mazeState.Position 'O'
    gridStr + "\n" + (match mazeState.Status with | NotDone -> "Not Done" | Done -> "Done!")

let sprintMazeStates (maze: Maze2D) (labelFunc: int -> char) (mazeStates: MazeState list) =
    let mazeStr = mazeToString maze
    mazeStates 
    |> List.mapi (fun i s -> i, s.Position)
    |> List.fold (fun str (i, position) -> markPosition (maze.Length, maze.[0].Length) position (labelFunc i) str) mazeStr
