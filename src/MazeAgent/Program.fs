open System
open Microsoft.FSharp.Core

type MazeSquare =
    | Wall
    | Empty
    | Start
    | Goal

type Maze2D = MazeSquare array array

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

type MazeStateStatus = 
    | NotDone
    | Done

type MazeState = {
    Status: MazeStateStatus 
    Position: int * int
}

type MazeAction =
    | MoveUp
    | MoveDown
    | MoveLeft
    | MoveRight

let getInitialState (maze: Maze2D) =
    let initialPosition =
        let startPositions = 
            maze
            |> Array.mapi (fun i x -> x |> Array.mapi (fun j x -> j, x = Start) |> Array.choose (function | (idx, true) -> Some (i, idx) | (idx, false) -> None))
            |> Array.collect id
        
        startPositions.[0] // assume only one start position

    { Position = initialPosition; Status = NotDone }

let handleAction (maze: Maze2D) (state: MazeState) (action: MazeAction) =
    match state.Status with
    | Done -> state // can't do anything once done
    | NotDone ->
        let displacement = 
            match action with
            | MoveUp -> (-1,0)
            | MoveDown -> (1,0)
            | MoveLeft -> (0,-1)
            | MoveRight -> (0,1)

        let newPosition =
            let desiredNewPosition = (fst state.Position + fst displacement, snd state.Position + snd displacement)

            let allowed =
                if fst desiredNewPosition < 0 || fst desiredNewPosition >= maze.Length then false
                else
                    if snd desiredNewPosition < 0 || snd desiredNewPosition >= maze.[0].Length then false
                    else
                        maze.[fst desiredNewPosition].[snd desiredNewPosition] <> Wall

            if allowed then desiredNewPosition else state.Position

        let newStatus =
            match maze.[fst newPosition].[snd newPosition] with
            | Goal -> Done
            | _ -> NotDone

        { Position = newPosition; Status = newStatus }

let sprintMazeState (maze: Maze2D) (mazeState: MazeState) =
    let mazeStr = mazeToString maze
    // figure out where to insert position marker in unrolled string
    let position = (maze.[0].Length + 1) * (fst mazeState.Position) + (snd mazeState.Position)
    let mazeStr = mazeStr.ToCharArray() |> Array.mapi (fun i x -> if i = position then 'O' else x) |> fun x -> System.String x
    mazeStr + "\n" + (match mazeState.Status with | NotDone -> "Not Done" | Done -> "Done!")

[<EntryPoint>]
let main argv = 
    let mazeText = System.IO.File.OpenText("..\..\..\..\mazes\smallMaze.txt").ReadToEndAsync() |> Async.AwaitTask |> Async.RunSynchronously
    
    let maze = parseMaze mazeText
    printfn "Maze:\n\n%s" (maze |> mazeToString)

    let initialState = getInitialState maze

    let rec execActions maze state actions =
        match actions with
        | [] -> state
        | (action::rest) -> 
            let state = handleAction maze state action
            printfn "Maze State After %A:\n\n%s" action (sprintMazeState maze state)
            System.Console.ReadLine() |> ignore
            execActions maze state rest

    let rec play maze state =
        Console.Clear()
        printfn "%s" (sprintMazeState maze state)
        let key = System.Console.ReadKey(true)
        let action =
            match key.Key with 
            | ConsoleKey.DownArrow -> Some MoveDown
            | ConsoleKey.UpArrow -> Some MoveUp
            | ConsoleKey.LeftArrow -> Some MoveLeft
            | ConsoleKey.RightArrow -> Some MoveRight
            | _ -> None

        let newState = 
            match action with
            | Some action -> handleAction maze state action
            | None -> state

        play maze newState

    play maze initialState

    0 // return an integer exit code
