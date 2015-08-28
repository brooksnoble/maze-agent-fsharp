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

let sprintMazeStates (maze: Maze2D) (mazeStates: MazeState list) =
    let mazeStr = mazeToString maze
    mazeStates 
    |> List.mapi (fun i s -> i, s.Position)
    |> List.fold (fun str (i, position) -> markPosition (maze.Length, maze.[0].Length) position ((int 'a') + i |> char) str) mazeStr

let possibleMazeActions state = [ MoveLeft; MoveRight; MoveUp; MoveDown ]

let constantCost action = 1

let search (frontierDiagnostic: ('TAction list * int * 'TState) list -> unit) 
           (initialState: 'TState) (handle: 'TState -> 'TAction -> 'TState) (possibleActions: 'TState -> 'TAction list) (costFunction: 'TAction -> int) (hasReachedGoal: 'TState -> bool) =
    let expand state =
        let nextActions = possibleActions state
        nextActions |> List.map (fun a -> a, costFunction a, handle state a)

    let rec inner frontier seenStates =
        frontierDiagnostic frontier // for display or logging
        match frontier with
        | [] -> failwith "empty frontier"
        | (prevActions, currentCost, nextState)::rest ->
            if hasReachedGoal nextState then (nextState, currentCost, prevActions)
            else
                let seenStates = (nextState::seenStates)
                let blah = 
                    expand nextState
                    |> List.filter (fun (a, cost, state) -> List.exists (fun s -> s = state) seenStates |> not)
                    |> List.map (fun (a, cost, state) -> (a::prevActions, cost+currentCost, state))

                let frontier = List.append blah rest 
                inner frontier seenStates

    inner [ ([], 0, initialState) ] []

let mainGo maze actions =
    let rec execActions maze state actions =
        match actions with
        | [] -> state
        | (action::rest) -> 
            let state = handleAction maze state action
            printfn "Maze State After %A:\n\n%s" action (sprintMazeState maze state)
            System.Console.ReadLine() |> ignore
            execActions maze state rest

    let initialState = getInitialState maze
    execActions maze initialState actions

let mainPlay maze =
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
        
    let initialState = getInitialState maze

    play maze initialState

let mainSearch maze =
    let initialState = getInitialState maze

    let diagnostic1 = fun frontier -> 
        Console.Clear()
        printfn "Frontier:\n"
        frontier
        |> List.mapi (fun i x -> i,x) |> List.filter (fun (i,x) -> i < 4) |> List.map snd
        |> List.iteri (fun i (actions, cost, state) -> 
            printfn "Option %d:" i
            printfn "Actions: %s" (List.rev actions |> List.map sprintAction |> String.concat "")
            printfn "Cost: %d" cost
            printfn "State:\n%s" (sprintMazeState maze state)
        )
        System.Console.ReadLine() |> ignore

    let diagnostic = fun frontier -> 
        Console.Clear()
        frontier
        |> List.map (fun (_, _, state) -> state)
        |> sprintMazeStates maze
        |> printfn "Frontier States:\n%s"

        System.Console.ReadLine() |> ignore

    let (goalState, cost, actions) = search diagnostic initialState (handleAction maze) possibleMazeActions (fun x -> 1) (fun s -> s.Status = Done)

    Console.Clear()
    printfn "GOAL Reached!"
    printfn "Actions: %s" (List.rev actions |> List.map sprintAction |> String.concat "")
    printfn "Cost: %d" cost
    printfn "State:\n%s" (sprintMazeState maze goalState)
    System.Console.ReadLine() |> ignore

    ()

[<EntryPoint>]
let main argv = 
    let mazeText = System.IO.File.OpenText("..\..\..\..\mazes\smallMaze.txt").ReadToEndAsync() |> Async.AwaitTask |> Async.RunSynchronously
    
    let maze = parseMaze mazeText
    printfn "Maze Loaded:\n\n%s" (maze |> mazeToString)
    System.Console.ReadLine() |> ignore

    mainSearch maze

    0 // return an integer exit code
