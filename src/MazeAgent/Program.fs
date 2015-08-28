open System
open Microsoft.FSharp.Core

open Maze
open Search
open Printing
open Parsing

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

let mainSearch maze frontierItemPicker title =
    let initialState = getInitialState maze

    let diagnostic = fun (frontier: FrontierItem<MazeAction, MazeState> list) -> 
        Console.Clear()
        frontier
        |> List.map (fun x -> x.State)
        |> sprintMazeStates maze (fun i -> 'o')
        |> printfn "Frontier States:\n%s"

        //System.Console.ReadLine() |> ignore
        System.Threading.Thread.Sleep(10)

    let result = search diagnostic frontierItemPicker initialState (handleAction maze) possibleMazeActions (fun x -> 1) (fun s -> s.Status = Done)
    Console.Clear()

    printfn "****************************** Results ******************************\n"
    printfn "Algorithm Steps Taken: %d" result.StepsTaken
    match result.Solution with
    | None -> 
        printfn "No Solution Found!"
    | Some fi ->
        let states = 
            fi.ActionsToReach
            |> List.scan (fun s a -> handleAction maze s a) initialState
        
        printfn "Solution Found!"
        printfn "Actions: %s" (fi.ActionsToReach |> List.map sprintAction |> String.concat "")
        printfn "Cost: %d" fi.CostToReach
        printfn "\n%s" (sprintMazeStates maze (fun i -> '.') states)
    System.Console.ReadLine() |> ignore

let loadMazeFile filename = 
    let mazeText = System.IO.File.OpenText("..\..\..\..\mazes\\" + filename).ReadToEndAsync() |> Async.AwaitTask |> Async.RunSynchronously
    parseMaze mazeText

[<EntryPoint>]
let main argv = 
    let mazes = 
        [ "smallMaze.txt"; "mediumMaze.txt"; "bigMaze.txt"]
        |> List.map loadMazeFile
    
    let searchStrategies = [ 
        ("Depth First"), depthFirst; 
        ("Breadth First"), breadthFirst
    ]

    for maze in mazes do
        for (title, strategy) in searchStrategies do
            mainSearch maze strategy title

    0 // return an integer exit code
