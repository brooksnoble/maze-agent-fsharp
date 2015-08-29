open System
open Microsoft.FSharp.Core

open Maze
open Search
open Printing
open Parsing

let pause () =
    printf "Press any key to continue..."
    System.Console.ReadKey(true) |> ignore


let mainPlay maze =
    let rec play maze state =
        Console.Clear()
        printfn "%s" (sprintMazeState maze state)
        let key = Console.ReadKey(true)
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

let mainSearch maze frontierItemPicker pauseBetweenRuns =
    let initialState = getInitialState maze

    let diagnostic = fun (frontier: FrontierItem<MazeAction, MazeState> list) -> 
        Console.Clear()
        frontier
        |> List.map (fun x -> x.State)
        |> sprintMazeStates maze (fun i -> 'o')
        |> printfn "Frontier States:\n%s"

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
    
    if pauseBetweenRuns then pause ()
    result

let (@@) str1 str2 = System.IO.Path.Combine(str1, str2)

let loadMazeFile filename = 
    let mazeText = System.IO.File.OpenText(".." @@ ".." @@ ".." @@ ".." @@ "mazes" @@ filename).ReadToEndAsync() |> Async.AwaitTask |> Async.RunSynchronously
    parseMaze mazeText

[<EntryPoint>]
let main argv = 
    let mazeFiles = [| 
        ("Small", "smallMaze.txt");
        ("Medium", "mediumMaze.txt");
        ("Big", "bigMaze.txt")
    |]

    let mazes = mazeFiles |> Array.map (fun (l,f) -> l, loadMazeFile f)
    
    let searchStrategies = [| 
        ("Depth First", depthFirst)
        ("Breadth First", breadthFirst)
    |]
    
    let results = 
        mazes 
        |> Array.map (fun (mazeName, maze) -> (mazeName, searchStrategies |> Array.map (fun (strategyName, strategy) -> (strategyName, mainSearch maze strategy false))))

    Console.Clear()

    for (mazeName, subResults) in results do
        printfn "%s" mazeName

        for (strategyName, result) in subResults do
            printfn "\t%s" strategyName
            printfn "\t\tAlgorithm Steps Taken: %d" result.StepsTaken
            match result.Solution with
            | None ->
                printfn "\t\tNo Solution Found"
            | Some solution ->
                printfn "\t\tSolution Found"
                printfn "\t\tCost: %d" solution.CostToReach

    pause ()

    0 // return an integer exit code
