module Maze

type MazeSquare =
    | Wall
    | Empty
    | Start
    | Goal

type Maze2D = MazeSquare array array

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

let possibleMazeActions state = [ MoveLeft; MoveRight; MoveUp; MoveDown ]

let constantCost action = 1
