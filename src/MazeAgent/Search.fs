module Search

let depthFirst frontier = 
    frontier |> List.rev |> List.head

let breadthFirst frontier = 
    frontier |> List.head

type FrontierItem<'TAction, 'TState> = {
    ActionsToReach: 'TAction list
    CostToReach: int
    State: 'TState
}

type SearchResult<'TAction, 'TState> = {
    Solution: FrontierItem<'TAction, 'TState> option
    StepsTaken: int
}

let search (frontierDiagnostic: FrontierItem<'TAction, 'TState> list -> unit) 
           (pickFromFrontier: FrontierItem<'TAction, 'TState> list -> FrontierItem<'TAction, 'TState>)
           (initialState: 'TState) (handle: 'TState -> 'TAction -> 'TState) (possibleActions: 'TState -> 'TAction list) (costForAction: 'TAction -> int) (isGoalState: 'TState -> bool) =
    let expand frontierItem =
        let nextActions = possibleActions frontierItem.State
        nextActions 
        |> List.map (fun a -> { ActionsToReach = a::frontierItem.ActionsToReach; CostToReach = frontierItem.CostToReach + (costForAction a); State = handle frontierItem.State a })

    let rec inner frontier seenStates stepsTaken =
        frontierDiagnostic frontier // for display or logging frontier at each step
        match frontier with
        | [] -> { Solution = None; StepsTaken = stepsTaken } // empty frontier means we've exhausted all states, so there is no solution
        | frontier ->
            let pickedFrontierItem = pickFromFrontier frontier
            let restOfFrontierItems = frontier |> List.filter (fun i -> i <> pickedFrontierItem)

            if isGoalState pickedFrontierItem.State then { Solution = Some { pickedFrontierItem with ActionsToReach = List.rev pickedFrontierItem.ActionsToReach}; StepsTaken = stepsTaken }
            else
                let seenStates = (pickedFrontierItem.State::seenStates)
                let newFrontierItems = 
                    expand pickedFrontierItem
                    |> List.filter (fun fi -> seenStates |> List.exists (fun s -> s = fi.State) |> not)

                let frontier = List.append restOfFrontierItems newFrontierItems
                inner frontier seenStates (stepsTaken + 1)

    inner [ { ActionsToReach = []; CostToReach = 0; State = initialState } ] [] 0
