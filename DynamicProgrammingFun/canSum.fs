module CanSumModule

open System.Collections.Generic

let canSum (targetSum:int) (nums:int list) : bool =
    let rec canSum' (targetSum:int) (memo:Dictionary<int,bool>) : bool =
        match targetSum with
        | targetSum when (memo.ContainsKey targetSum) -> memo.[targetSum]
        | 0 -> memo.Add(targetSum, true); true
        | targetSum when targetSum < 0 -> memo.Add(targetSum, false); false
        | _ ->
            let result = nums
                         |> List.exists(fun n -> true = canSum' (targetSum - n) memo)
            memo.Add(targetSum, result)
            result
    canSum' targetSum (Dictionary<int,bool>())

let runCanSum =
    printfn $"canSum: %A{canSum 7 [2;3]}" // false
    printfn $"canSum: %A{canSum 7 [5;3;4;7]}" // true
    printfn $"canSum: %A{canSum 7 [2;4]}" // false
    printfn $"canSum: %A{canSum 8 [5;3;2]}" // true
    printfn $"canSum: %A{canSum 300 [7;14]}" // false
