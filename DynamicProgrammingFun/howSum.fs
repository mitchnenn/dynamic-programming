module HowSumModule

open System.Collections.Generic

// Time: O(n^m * m) --> O(n * m^2)
// Space: O(m) --> O(m^2)

let rec howSome (targetSum:int) (nums:int list) : Option<int list> =
    let rec howSome' (targetSum:int) (memo:Dictionary<int,Option<int list>>) =
        match targetSum with
        | targetSum when memo.ContainsKey(targetSum) -> memo.[targetSum]
        | 0 -> memo.Add(targetSum, Some []); Some []
        | targetSum when targetSum < 0 -> memo.Add(targetSum, None); None
        | _ ->
            let result = nums
                         |> List.map(fun n -> (n, howSome' (targetSum - n) memo))
                         |> List.tryPick(fun (n,o) -> if o.IsSome then Some(n::o.Value) else None)
            memo.Add(targetSum, result)
            result
    howSome' targetSum (Dictionary<int,Option<int list>>())

let runHowSum =
    printfn $"howSum {howSome 7 [5;4;3;7]}" // None
    printfn $"howSum {howSome 8 [2;3;5]}" // Some [2;2;2;2]
    printfn $"howSum {howSome 7 [2;4]}" // None
    printfn $"howSum {howSome 0 [1;2;3]}" // []
    printfn $"howSum {howSome 300 [7;14]}" // []
