module BestSumModule

open System.Collections.Generic

let bestSum (targetSum:int) (nums:int list) : Option<int list> =
    let rec bestSum' (targetSum:int) (memo:Dictionary<int,Option<int list>>) : Option<int list> =
        match targetSum with
        | targetSum when memo.ContainsKey(targetSum) -> memo.[targetSum]
        | 0 -> memo.Add(targetSum, Some []); Some []
        | targetSum when targetSum < 0 -> memo.Add(targetSum, None); None
        | _ ->
            let result = nums
                         |> List.map(fun n -> (n, bestSum' (targetSum - n) memo))
                         |> List.map(fun (n,o) -> if o.IsSome then Some (n::o.Value) else None)
                         |> List.choose id
                         |> List.sortBy (fun l -> l.Length)
                         |> List.tryHead
            memo.Add(targetSum, result)
            result
    bestSum' targetSum (Dictionary<int,Option<int list>>())

let runBestSum =
    printfn $"bestSum {bestSum 7 [5;3;4;7]}" // [7]
    printfn $"bestSum {bestSum 8 [2;3;5]}" // [3;5]
    printfn $"bestSum {bestSum 8 [1;4;5]}" // [4;4]
    printfn $"bestSum {bestSum 100 [1;2;5;25]}" // [25;25;25;25]
