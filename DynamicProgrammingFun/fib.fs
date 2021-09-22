module FibModule

open System
open System.Collections.Generic

let big (value : int) : int64 = Convert.ToInt64(value)

let rec fib (n:int) : int64 =
    let rec fib' (n:int64) (memo:Dictionary<Int64, int64>) : int64 =
        match n with
        | n when (memo.ContainsKey(n)) -> memo.[n]
        | 1L | 2L -> memo.Add(n,1L); 1L
        | _ ->
            let r = (fib' (n - 2L) memo) + (fib' (n - 1L) memo)
            memo.Add(n,r)
            r
    fib' (big n) (Dictionary<int64, int64>())

let runFib =
    printfn "fib: %d" (fib 6)
    printfn "fib: %d" (fib 8)
    printfn "fib: %d" (fib 50)

