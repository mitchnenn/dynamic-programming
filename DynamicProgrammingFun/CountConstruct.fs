module CountConstructModule

open System.Collections.Generic
open Common

let countConstruct (target:string) (words:string list) : int =
    let rec countConstruct' (target:string) (memo:Dictionary<string,int>) : int =
        match target with
        | target when memo.ContainsKey(target) -> memo.[target]
        | "" -> memo.Add(target, 1); 1
        | _ ->
            let result = words
                         |> List.map (getSuffixOrNothing target)
                         |> List.choose id
                         |> List.sumBy (fun suffix -> countConstruct' suffix memo)
            memo.Add(target, result)
            result
    countConstruct' target (Dictionary<string,int>())

let runCountConstruct =
    printfn "%d" (countConstruct "abcdef" ["ab";"abc";"cd";"def";"abcd"]) // 1
    printfn "%d" (countConstruct "purple" ["purp";"p";"ur";"le";"purpl"]) // 2
    printfn "%d" (countConstruct "skateboard" ["bo";"rd";"ate";"t";"ska";"sk";"boar"]) // 0
    printfn "%d" (countConstruct "" ["whate";"ever"]) // 1
    printfn "%d" (countConstruct "enterpotentpot" ["a";"p";"ent";"enter";"ot";"o";"t"]) // 4
    printfn "%d" (countConstruct "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeef" ["e";"ee";"eee";"eeee";"eeeee";"eeeeee"]) // 0
