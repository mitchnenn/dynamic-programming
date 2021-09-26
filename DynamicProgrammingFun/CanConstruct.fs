module CanConstructModule

open System.Collections.Generic

let getSuffixOrNothing (target:string) (prefix:string) : Option<string> =
    match target.IndexOf(prefix) with
    | 0 -> Some(target.Substring(prefix.Length))
    | _ -> None

let canConstruct (target:string) (words:string list) : bool =
    let rec canConstruct' (target:string) (memo:Dictionary<string,bool>) : bool =
        match target with
        | target when memo.ContainsKey(target) -> memo.[target]
        | "" -> memo.Add(target, true); true
        | _ ->
            let result =  words
                          |> List.map (getSuffixOrNothing target)
                          |> List.choose id
                          |> List.exists (fun suffix -> true = canConstruct' suffix memo)
            memo.Add(target, result)
            result
    canConstruct' target (Dictionary<string,bool>())

let runCanConstruct =
    printfn "%b" (canConstruct "abcdef" ["ab";"abc";"cd";"def";"abcd"]) // true
    printfn "%b" (canConstruct "skateboard" ["bo";"rd";"ate";"t";"ska";"sk";"boar"]) // false
    printfn "%b" (canConstruct "" ["whate";"ever"]) // true
    printfn "%b" (canConstruct "enterpotentpot" ["a";"p";"ent";"enter";"ot";"o";"t"]) // true
    printfn "%b" (canConstruct "eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeef" ["e";"ee";"eee";"eeee";"eeeee";"eeeeee"])
