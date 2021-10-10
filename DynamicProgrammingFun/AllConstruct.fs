module AllConstructModule

open System.Collections.Generic
open Common

let rec allConstruct (target:string) (wordBank:string list) : string list list =
    match target with
    | "" -> [[]]
    | target ->
        let result = wordBank
                     |> List.map(fun word -> (word, getSuffixOrNothing target word))
                     |> List.choose(fun tup -> match snd(tup) with | Some v -> Some(fst(tup),v) | None -> None)
                     |> List.map(fun (word,suffix) ->
                                            allConstruct suffix wordBank |> List.map(fun i -> word::i)
                                            |> List.reduce(fun _ -> id))
        result

let runAllConstruct =
    printfn "%A" (allConstruct "abcdef" ["ab";"abc";"cd";"def";"abcd";"ef";"c"]) // [["ab";"cd";"ef"];["ab";"c";"def"];["abc";"def"];["abcd";"ef"]]
    printfn "%A" (allConstruct "hello" ["cat";"dog";"mouse"]) // []
    printfn "%A" (allConstruct "" ["cat";"dog";"mouse"]) // [[]]