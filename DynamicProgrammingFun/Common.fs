module Common

let getSuffixOrNothing (target:string) (prefix:string) : Option<string> =
    match target.IndexOf(prefix) with
    | 0 -> Some(target.Substring(prefix.Length))
    | _ -> None
