namespace AgdaUnusedOpens.Internals

[<RequireQualifiedAccess>]
module internal Seq =
    let omit<'a> (n : int) (s : 'a seq) : 'a seq =
        seq {
            let enumerator = s.GetEnumerator ()
            let mutable i = 0
            while enumerator.MoveNext () do
                if i <> n then
                    yield enumerator.Current
                i <- i + 1
        }

module internal List =
    let replaceAt<'a> (n : int) (replacement : 'a) (s : 'a list) : 'a list =
        let arr = List.toArray s
        arr.[n] <- replacement
        Array.toList arr