type Permutation<'T>(range : 'T[], perm : 'T[]) =
    member this.Perm = perm
    member this.Range = range
    (*new (range : 'T[], cycles : 'T[][]) =
        let indexOf (elm : 'a) (arr : 'a[]) : int =
            let mutable index = -1
            for i = 0 to arr.Length - 1 do
                if elm = arr.[i] then
                    index <- i
            index
        let mutable perm : 'T[] = Array.copy range
        for i = 0 to cycles.Length - 1 do
            for k = 0 to cycles.[i].Length - 2 do
                perm.[(indexOf cycles.[i].[k] range)] <- cycles.[i].[k + 1]
            //perm.[cycles.[i].[cycles.[i].Length - 1] - 1] <- cycles.[i].[0]
        Permutation<'T>(range, perm)*)
    member this.PermutationOf(elm : 'T) =
        let mutable res = Unchecked.defaultof<'T>
        for i = 0 to this.Range.Length - 1 do
            if (this.Range.[i].ToString() = elm.ToString()) then
                res <- this.Perm.[i]
        res

    override this.ToString() =
        sprintf "%A\n%A" this.Range this.Perm

    static member (*) (a : Permutation<'T>, b : Permutation<'T>) =
        let mutable newPerm = Array.copy b.Range
        for i = 0 to b.Range.Length - 1 do
            newPerm.[i] <- a.PermutationOf(b.PermutationOf(b.Range.[i]))
        Permutation<'T>(b.Range, newPerm)

    member this.Inverse() =
        let mutable newPerm = Array.copy this.Range
        for i = 0 to this.Range.Length - 1 do
            newPerm.[i] <- this.PermutationOf(this.PermutationOf(this.Perm.[i]))
        Permutation<'T>(this.Range, newPerm)


(*let p = Permutation([|1;2;3|], [|3;2;1|])
printfn "%A" p
printfn "p(1) = %A" (p.PermutationOf(1))
let p2 = Permutation([|1;2;3;4;5|], [|[|2;3|];[|4;1|]|])
printfn "%A" p2
printfn "p2(1) = %A" (p2.PermutationOf(1))*)

let p1 = Permutation<int>([|1;2;3;4;5;6|], [|3;4;1;2;6;5|])
let p2 = Permutation<int>([|1;2;3;4;5;6|], [|2;3;1;5;4;6|])
printfn "("
printfn "%A" p1
printfn "*"
printfn "%A" p2
printfn ")"
printfn "*"
printfn "%A" p2
printfn "="
printfn "%A" ((p2 * p1) * p2)
printfn "\n"
printfn "%A" (p1.Inverse())
printfn "p1(2) = %A" (p1.PermutationOf(2))
let charPerm = Permutation<char>([|'a';'b';'c'|], [|'b';'c';'a'|])
printfn "\nInverse of \n%A \n = \n%A" (charPerm) (charPerm.Inverse())
