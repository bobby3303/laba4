open System
let rnd = Random()

type STree = 
    | Empty
    | Node of string * STree * STree

let rec insert tree x =
    match tree with
    | Empty -> Node(x, Empty, Empty)
    | Node(v, l, r) ->
        if x < v then Node(v, insert l x, r)
        elif x > v then Node(v, l, insert r x)
        else Node(v, l, r) 

let gen () =
    let chars = "abcdefgABCDEFG0123456789!@#$%^&*"
    let len = rnd.Next(0, 8)
    if len = 0 then 
        "" 
    else 
        [| for _ in 1..len -> chars.[rnd.Next(chars.Length)] |]
        |> String

let rec genTree n tree =
    if n = 0 then 
        tree
    else
        let value = gen() 
        genTree (n - 1) (insert tree value)

let rec fold f acc tree =
    match tree with
    | Empty -> acc
    | Node(v, l, r) ->
        let accLeft = fold f acc l
        let accCur = f accLeft v
        fold f accCur r


let count (c: char) n (s: string) =
    if s.Contains(c) then n else n + 1

let rec print d tree =
    match tree with
    | Empty -> ()
    | Node(s, l, r) ->
        let indent = String.replicate d "  "
        let outStr = 
            if s = "" then 
                "\"\"" 
            else 
                s
        printfn "%s|-- %s" indent outStr
        print(d + 1) l
        print (d + 1) r

let rec getSymb () =
    printf "Введите символ для поиска: "
    let input = Console.ReadLine()
    if String.IsNullOrEmpty(input) then
        printfn "Ошибка: вы ничего \
         не ввели. Попробуйте снова."
        getSymb()
    else
        input.[0] 


[<EntryPoint>]
let main argv =
    let source = genTree (rnd.Next(0, 24)) Empty
    
    if source = Empty then
        printfn "Дерево пустое, попробуйте запустить еще раз"
    else
        printfn "Сгенерированное дерево строк:"
        print 0 source
        
        printfn "\n--------------------------------------"
        let symb = getSymb()
        let result = fold (count symb) 0 source

        printfn "\nКоличество узлов, в которых \
         НЕТ символа '%c': %d" symb result
    0
