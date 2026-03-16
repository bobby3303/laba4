open System
let rnd = Random()

type STree = 
    | Empty
    | Node of string * STree * STree

let rec getSymb () =
    printf "Введите символ для поиска: "
    let input = Console.ReadLine()
    if String.IsNullOrEmpty(input) then
        printfn "Ошибка: вы ничего \
         не ввели. Попробуйте снова."
        getSymb()
    else
        input.[0] 

let gen () =
    let chars = "abcdefgABCDEFG0123456789!@#$%^&*"
    let len = rnd.Next(0, 8)
    if len = 0 then 
        "" 
    else 
        [| for _ in 1..len -> chars.[rnd.Next(chars.Length)] |]
        |> String

let rec dice d =
    if d > 7 then 
        Empty

    else
        let threshold = 
            if d < 3 then 2    
            elif d < 5 then 8  
            else 15    
            
        if rnd.Next(1, 21) > threshold then
            let v = gen ()
            Node (v, dice(d + 1), dice(d + 1))
        else
            Empty

let rec count (c: char) tree =
    match tree with
    | Empty -> 0
    | Node(s, l, r) ->
        let cur = 
            if s.Contains(c) then 
                0 
            else 
                1
        cur + (count c l) + (count c r)

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


[<EntryPoint>]
let main argv =
    let source = dice 1
    
    if source = Empty then
        printfn "Дерево пустое, попробуйте запустить еще раз"
    else
        printfn "Сгенерированное дерево строк:"
        print 0 source
        
        printfn "\n--------------------------------------"
        let symb = getSymb()
        let result = count symb source

        printfn "\nКоличество узлов, в которых \
         НЕТ символа '%c': %d" symb result
    0
