open System
let rnd = Random()

type Tree = 
    | Empty
    | Node of int * Tree * Tree

let rec insert tree x =
    match tree with
    | Empty -> 
        Node(x, Empty, Empty) 
    | Node(v, l, r) ->
        if x < v then 
            Node(v, insert l x, r)   
        elif x > v then 
            Node(v, l, insert r x) 
        else 
            Node(v, l, r) 

let rec genTree n tree =
    if n = 0 then 
        tree 
    else
        let value = rnd.Next(1, 10001)
        genTree (n - 1) (insert tree value)

let rec upDigit n =
    if n < 10 then
        if n = 9 then
            9
        else
            n + 1
    else 
        let digit = n % 10
        let remain = n / 10

        let newDigit = 
            if digit = 9 then 
                9 
            else 
                digit + 1

        (upDigit remain) * 10 + newDigit

let rec map f tree =
    match tree with
    | Empty -> Empty
    | Node(v, l, r) ->
        let newVal = f v
        Node(newVal, map f l, map f r)

let rec print d tree =
    match tree with
    | Empty -> ()
    | Node(v, l, r) ->
        let indent = String.replicate d "  "
        printfn "%s|-- %d" indent v
        
        print (d + 1) l
        print (d + 1) r



[<EntryPoint>]
let main args =
    let source = genTree (rnd.Next(0, 24)) Empty
    
    if source = Empty then 
        printfn "Дерево пустое, попробуйте запустить еще раз"
    else
        printfn "ИСХОДНОЕ ДЕРЕВО:"
        print 0 source

        // Применяем мап
        let result = map upDigit source
        printfn "\nОБРАБОТАННОЕ ДЕРЕВО (разряды +1, кроме 9):"
        print 0 result

    0

