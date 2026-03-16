open System
let rnd = Random()

type Tree = 
    | Empty
    | Node of int * Tree * Tree

let rec dice d =
    if d > 7 then 
        Empty

    else
        let threshold = 
            if d < 3 then 2    
            elif d < 5 then 8  
            else 15     
            
        if rnd.Next(1, 21) > threshold then
            let v = rnd.Next(1,10001)
            Node (v, dice(d + 1), dice(d + 1))
        else
            Empty

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

let rec mapTree tree =
    match tree with
    | Empty ->
        Empty
    | Node(v, l, r) ->
        let newV = upDigit v
        let newL = mapTree l
        let newR = mapTree r

        Node (newV, newL, newR)

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
    let source = dice 1
    
    if source = Empty then 
        printfn "Дерево пустое, попробуйте запустить еще раз"
    else
        printfn "ИСХОДНОЕ ДЕРЕВО:"
        print 0 source

        // Применяем мап
        let result = mapTree source
        printfn "\nОБРАБОТАННОЕ ДЕРЕВО (разряды +1, кроме 9):"
        print 0 result

    0


