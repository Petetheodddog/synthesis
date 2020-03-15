module Synthesis

let abelar a =
    (a > 12) && (a < 3097) && (a % 12 = 0)
    //failwith "Not implemented"

let area b h =
    match b < 0.0 || h < 0.0 with
    |true -> failwith "Base or Height is negative"
    |false -> (0.5 * b * h)
    //failwith "Not implemented"

let zollo x =
    match x < 0 with
    |true -> (x * -1)
    |false -> (x * 2)
    //failwith "Not implemented"

let min a b =
    match a > b with
    |true -> b
    |false -> a
    //failwith "Not implemented"

let max a b =
    match a > b with
    |true -> a
    |false -> b
    //failwith "Not implemented"

let ofTime h m s =
    ((h*3600) + (m*60) + s)
    //failwith "Not implemented"

let toTime t =
    match t < 0 with
    |true -> (0,0,0)
    |false -> 
        let h = (t / 3600) //hours
        let m = (t - (h*3600)) / 60 //mins
        let s = (t - (h*3600)) - (m*60) //secs
        (h,m,s)
    //failwith "Not implemented"

let digits a =
    let rec cnt j k =
        match j/10 = 0 with //div 10 will give the digits
        |true -> k 
        |false -> cnt (j/10) (k+1) 
    cnt a 1
    //failwith "Not implemented"

let minmax (a,b,c,d) = 
    let x = min (min a b) (min c d)
    let y = max (max a b) (max c d)
    (x,y)
    //failwith "Not implemented"

let isLeap a =
    match a < 1582 with
    |true -> failwith "Year less than 1582"
    |false -> match a%100 = 0 with 
              |true -> (a%4 = 0) && (a%400 = 0)
              |false -> (a%4 = 0)
    //failwith "Not implemented"

let month = function
    |1 -> ("January", 31)
    |2 -> ("February", 28)
    |3 -> ("March", 31)
    |4 -> ("April", 30)
    |5 -> ("May", 31)
    |6 -> ("June", 30)
    |7 -> ("July", 31)
    |8 -> ("August", 31)
    |9 -> ("September", 30)
    |10 -> ("October", 31)
    |11 -> ("November", 30)
    |12 -> ("December", 31)
    |_ -> failwith "Invalid input"

    //failwith "Not implemented"

let rec toBinary a =
    match a<0 with 
    |true -> failwith "Invalid input"
    |false -> match a with 
                | 0 | 1 -> string a 
                |_ -> 
                    let bit = string (a%2) 

                    (toBinary (a/2)) + bit
    //failwith "Not implemented"

let bizFuzz n =
    match n > 0 with 
    |false -> (0,0,0)
    |true -> (n/3,n/5,(n/3)/5)
    //failwith "Not implemented"

let monthDay d y = 
    let rec FindMonth days count leap = 
        let a,b = month count
        match days > b with
           | false -> a
           | true -> match leap = 1 && count = 1 with 
                        | false -> FindMonth (days-b) (count+1) leap
                        | true -> FindMonth (days-b-leap) (count+1) leap
    match isLeap y with
        | false -> 
            match d >= 1 && d <= 365 with
            | false -> failwith "Invalid day"
            | true -> FindMonth d 1 0
        | true -> 
            match d >= 1 && d <= 366 with
            | false -> failwith "Invalid day"
            | true -> FindMonth d 1 1
    //failwith "Not implemented"

              // float * float -> ((float * float -> float) * (float * float -> bool))
let coord c =
       // float * float -> 
    let brackets c1 c2 = 
        let n1,_ = c1
        let _,k1 = c1
        let n2,_ = c2
        let _,k2 = c2
        let moo = (((n1-n2)**2.0) , ((k1-k2)**2.0)) 
        (moo) 


    let dist myCoord = //this needs to take a tuple of f*f... (float * float -> float)
        let c1,c2 = brackets c myCoord
        let n = c1 + c2 
        let sqrt n =    //this will work if everything else does
            let rec calculate guess i =
                  match i with
                  | 10 -> guess
                  | _ ->
                      let g = (guess + n/guess) / 2.0
                      calculate g (i+1)
            match n <= 0.0 with
            | true -> failwith "Impossibru!"
            | _ ->
                calculate (n/2.0) 0
        sqrt n

    let topLeftC c1 c2 = 
        let x1,y1 = c1
        let x2,y2 = c2
        (min x1 x2, max y1 y2)

    let within myCoord = //Also tuple input, (float * float -> bool)
        let c1,c2 = topLeftC c myCoord
        //let c1,c2 = topLeftC c
        let x1,y1 = c1
        let x2,y2 = c2
        //let n = c1 + c2 
        //let x1,y1 = topLeftC
        //match c1 >= x1 && c2 >= y2 with 
        match fst(topLeftC c1 c2) >= x1 && snd(topLeftC c1 c2) >= y2 with 
        | true -> false
        | _ -> true

    (dist, within)  // ((float * float -> float) * (float * float -> bool))
