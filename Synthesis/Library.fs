module Synthesis

let abelar n = n > 12 && n < 3097 && n % 12 = 0

let area a b = match a >= 0.0 && b >= 0.0 with
    |true -> a * b * 0.5
    |_ -> failwith "either base or height is negative"

let zollo n = match n < 0 with 
    |true -> n * -1
    |_ -> n * 2

let min a b  = match a < b with 
    |true -> a
    |_ -> b

let max a b = match a > b with 
    |true -> a
    |_ -> b

let ofTime h m s = (h * 3600) + (m * 60) + s

let toTime t = match t >= 0 with
    |true ->    let m = t / 60 in   
                    let s = t % 60 in
                        let h = m / 60 in
                            let m = m % 60 in 
                                h,m,s
    |_ -> 0,0,0

let digits n = let rec numdigits a c =
                   match (a / 10) = 0 with
                    |true -> c
                    |_ -> numdigits (a / 10) (c + 1) 
               match (n < 10 && n > -10) with 
                |true -> 1
                |_ -> numdigits n 1

let minmax _ =
    failwith "Not implemented"

let isLeap y = match y < 1582 with 
                |true -> failwith "Year is less than 1582"
                |_ -> match y % 100 = 0 with 
                    |true -> match y % 400 = 0 with
                        |true -> true
                        |_ -> false
                    |false -> (y % 4) = 0

                    
    

let month _ =
    failwith "Not implemented"

let toBinary _ =
    failwith "Not implemented"

let bizFuzz _ =
    failwith "Not implemented"

let monthDay _ _ =
    failwith "Not implemented"

let coord _ =
    failwith "Not implemented"