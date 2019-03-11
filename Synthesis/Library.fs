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

let minmax (a,b,c,d) = 
    let rec func (a,b,c,d) =
        match a <= b && a <= c && a <= d  with
            |true -> match d >= a && d >= b && d >= c with 
                |true -> (a, d)
                |false -> func (a, c, d, b) 
            |false -> func (b, c, d, a)
    func (a,b,c,d)

let isLeap y = match y < 1582 with 
                |true -> failwith "Year is less than 1582"
                |_ -> match y % 100 = 0, y % 400 = 0, y % 4 = 0 with 
                    |true, true, true -> true
                    |false, _ , true -> true
                    |_ -> false  

let month m = match m with
                |1 -> "January", 31
                |2 -> "February", 28
                |3 -> "March", 31
                |4 -> "April", 30
                |5 -> "May", 31
                |6 -> "June", 30
                |7 -> "July", 31
                |8 -> "August", 31
                |9 -> "September", 30
                |10 -> "October", 31
                |11 -> "November", 30
                |12 -> "December", 31
                |_ -> failwith "Invalid input"

let toBinary n = let rec func n s = 
                         match n = 0, s = "" with 
                            |true, true -> "0"
                            |true, false -> s
                            |_, _ -> match (n % 2) = 0 with 
                                |true -> func (n / 2) ("0" + s)
                                |false -> func (n / 2) ("1" + s)
                 match n < 0 with 
                    |true -> failwith "Negative input"
                    |_ -> func n ""

let bizFuzz n = let rec func i (a,b,c) = 
                    match i > n,  (i % 3) = 0, (i % 5) = 0 with 
                        |true, _, _ -> (a,b,c)
                        |false, true, true -> func (i + 1) (a + 1, b + 1, c + 1)
                        |false, true, false -> func (i + 1) (a + 1, b, c)
                        |false, false, true -> func (i + 1) (a, b + 1, c)
                        |_, _, _ -> func (i + 1) (a,b,c)
                func 1 (0,0,0)
   
let monthDay d y = 
    let f = match isLeap y with 
        |true -> 29
        |false -> 28
    let monthdays = (31, f, 31, 30, 31, 30, 31, 31, 30, 31,30, 31)
    let monthstrings = ("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
    let rec func n (djan, dfeb, dmar, dapr, dmay, djun, djul, daug, dsep, doct, dnov, ddec) (sjan, sfeb, smar, sapr, smay, sjun, sjul, saug, ssep, soct, snov, sdec) = 
        match djan = -1 || n < 1 with 
            |true -> failwith "Out of range"
            |false -> match (n <= djan)with
                        |true  -> sjan
                        |false -> func (n - djan) (dfeb, dmar, dapr, dmay, djun, djul, daug, dsep, doct, dnov, ddec, -1) (sfeb, smar, sapr, smay, sjun, sjul, saug, ssep, soct, snov, sdec, sjan)
    func d monthdays monthstrings

let coord (x, y) = 
    let sqrt n = 
        let rec calc guess lastguess = 
            match -0.00000001 < (lastguess - guess) && (lastguess - guess) < 0.0000001 with 
                |true -> guess
                |_ -> calc ((guess + n / guess) / 2.0) (guess)
        calc (n / 2.0) 0.0
    let distance (a, b) = sqrt ((a - x) * (a - x) + (b - y) * (b - y))
    let within (j, k) w h = ((x > j) && (x < (j + w * 1.0)) && (y < k) && (y > (k - h * 1.0)))
    (distance, within)    