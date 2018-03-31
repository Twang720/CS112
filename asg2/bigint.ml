(* $Id: bigint.ml,v 1.5 2014-11-11 15:06:24-08 - - $ *)

open Printf


(*Note: to create a Bigint, call the constructor Bigint 
        and pass in sign and list*)
(*Bigintis defined in bigint.mli*)
module Bigint = struct


    type sign     = Pos | Neg
    type bigint   = Bigint of sign * int list

    let  radix    = 10
    let  radixlen =  1

    let car       = List.hd
    let cdr       = List.tl
    let map       = List.map
    let reverse   = List.rev
    let strcat    = String.concat
    let strlen    = String.length
    let strsub    = String.sub
    let zero      = Bigint (Pos, [])

    let trimzeros list =
        let rec trimzeros' list' = match list' with
        | []       -> []
        | [0]      -> []
        | car::cdr ->
             let cdr' = trimzeros' cdr
             in  match car, cdr' with
                 | 0, [] -> []
                 | car, cdr' -> car::cdr'
        in trimzeros' list

    let charlist_of_string str = 
        let last = strlen str - 1
        in  let rec charlist pos result =
            if pos < 0
            then result
            else charlist (pos - 1) (str.[pos] :: result)
        in  charlist last []

    let bigint_of_string str =
    (*let len be the len of the str*)
        let len = strlen str
        (*in function to_intlist, beta-red => first will be str*)
        in  let to_intlist first =
                let substr = strsub str first (len - first) in
                let digit char = int_of_char char - int_of_char '0' in
                map digit (reverse (charlist_of_string substr))
            in  if   len = 0
                then zero
                else if   str.[0] = '_'
                     then Bigint (Neg, to_intlist 1)
                     else Bigint (Pos, to_intlist 0)


    let string_of_bigint (Bigint (sign, value)) =
        match value with
        | []    -> "0"
        (*if there's a list, reverse it*)
        | value -> let reversed = reverse value
                    (*concat empty str and then interpret sign*)
                   in  strcat ""
                       ((if sign = Pos then "" else "-") ::
                        (map string_of_int reversed))


    (*modified versions*)
    let rec cmp' (list1: int list) (list2: int list) compare = 
    match (list1, list2, compare) with
    (*returns result of first comparison, MSB, if lengths are same*) 
    | [], [], compare           -> compare 
    | [], list2, compare        -> (-1)
    | list1, [], compare        -> 1
    | hd1::tl1, hd2::tl2,compare  -> 
      if hd1 = hd2 then cmp' tl1 tl2 compare
      else if hd1 < hd2 then cmp' tl1 tl2 (- 1) else cmp' tl1 tl2 1


    (*check to see if first element is less than equal to or whatver, 
      and if they both run to null, check that first element and that 
      will tell you which is greater than less than*)
    let cmp list1 list2 = match (list1, list2) with
    | [], []            -> 0
    | _::_, []          -> 1
    | [], _::_          -> -1
    | h1::t1,h2::t2 -> if h1 = h2 then cmp' t1 t2 0
                       else if h1 > h2 then cmp' t1 t2 1
                       else cmp' t1 t2 (- 1)

    let rec add' list1 list2 carry = match (list1, list2, carry) with
        | list1, [], 0       -> list1
        | [], list2, 0       -> list2
        | list1, [], carry   -> add' list1 [carry] 0
        | [], list2, carry   -> add' [carry] list2 0
        | car1::cdr1, car2::cdr2, carry ->
          let sum = car1 + car2 + carry
          in  sum mod radix :: add' cdr1 cdr2 (sum / radix)

    (*match the tuple, (list1, list2, carry) with other tuples*)
    (*need case for when they're same number*)
    let rec sub' list1 list2 carry = match (list1, list2, carry) with
        | [], [], _             -> [] 
        | list1, [], 0          -> list1
        | list1, [], carry      -> sub' list1 [- carry] 0
        | [], list2, carry      -> list2
        | h1::t1, h2::t2, carry -> 
            let sum = h1 + radix - h2 + carry 
            in sum mod radix :: sub' t1 t2 (sum / radix - 1)

    (* parameter extracts neg and value from parameter*)
    (*wrong output*)
    let sub (Bigint (neg1,value1)) (Bigint (neg2,value2)) =
        if neg1 = neg2
        (*need to swap according to magnitude*)
        then if (cmp value1 value2) = 0 
             then zero else if (cmp value1 value2) > 0 
             then match neg1 with
                | Neg -> Bigint(Neg, trimzeros (sub' value1 value2 0))
                | Pos -> Bigint(Pos, trimzeros (sub' value1 value2 0))
             else match neg2 with
                | Neg -> Bigint(Pos, trimzeros(sub' value2 value1 0))
                | Pos -> Bigint(Neg, trimzeros(sub' value2 value1 0))
        (* if both signs are different, add them and pass 
          in sign of first one cuz that will 
          determine whether sum is pos or neg*)
        else Bigint (neg1, add' value1 value2 0)

    (*Bigint -> sign * int list*)
    let add (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        (*if 2 signs are equal, call add' with negative*)
        if neg1 = neg2
        (*returns Bigin of values*)
        then Bigint (neg1, add' value1 value2 0)
        (*call trim zeros over*)
        (* if value1 greater than value2, 
           compute v1 - v2 with v1's sign*)
        else if (cmp value1 value2) > 0 
                then Bigint (neg1, trimzeros (sub' value1 value2 0))
                else Bigint (neg2, trimzeros (sub' value2 value1 0))

    (* returns doubled value of list*)
    (* need to double then add new element*)
    let double list1 = add' list1 list1 0

    (* working *)
    let rec mul' multiplier powerof2 multiplicand = 
        (* if the power2 becomes greater than multiplier, 
           return multiplier*)
        if (cmp powerof2 multiplier) > 0 
        then multiplier, [0]
        (* else keep doubling power of 2 and 
            multiplicand (left and right columns)*)
        else let remainder, product = 
             mul' multiplier (double powerof2) (double multiplicand)
            (*then check if remainder is less than power of 2, 
              if it is return remainder and product*)
             in if (cmp remainder powerof2) < 0
                then remainder, product
                (*so will either add/subtract 0 
                 or a power of 2 lesser than it*)
                else trimzeros (sub' remainder powerof2 0), 
                    trimzeros (add' product multiplicand 0)

    (* want lesser of two to be multplicand*)
    let mul (Bigint (neg1, value1)) (Bigint (neg2, value2)) =
        let (multiplier, multiplicand) = 
            if (cmp value1 value2) < 0
            then (value1, value2) 
            else (value2, value1) in
            (*returns a tuple of lists, where first 
              value is remainder and is discarded*)
        let _, product = mul' multiplier [1] multiplicand in 
            if neg1 = neg2 
            then Bigint(Pos, trimzeros(product)) 
            else Bigint(Neg, trimzeros(product))

    let rec divrem' dividend powerof2 divisor = 
        if (cmp divisor dividend) > 0
        then [0], dividend
        else let quotient, remainder = 
             divrem' dividend (double powerof2) (double divisor)
             in if (cmp remainder divisor) < 0  
             then quotient, remainder
             else trimzeros(add' quotient powerof2 0), 
                  trimzeros(sub' remainder divisor 0)

    let divrem dividend divisor = divrem' dividend [1] divisor 

    let div (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) = 
        let quotient, _ = divrem dividend divisor in
            if neg1 = neg2 
            then Bigint(Pos, quotient) 
            else Bigint(Neg, quotient)


    let rem (Bigint (neg1, dividend)) (Bigint (neg2, divisor)) = 
        let _, remainder = divrem dividend divisor 
        in Bigint(Pos,remainder)

    let rec pow' (base,expt,result) = match expt with
        | [0]               -> result
        | expt when let _,rem' = (divrem' expt [1] [2]) in 
          rem' = [0] -> 
          let quotient',_ = divrem expt [2] in 
          let _,product = mul' base [1] base in 
          pow' (product, quotient', result)
        | expt              -> 
          let _,product' =  mul' base [1] result in 
          let difference = trimzeros (sub' expt [1] 0) in 
          if difference = [] then let difference = [0] in 
          pow' (base, difference, product')
          else pow' (base, difference, product')

    (*assume expt is non-negative*)
    let pow (Bigint (neg1, base)) (Bigint (neg2, expt)) =
        if neg2 = Neg then 
        (* get a' = 1/a, which is either 0 or 1,
          if 0 throw DC divide error*)
        let a',_ = divrem [1] base in 
        if a' = [0] then zero
        (*if base is negative and exponent is odd*)
        else let _,rem' = (divrem expt [2]) in 
             match neg1 with 
             | Neg -> if rem' = [0] 
                      then Bigint(Pos,pow' (a',expt,[1]))
                      else Bigint(Neg,pow' (a',expt,[1]))
             | Pos -> Bigint(Pos,pow' (a',expt,[1]))
        (*if base is negative and exponent is odd*)
        else let _,rem' = (divrem expt [2]) in
             match neg1 with  
             | Neg -> if rem' = [0] 
                      then Bigint(Pos,pow' (base,expt,[1]))
                      else Bigint(Neg,pow' (base,expt,[1]))
             | Pos -> Bigint(Pos,pow' (base,expt,[1]))

end
