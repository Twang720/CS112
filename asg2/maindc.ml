(* $Id: maindc.ml,v 1.5 2017-04-07 13:24:41-07 - - $ *)

include Scanner
include Bigint

(* for registers*)
let aget = Array.get
let aset = Array.set
let amake = Array.make
let char_lim = 69

open Bigint
open Printf
open Scanner

(*create a stack of bigints*)
type stack_t = Bigint.bigint Stack.t
let push = Stack.push
let pop = Stack.pop

let ord thechar = int_of_char thechar
type binop_t = bigint -> bigint -> bigint

(*check if length of string is > 70, if it is, add newline*)
let print_number number = 
        let (num,index) = (string_of_bigint number,0) in 
    let rec print' index = 
        if ((String.length num) - index >= char_lim) then
            (printf "%s\\\n%!" (String.sub num index char_lim);
            (print' (index + char_lim)))
        else printf "%s\n%!" 
           (String.sub num index ((String.length num) - index))
        in print' 0


let print_stackempty () = printf "dc: stack empty\n%!"

(*for registers*)
let symbol_table = amake 256 Bigint.zero

(* called to execute register functions*)
let executereg (thestack: stack_t) (oper: char) (reg: int) =
    try match oper with
        | 'l' -> push (aget symbol_table reg) thestack 
        | 's' -> aset symbol_table reg (pop thestack)
        | _   -> printf "0%o 0%o is unimplemented\n%!" (ord oper) reg
    with Stack.Empty -> print_stackempty()

let executebinop (thestack: stack_t) (oper: binop_t) =
    try let right = pop thestack
        in  try let left = pop thestack
                in  push (oper left right) thestack
            with Stack.Empty -> (print_stackempty ();
                                 push right thestack)
    with Stack.Empty -> print_stackempty ()

let execute (thestack: stack_t) (oper: char) =
    try match oper with
        (*all these functions are defined in bigint.ml*)
        | '+'  -> executebinop thestack Bigint.add
        | '-'  -> executebinop thestack Bigint.sub
        | '*'  -> executebinop thestack Bigint.mul
        | '/'  -> executebinop thestack Bigint.div
        | '%'  -> executebinop thestack Bigint.rem
        | '^'  -> executebinop thestack Bigint.pow
        | 'c'  -> Stack.clear thestack
        | 'd'  -> push (Stack.top thestack) thestack
        | 'f'  -> Stack.iter print_number thestack
        | 'l'  -> failwith "operator l scanned with no register"
        | 'p'  -> print_number (Stack.top thestack)
        | 'q'  -> raise End_of_file
        | 's'  -> failwith "operator s scanned with no register"
        | '\n' -> ()
        | ' '  -> ()
        | _    -> printf "0%o is unimplemented\n%!" (ord oper)
    with Stack.Empty -> print_stackempty()

let toploop (thestack: stack_t) inputchannel =
    let scanbuf = Lexing.from_channel inputchannel in
    let rec toploop () = 
    (* try to get next token, else throw end of file*)
        try  let nexttoken = Scanner.scanner scanbuf
             in  (match nexttoken with
                 | Number number       -> push number thestack
                 (* if register, which is a tuple, call execute reg *)
                 | Regoper (oper, reg) -> executereg thestack oper reg
                 | Operator oper       -> execute thestack oper
                 );
             toploop ()
        with End_of_file -> printf "";
    in  toploop ()

let readfiles () =
    let thestack : bigint Stack.t = Stack.create ()
    in  ((if Array.length Sys.argv > 1 
         then try  let thefile = open_in Sys.argv.(1)
                   in  toploop thestack thefile
              with Sys_error message -> (
                   printf "%s: %s\n%!" Sys.argv.(0) message;
                   exit 1));
        toploop thestack stdin)

(*interact is called first, it create a bigint stack*)
let interact () =
    let thestack : bigint Stack.t = Stack.create ()
    in  toploop thestack stdin

let _ = if not !Sys.interactive then readfiles ()
