open Lexer
open Parser
open Printf


let cin=open_in "ass5.pl";;
let lexbuf = Lexing.from_channel cin;;
let program = (Parser.file Lexer.scanner lexbuf);;

printf "%d\n" (List.length program);;

(* 
let _ =

try 

let lexbuf = Lexxing.from_channel stdin in 
while true do


done


with _ -> exit 0;; *)