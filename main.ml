open Lexer
open Parser
open Printf

let cin=stdin;;

printf "Enter the name of file\n";;
flush stdout;;

let lexbuf = Lexing.from_channel cin;;

let fname = (Parser.filename Lexer.scanner lexbuf);;
let cin=open_in fname;;
let lexbuf = Lexing.from_channel cin;;

let program = (Parser.file Lexer.scanner lexbuf);;

printf "%d actual lines\n" (List.length program);;
flush stdout;;

let main () =

  try 

    let lexbuf = Lexing.from_channel stdin in 
    while true do

    let q = (Parser.goal Lexer.scanner lexbuf) in
    let Unif.Goal(q2)=q in
    Unif.answer q2

    done

  with _ -> exit 0
;;

let _ = main();;