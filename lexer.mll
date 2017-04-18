{
open Parser	
open Printf
open Lexing

let incr_linenum lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with
      pos_lnum = pos.pos_lnum + 1;
      pos_bol = pos.pos_cnum;
    }

}

let capstart = ['A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']*
let smlstart = ['a'-'z']['a'-'z' 'A'-'Z' '0'-'9' '_']*

rule scanner = parse



| ','		{ (*printf "COMMA\n";*)COMMA }
| '.'		{ END }

| '('		{ OPEN_PAREN }
| ')' 		{ CLOSE_PAREN }
| '\\'		{ (*printf "BS\n";*) BS }
| '='		{ EQ }
| ":-" 		{ FROM }
| "?-"		{ (*printf "GOAL\n";*)GOAL }
| capstart as text	 { (*printf "CSTART %d %s\n" lexbuf.lex_curr_p.pos_lnum (Lexing.lexeme lexbuf);*)CSTART(text) }
| smlstart as text  { (*printf "SSTART\n";*) SSTART(text)  }

| ['%'][^'\n']*['\n']		{ incr_linenum lexbuf; scanner lexbuf }


| [' ''\t']+ { scanner lexbuf }
| ['\n']		{incr_linenum lexbuf; scanner lexbuf }

| [^' ''\t''\n' ',' '.' '(' ')' '?' '-' ':' '=' '\\' '[' ''' ']']+  	{ (*printf " Invalid_token(%s) " invalid;*)scanner lexbuf}


| "['" 			{ FL }
| "']." 		{ FR }

| capstart ".pl"	as text		{ NAME_OF_FILE(text) }
| smlstart ".pl"	as text		{ NAME_OF_FILE(text) }

| eof { EOF }

