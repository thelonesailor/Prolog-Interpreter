%{
open Printf
open Unif
%}

%token <string> CSTART
%token <string> SSTART
%token OPEN_PAREN CLOSE_PAREN 
%token FROM COMMA END GOAL BS EQ
%token EOF
%start file
%start query
%type <Unif.top list> file
%type <Unif.query> query
%%


file:
  | EOF  		                    { [] }
  | assertion file                { $1 :: $2 }
  ;

query:
  | goal                { $1 }
  ;

goal: 
  | GOAL clause END        { Goal($2) (* ?- relation(X,Y) . *)}
  ;

assertion:
  | atom END               { Fact($1) (* returns 'head' *)}
  | atom FROM clause END   { Rule($1, $3) (* son(X,Y) :- male(X),parent(Y,X). *)}
  ;

atom:
  | SSTART                     			{ Atom(S($1), []) (* returns 'head' *)}
  | SSTART OPEN_PAREN args CLOSE_PAREN  { Atom(S($1), $3) (* married(ab,cd) *)}
  | CSTART BS EQ CSTART 				{ Atom(S("Not_equal"),[V(Var($1),0);V(Var($4),0)]) } 
  ;

clause:
  | atom                      { [$1] (* returns 'head list' *)}
  | atom COMMA clause         { $1 :: $3 }
  ;

args:
  | literal            { [$1] (* returns 'term list' *)}
  | literal COMMA args { $1::$3 }
  ;

	
literal:
  | SSTART                    			{ Cons(S($1)) (* returns 'term' *)}
  | CSTART                      		{ V(Var($1),0) }
  | SSTART OPEN_PAREN args CLOSE_PAREN 	{ Node(S($1),$3) }
  ;

