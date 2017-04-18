%{
open Printf
open Unif
%}

%token <string> CSTART
%token <string> SSTART
%token OPEN_PAREN CLOSE_PAREN 
%token FROM COMMA END GOAL BS EQ
%token EOF FL FR
%token <string> NAME_OF_FILE
%start file
%start goal
%start filename
%type <Unif.assertion list> file
%type <Unif.query> goal
%type <string> filename
%%

filename:
	FL NAME_OF_FILE FR				{ $2 }
;

file:
  | EOF  		                    { [] }
  | assertion file                  { add_line ($1) ; $1::$2 }
  ;

goal: 
  | GOAL clause END        { Goal($2) (*returns  Goal of (atom list)=query *) (* ?- relation(X,Y) . *)}
  ;

assertion:
  | atom END               { ($1,[]) (* returns 'atom*(atom list)' *)}
  | atom FROM clause END   { ($1, $3) (* son(X,Y) :- male(X),parent(Y,X). *)}
  ;

atom:
  | SSTART                     			{ (S($1), []) (* returns 'atom' *)}
  | SSTART OPEN_PAREN args CLOSE_PAREN  { (S($1), $3) (* married(ab,cd) *)}
  | CSTART BS EQ CSTART 				{ (S("Not_equal"),[V(Var($1),0);V(Var($4),0)]) } 
  ;

clause:
  | atom                      { [$1] (* returns 'atom list' *)}
  | atom COMMA clause         { $1::$3 }
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

%%