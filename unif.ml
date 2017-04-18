open Printf


type symbol=S of string;;
type variable=Var of string;;
type term=V of (variable * int) | Cons of symbol | Node of (symbol*(term list)) ;;
type atom = symbol*(term list);; 
type query = Goal of (atom list);;
type assertion = (atom * (atom list));;
type environment = (variable * term) list;;


exception NON_UNIFIABLE;;
exception NoSolution;;

let rec find env x =
  try List.assoc x env with Not_found -> V(x); 
;;


let rec subst env t = match t with
  | V(x) -> (let e' = find env x in
	 if (V(x)=e') then e' else subst env e')
  | Cons(x) -> Cons(x)
  | Node(c,ls) -> Node(c, List.map (subst env) ls);	
;;


let rec termtostring t = match t with
  | V(Var(v),0) -> v;
  | V(Var(v),n) -> v ^ (string_of_int n) ;
  | Cons(S(c)) -> c;
  | Node(S(f), ls) -> f ^ "(" ^ (String.concat ", " (List.map termtostring ls)) ^ ")";
;;

let env_to_string env = match List.filter (fun ((_, n), _) -> n = 0) env with
    | [] -> "Yes";
    | env' -> String.concat ",\n" 
	(List.map	   (fun ((Var(x),n), e) -> x ^ " = " ^ termtostring (subst env e)) (List.rev env'));
;;


let rec occurs x t = match t with
  | V(y) -> if(x=y)then true else false ;
  | Cons(_) -> false ;
  | Node(_,l) -> List.exists (occurs x) l ;
;;


let rec mgu env t1 t2 = match subst env t1, subst env t2 with
    | t1,t2 when t1 = t2 -> env;
    | (V(y),t) | (t,V(y)) -> if (occurs y t) then raise NON_UNIFIABLE else ((y,t)::env) ;
    | Cons _ , _ -> raise NON_UNIFIABLE;
    | Node(c1, ts1), Node(c2, ts2) when c1 = c2 -> (unif env ts1 ts2)
    | Node _ , _ -> raise NON_UNIFIABLE;

and unif env l1 l2 =
  try
    List.fold_left2 (fun env t1 t2 -> mgu env t1 t2) env l1 l2;
  with Invalid_argument _ -> raise NON_UNIFIABLE;
;;


let unify_atms env a1 a2 = match a1,a2 with
  (c1,t1),(c2,t2) -> if c1 = c2 then unif env t1 t2 else raise NON_UNIFIABLE
;;



let base = ref ([] : (assertion list));;

 
let add_line a = 
  let rec add = function 
  | [] -> [a] 
  | b::bs -> b::(add bs) in (base := add !base)
;;
 

let rec renumber_term n t = match t with
  | V(x,n') -> V(x,n);
  | Cons(c) -> Cons(c);
  | Node(x,y) -> Node(x, List.map (renumber_term n) y);
;;

let rec renumber_atom n (c,l) = (c, List.map (renumber_term n) l);;


let rec print_answer ch env = match (env_to_string (env)), (ch) with
    | "Yes", _ -> printf "Yes."
    | answer, [] -> printf "%s." answer
    | answer, ch -> begin
      	printf "%s " answer;
      	match read_line () with
      	  | "" | ";" -> continue_search ch
      	  | _ -> printf "\tUnknown action\n" ;flush stdout;raise NoSolution
      end;


and 
continue_search s = match s with
  | [] -> raise NoSolution ;
  | (asrl,env,gs,n)::cs -> solve cs asrl env gs n;


and 
solve ch asrl env c n =

  let rec reduce_atom a = function
    | [] -> None
    | (b,lst)::al' ->
		(try
		   let env' = unify_atms env a (renumber_atom n b) in
		     Some (al', env', List.map (renumber_atom n) lst)
		 with NON_UNIFIABLE -> reduce_atom a al')

  in
    match c with
      | [] -> print_answer ch env
      | a::c' ->
		  (match reduce_atom a asrl with
		     | None -> continue_search ch;
	    	 | Some (asrl', env', d) -> let ch' = (asrl', env, c, n)::ch in  solve ch' !base env' (d @ c') (n+1));
;;

let answer c =
  try
    solve [] !base [] c 1
  with NoSolution -> printf "End\n";flush stdout;
;;
