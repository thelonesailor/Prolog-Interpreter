open Printf

type symbol=S of string;;
type variable=Var of string;;
type term=V of (variable * int) | Cons of symbol | Node of symbol*(term list) ;;
type atom = symbol*(term list);; 
type head = Atom of atom;;
type body = head list;;
type clause = atom list;;
type query = Goal of (head list);;

(** An assertion [(a,b_1,...,b_n)] is a Horn formula [b_1 & ... & b_n => a]. *)
type assertion = atom * clause;;

(** An environment is a list of pairs [(x, e)] where [x] is a variable
    instance and [e] is a term. An environment represents the current
    values of variables. *)
type envrnmnt = (variable * term) list;;


type top = Fact of head | Rule of head * body | Goal of (head list);;
type program = clause list;;

(*type goal = head list;;*)


let rec check_occurance s y = match y with
	|[] -> true;
	|(a,b)::x -> (not(s=a)) && (check_occurance s x) ;
;;

(*signature is (string*int)list *)

let rec wfterm sg t = match t with
	|V(x,_) -> true;
	|Cons(c) ->	true;
	|Node(S(s),[]) -> List.mem (s,0) sg;
	|Node(S(s),l) -> (List.mem (s,List.length l) sg) && (List.fold_left (fun bool el -> ((wfterm sg el) && bool)) true l) 
;;


let rec ht t = match t with
  |V(x,_) -> 0;
  |Cons(c) -> 1;
  |Node(s,[]) -> 0;
  |Node(s,l) -> 1 + (List.fold_left (fun acc el -> max (ht el) acc) 0 l);
;;


module VS = Set.Make (String);;

let rec vars t = match t with 
	|V(Var(x),_) -> VS.singleton x;
	|Cons(c) -> VS.empty;
	|Node(s,[]) -> VS.empty;
    |Node(s,l) -> (List.fold_left (fun set el -> VS.union (vars el) set ) VS.empty l); (*CHECK*)
;;

(*recursively apply subtitution sigma to term*)
let rec subst sigma term = match term with
	|V(x,_) -> sigma x;
	|Cons(x) -> Cons(x);
	|Node(s,[]) -> Node(s,[]);
	|Node(s,l) -> Node(s,List.map (fun el -> subst sigma el) l);
;;

let compose f1 f2 = (fun x-> let a =(f1 x) in (subst f2 a))
;;

(*
let id x = V(x);;

let rec sublist f l = match l with
	|[]->[];
	|x::y -> (subst f x) :: (sublist f y);
;;

exception NOT_UNIFIABLE ;;

let rec mgu t1 t2 = match (t1,t2) with
	|(V(x),V(y)) -> if (x=y) then id else (fun u -> if(u=x) then V(y) else V(u));
	|(V(x),Node(y,[])) -> fun u -> if(u=x) then Node(y,[]) else V(u);
	|(V(Var(vx) as x),Node(y,l)) -> let t2=Node(y,l) in if ( VS.mem (vx) (vars t2 ) ) then raise NOT_UNIFIABLE else (fun u -> if(u=x) then t2 else V(u));

	|(Node(c,[]),Node(d,[])) -> if (c=d) then id else raise NOT_UNIFIABLE;
	|(Cons(c),Cons(d)) -> if (c=d) then id else raise NOT_UNIFIABLE;
	
	|(Node(c,[]),Node(x,l)) -> raise NOT_UNIFIABLE;
	|(Cons(c),Node(x,l)) -> raise NOT_UNIFIABLE;

	|(Node(c,[]),V(x)) -> raise NOT_UNIFIABLE;
	|(Cons(c),V(x)) -> raise NOT_UNIFIABLE;

	|(Node(f,l1),Node(g,l2)) -> if (f=g) then (unif l1 l2) else raise NOT_UNIFIABLE;
	|(x,y) -> (mgu y x);

and unif l1 l2 = match (l1,l2) with
	|([],[]) -> id;
	|([],l) -> raise NOT_UNIFIABLE;
	|(l,[]) -> raise NOT_UNIFIABLE;
	|(t1::xs,t2::ys) -> let g=(mgu t1 t2) in (compose g (unif (sublist g xs) (sublist g ys)) );
;;
*)

type environment = (variable * term) list;;


(*snytax.ml*)
let rec lookup env x =
  try List.assoc x env with Not_found -> V(x);(*TODO*)
;;

(** [subst_term sub t] substitutes in term [t] values for variables,
    as specified by the associative list [s]. It substitutes
    repeatedly until the terms stops changing, so this is not the
    usual kind of substitution. It is what we need during
    unification. *)
let rec subst_term env = function
  | V(x) as e -> (let e' = lookup env x in
	 if e = e' then e' else subst_term env e')
  | Cons _ as e -> e
  | Node(c,ls) -> Node(c, List.map (subst_term env) ls)	
;;


(** [string_of_term t] converts term [t] to its string represenation. *)
let rec string_of_term = function
  | V(Var(v),0) -> v;
  | V(Var(v),n) -> v ^ string_of_int n ;
  | Cons(S(c)) -> c;
  | Node(S(f), ls) -> f ^ "(" ^ (String.concat ", " (List.map string_of_term ls)) ^ ")";
;;

(** [string_of_env env] converts environment [env] to its string
    representation. It only keeps instance variables at level 0, i.e.,
    those that appear in the toplevel goal. *)
let string_of_env env = match List.filter (fun ((_, n), _) -> n = 0) env with
    | [] -> "Yes";
    | env' -> String.concat "\n" 
	(List.map	   (fun ((Var(x),n), e) -> x ^ " = " ^ string_of_term (subst_term env e)) (List.rev env'));
;;

(** [occurs x t] returns [true] when variable instance [x] appears in
    term [t]. *)
let rec occurs x = function
    V(y) -> x=y ;
  | Cons _ -> false ;
  | Node(_, ts) -> List.exists (occurs x) ts ;
;;



(**unify.ml

 [NoUnify] is raised when terms cannot be unified. *)
exception NoUnify;;

(** [unify_terms env t1 t2] unifies terms [t1] and [t2] in the current
    environment [env]. On success it returns the environment extended with
    the result of unification. On failure it raises [NoUnify]. *)
let rec unify_terms env t1 t2 = match subst_term env t1, subst_term env t2 with
    | t1, t2 when t1 = t2 -> env
    | (V y, t) | (t, V y) -> if occurs y t then	  raise NoUnify 	else (y,t) :: env
    | Cons(_), _ -> raise NoUnify
    | Node(c1, ts1), Node(c2, ts2) when c1 = c2 -> unify_lists env ts1 ts2
    | Node _, _ -> raise NoUnify

(** [unify_lists env lst1 lst2] unifies two lists of terms in current
    environment [env] and returns a new environment [env'] on success. It
    raises [NoUnify] on failure or if the lists are not of equal length.
*)
and unify_lists env lst1 lst2 =
  try
    List.fold_left2 (fun env t1 t2 -> unify_terms env t1 t2) env lst1 lst2
  with Invalid_argument _ -> raise NoUnify
;;

(** [unify_atoms env a1 a2] unifies atomic propositions [a1] and [a2]
    in current environment [env] and returns a new environment [env'] on
    success. It raises [NoUnify] on failure. *)
let unify_atoms env (c1,ts1) (c2,ts2) =
  if c1 = c2 then unify_lists env ts1 ts2 else raise NoUnify
;;




(** solve.ml

A value of type [choice] represents a choice point in the proof
    search at which we may continue searching for another solution. It
    is a tuple [(asrl, env, c, n)] where [asrl]
    for other solutions of clause [c] in environment [env], using
    assertion list [asrl], where [n] is the search depth. 
*)

(** A database is a list of assertions. It represents the current
    program. *)
type database = assertion list;;
type choice = database * environment * clause * int;;

(** The global database of assertions. *)
let base = ref ([] : database)

(** Add a new assertion at the end of the current database. *)
let assertz a = 
  let rec add = function [] -> [a] | b::bs -> b::(add bs) in
    (base := add !base)

(** Exception [NoSolution] is raised when a goal cannot be proved. *)
exception NoSolution

(** [renumber_term n t] renumbers all variable instances occurring in
    term [t] so that they have level [n]. *)
let rec renumber_term n = function
  | V(x,_) -> V(x,n);
  | Cons _ as c -> c;
  | Node(c,ts) -> Node(c, List.map (renumber_term n) ts);
;;

(** [renumber_atom n a] renumbers all variable instances occurring in
    atom [a] so that they have level [n]. *)
let rec renumber_atom n (c,ts) = (c, List.map (renumber_term n) ts)

(** [display_solution ch env] displays the solution of a goal encoded
    by [env]. It then gives the user the option to search for other
    solutions, as described by the list of choice points [ch], or to abort
    the current proof search. *)
let rec display_solution ch env = match string_of_env env, ch with
    | "Yes", _ -> printf "Yes@."
    | answer, [] -> printf "%s@." answer
    | answer, ch -> begin
      	printf "%s@.more? (y/n) [y]@?" answer;
      	match String.lowercase (read_line ()) with
      	  | "y" | "yes" | ";" -> continue_search ch
      	  | _ -> raise NoSolution
      end;

(** [continue_search ch] looks for other answers. It accepts a list of
    choices [ch]. It continues the search at the first choice in the
    list. *)
and continue_search = function
  | [] -> raise NoSolution
  | (asrl,env,gs,n)::cs -> solve cs asrl env gs n

(** [solve ch asrl env c n] looks for the proof of clause [c]. Other
    arguments are:
    
    [ch] is a list of choices at which we may look for other solutions,
    [asrl] is the list of assertions that are used to reduce [c] to subgoals,
    [env] is the current environment (values of variables),
    [n] is the search depth, which is increased at each level of search.
    When a solution is found, it is printed on the screen. The user
    then decides whether other solutions should be searched for.
*)

and solve ch asrl env c n =

  (** [reduce_atom a asrl] reduces atom [a] to subgoals by using the
      first assertion in the assetion list [asrl] whose conclusion matches
      [a]. It returns [None] if the atom cannot be reduced, or the
      remaining assertions, the new environment and the list of subgoals.
  *)
  let rec reduce_atom a = function
    | [] -> None
    | (b,lst)::asrl' ->
	(try
	   let env' = unify_atoms env a (renumber_atom n b) in
	     Some (asrl', env', List.map (renumber_atom n) lst)
	 with NoUnify -> reduce_atom a asrl')
  in
    match c with
      | [] ->
	  (* All atoms are solved, we found a solution. *)
	  display_solution ch env
      | a::c' ->
	  (* Reduce the first atom in the clause. *)
	  (match reduce_atom a asrl with
	     | None -> 
		 (* This clause cannot be solved, look for other solutions. *)
		 continue_search ch
	     | Some (asrl', env', d) ->
		 (* The atom was reduced to subgoals [d]. Continue
		    search with the subgoals added to the list of
		    goals. *)
		 let ch' = (asrl', env, c, n)::ch (* Add a new choice. *)
		 in
		   solve ch' !base env' (d @ c') (n+1))
;;

(** [solve_toplevel c] searches for the proof of clause [c] using the
    global databased [!base]. This function is called from the main
    program. *)
let solve_toplevel c =
  try
    solve [] !base [] c 1
  with NoSolution -> printf "No solution\n";
;;


