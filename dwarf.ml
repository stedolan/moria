(* 
Stack types are:
() - empty stack
'a * 'stk - stack with 'a on top

how much yoneda?
I could work with functions, and composition
this is like cayley's for monoids

dup : ('a * 'stk) dwarf -> ('a * ('a * 'stk)) dwarf

(@) : 'a dwarf -> 

bind : ('stk dwarf -> ('a * 'stk) dwarf) -> (


bind c1 (fun x -> add x 5)

(@) : ('stk1, 'stk2) code -> ('stk2, 'stk3) code -> ('stk1, 'stk3) code

under : ('stk, 'a * 'stk) code -> ('b * 'stk, 'a * 'b * 'stk) code

both : ('stk, 'a * 'stk) code -> ('stk, 'b * 'stk) code -> ('stk, 'a * ('b * 'stk)) code
both f g = f @ under g

add : (int * (int * 'stk), int * 'stk) code


bind : ('stk, 'a * 'stk) code -> ('a var -> ('stk, 'stk2) code) -> ('stk, 'stk2) code
var : 'a var -> ('stk, 'a * 'stk) code

 *)


(*>


add1 :
  ('stk, int * 'stk) code ->
  (int * 'stk, int * (int * 'stk)) code ->
  ('stk, int * 'stk) code 

bind1 :
  ('stk, 'a * 'stk) code ->
  ('a var -> ('a * 'stk, 'b * ('a * 'stk)) code) ->
  ('stk, 'b * 'stk) code

var :
  'a var -> ('stk, 'a * 'stk) code

bind1 x (fun x -> add x x)

bind2 x (fun x -> var x @ var x) @ add

x; dup; over; rot; rot; drop


over; rot;
[y x] -> [y x y] -> [y y x]
==
over; swap

swap; rot
[z y x] -> [z x y] -> [y z x]

swap; rot; drop
[z y x] -> [z x y] -> [y z x] -> [y z]
==
drop; swap

dup; rot
[y x] -> [y x x] -> [x y x]
==
swap; over


over; rot; rot; drop
==
over; swap; rot; drop
==
over; drop; swap
==
swap

over; rot; rot => over; dup; rot => over; swap; over

swap; over
[y x] -> [x y] -> [x y x]

over; swap; over
[y x] -> [y x y] -> [y y x] -> 


x; pick 0; pick 1; add; swap; drop

pick1 is last use:
x; pick 0; pick 1



v: ('stk, int * 'stk) code

add : (int * (int * 'stk), int * 'stk) code

const 1 : ('stk, int * 'stk)


lam (fun v -> add (push 1) v) == add (push 1)
*)


type addr = int64

type nullop = Const of Int64.t | Fbreg | Breg of int
type unop = Abs | Neg | Not | Deref
type binop = Plus | Minus | Mul | Mod | Div | Shl | Shr | Shra | And | Or | Xor

type (_,_) index =
| Top : ('stk * 'a, 'a) index
| Below : ('stk, 'a) index -> ('stk * 'b, 'a) index

type (_,_) op =
| Drop : ('stk * 'a, 'stk) op
| Swap : (('stk * 'a) * 'b, ('stk * 'b) * 'a) op
| Pick : ('stk, 'a) index -> ('stk, 'stk * 'a) op
| Nullop : nullop -> ('stk, 'stk * addr) op
| Unop : unop -> ('stk * addr, 'stk * addr) op
| Binop : binop -> (('stk * addr) * addr, 'stk * addr) op

type (_,_) oplist =
| [] : ('stk, 'stk) oplist
| (::) : ('stk1, 'stk2) op * ('stk2, 'stk3) oplist ->
         ('stk1, 'stk3) oplist

let emit s = Printf.printf "%s\n" s

let rec int_of_index : type stk a . (stk, a) index -> int = function
  | Top -> 0
  | Below i -> 1 + int_of_index i

let rec assemble : type stk1 stk2 . (stk1, stk2) oplist -> unit = function
  | Drop :: code ->
     emit "DW_OP_dup";
     assemble code
  | Swap :: code ->
     emit "DW_OP_swap";
     assemble code
  | Pick Top :: code ->
     emit "DW_OP_dup";
     assemble code
  | Pick (Below Top) :: code ->
     emit "DW_OP_over";
     assemble code
  | Pick i :: code ->
     emit "DW_OP_pick";
     emit (string_of_int (int_of_index i));
     assemble code
  | Nullop (Const n) :: code when n < Int64.of_int 32 ->
     emit ("DW_OP_lit" ^ Int64.to_string n)
  | Nullop (Const _) :: code ->
     failwith "constants??"
  | _ -> failwith "bored now"


type 'a ext = ..
module type Tag = sig type t type 'a ext += Tag : t ext end
type 'a tag = (module Tag with type t = 'a)
let fresh (type a) : unit -> a tag = fun () ->
  let module M = struct type t = a type 'a ext += Tag : a ext end in
  (module M)

type ('a, 'b) eqp = Eq : ('a, 'a) eqp | Neq : ('a, 'b) eqp
let cmp (type a) (type b) : a tag -> b tag -> (a, b) eqp =
  fun (module A) (module B) ->
  match A.Tag with B.Tag -> Eq | _ -> Neq

type 'a var = 'a tag

type (_,_) code =
| Prim : ('stk1, 'stk2) op -> ('stk1, 'stk2) code
| Seq : ('stk1, 'stk2) code * ('stk2, 'stk3) code ->
        ('stk1, 'stk3) code
| Lam : 'a var * ('stk * 'a, ('stk * 'a) * 'b) code ->
        ('stk * 'a, 'stk * 'b) code
| Var : 'a var -> ('stk, 'stk * 'a) code

type _ env =
| Empty : 'stk env
| Vars : 'a var list * 'stk env -> ('stk * 'a) env

let uncons_env : type stk . (stk * 'a) env -> 'a var list * stk env =
  function
  | Empty -> [], Empty
  | Vars (vs, env) -> vs, env


let env_drop env = snd (uncons_env env)
let env_push env = Vars ([], env)

let env_op : type stk1 stk2 . stk1 env -> (stk1, stk2) op -> stk2 env =
  fun env op -> match op with
  | Drop -> env_drop env
  | Swap ->
     let (a, env) = uncons_env env in
     let (b, env) = uncons_env env in
     Vars (b, Vars (a, env))
  | Pick _ -> env_push env      (* FIXME: should be more precise *)
  | Nullop _ -> env_push env
  | Unop _ -> env_push (env_drop env)
  | Binop _ -> env_push (env_drop (env_drop env))

let rec memv : type a b . a var -> b var list -> (a, b) eqp =
  fun v' vs -> match vs with
  | [] -> Neq
  | v :: vs ->
     match cmp v v' with
     | Eq -> Eq
     | Neq -> memv v' vs
  
let rec pickv : type a stk . stk env -> a var -> (stk, a) index =
  fun env var -> match env with
  | Empty -> failwith "variable used out of scope"
  | Vars (vs, env) ->
     match memv var vs with
     | Eq -> Top
     | Neq -> Below (pickv env var)

let rec compile_acc : type stk1 stk2 stk3 .
  stk1 env -> (stk1, stk2) code ->
  stk2 env * (
    (stk2, stk3) oplist ->
    (stk1, stk3) oplist) = fun env code ->
  match code with
  | Prim op -> env_op env op, fun acc -> op :: acc
  | Seq (c1, c2) ->
     let env2, comp1 = compile_acc env c1 in
     let env3, comp2 = compile_acc env2 c2 in
     env3, fun acc -> comp1 (comp2 acc)
  | Lam (v, body) ->
     let vs, env = uncons_env env in
     let body' = Seq (body, Seq (Prim Swap, Prim Drop)) in
     compile_acc (Vars (v :: vs, env)) body'
  | Var v ->
     Vars ([v], env),
     fun acc -> Pick (pickv env v) :: acc

let compile code = snd (compile_acc Empty code) []

let add = Prim (Binop Plus)
let dup = Prim (Pick Top)

let const n = Prim (Nullop (Const (Int64.of_int n)))

let (@) c1 c2 = Seq (c2, c1)

let inc x = x @ add @ const 1

let lam body =
  let v = fresh () in Lam (v, body v)

let bind e body = lam body @ e


let two = bind (const 1) (fun x -> add @ Var x @ Var x)
