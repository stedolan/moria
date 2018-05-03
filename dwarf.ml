type addr = int64

type nullop = Const of Int64.t | Fbreg of Int64.t | Breg of int * Int64.t
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

type buffer = {
    buf : Buffer.t;
    line_prefix : string;
    line_max : int;
    mutable len_since_newline : int
}
let emit b s =
  if b.len_since_newline + String.length s + 4 > b.line_max then begin
    Buffer.add_string b.buf ", \\\n";
    b.len_since_newline <- 0
  end;
  let sep =
    if b.len_since_newline > 0 then ", "
    else b.line_prefix in
  Buffer.add_string b.buf sep;
  Buffer.add_string b.buf s;
  b.len_since_newline <- b.len_since_newline + String.length sep + String.length s

let emit_op b s = emit b ("DW_OP_" ^ s)
let emit_byte b n = emit b (Printf.sprintf "0x%02x" n)
let rec emit_leb128 b n =
  let c = Int64.(logand n (of_int 127) |> to_int) in
  let n' = Int64.(shift_right_logical n 8) in
  if n' = Int64.zero then
    emit_byte b c
  else
    (emit_byte b (c lor 128); emit_leb128 b n')


let rec int_of_index : type stk a . (stk, a) index -> int = function
  | Top -> 0
  | Below i -> 1 + int_of_index i

let rec assemble : type stk1 stk2 . buffer -> (stk1, stk2) oplist -> unit =
  fun b code -> match code with
  | Drop :: code ->
     emit_op b "drop";
     assemble b code
  | Swap :: code ->
     emit_op b "swap";
     assemble b code
  | Pick Top :: code ->
     emit_op b "dup";
     assemble b code
  | Pick (Below Top) :: code ->
     emit_op b "over";
     assemble b code
  | Pick i :: code ->
     emit_op b "pick";
     emit_byte b (int_of_index i);
     assemble b code
  | Nullop (Const n) :: code when n < Int64.of_int 32 ->
     emit_op b ("lit" ^ Int64.to_string n);
     assemble b code
  | Nullop (Const n) :: code ->
     emit_op b "constu";
     emit_leb128 b n;
     assemble b code
  | Nullop _ :: code -> failwith "unimplemented"
  | Unop op :: code ->
     let opname = match op with
       | Abs -> "abs"
       | Neg -> "neg"
       | Not -> "not"
       | Deref -> "deref" in
     emit_op b opname;
     assemble b code
  | Binop op :: code ->
     let opname = match op with
       | Plus -> "plus"
       | Minus -> "minus"
       | Mul -> "mul"
       | Mod -> "mod"
       | Div -> "div"
       | Shl -> "shl"
       | Shr -> "shr"
       | Shra -> "shra"
       | And -> "and"
       | Or -> "or"
       | Xor -> "xor" in
     emit_op b opname;
     assemble b code
  | [] -> ()




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

let compile code =
  let oplist = snd (compile_acc Empty code) [] in
  let b = {
      buf = Buffer.create 100;
      line_prefix = "\t  ";
      line_max = 60;
      len_since_newline = 0
    } in
  assemble b oplist;
  Buffer.contents b.buf




type 's binary_operator =
  ('s,         's * addr        ) code ->
  ('s * addr, ('s * addr) * addr) code ->
  ('s,         's * addr        ) code

type 's unary_operator =
  ('s,         's * addr        ) code ->
  ('s,         's * addr        ) code

let (@) c1 c2 = Seq (c1, c2)

let const n = Prim (Nullop (Const n))
let fbreg = Prim (Nullop (Fbreg Int64.zero))
let breg n = Prim (Nullop (Breg (n, Int64.zero)))

module Expr = struct
  let int n = const (Int64.of_int n)
  let (+) a b = a @ b @ Prim (Binop Plus)
  let (-) a b = a @ b @ Prim (Binop Minus)
  let ( * ) a b = a @ b @ Prim (Binop Mul)
  let (mod) a b = a @ b @ Prim (Binop Mod)
  let (/) a b = a @ b @ Prim (Binop Div)
  let (lsl) a b = a @ b @ Prim (Binop Shl)
  let (asr) a b = a @ b @ Prim (Binop Shra)
  let (lsr) a b = a @ b @ Prim (Binop Shr)
  let (land) a b = a @ b @ Prim (Binop And)
  let (lor) a b = a @ b @ Prim (Binop Or)
  let (lxor) a b = a @ b @ Prim (Binop Xor)
  let abs a = a @ Prim (Unop Abs)
  let (~-) a = a @ Prim (Unop Neg)
  let lnot a = a @ Prim (Unop Not)
  let (!) a = a @ Prim (Unop Deref)
end


let var v = Var v
let lam body =
  let v = fresh () in Lam (v, body v)
let apply f x = x @ f
let bind e body = apply (lam body) e


