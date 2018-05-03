(* An embedded DSL for typechecking and compiling DWARF expressions *)

(* The type ('s, 't) code represents a DWARF expression that expects
   an input stack 's and produces an output stack 't.

   Stack shapes are represented as nested tuples: a stack containing
   two machine-address-sized integers is represented by the type

      ((unit * addr) * addr)

   Instead of referencing "unit" for the empty stack, operations are
   generally polymorphic in the stack type, since they only touch the
   top few stack elements *)

type ('s, 't) code
type addr (* machine-address-sized integers *)

val compile : ('s, 't) code -> string


(* Pushes an integer constant to the stack *)
val const : Int64.t -> ('s, 's * addr) code

(* 
val fbreg : ('s, 's * addr) code
val breg : int -> ('s, 's * addr) code
*)


(* Binary arithmetic operators all have the same signature. They take
   two parameters. The first is evaluated on a stack 's, pushing an
   addr, while the second is evaluated on the resulting 's * addr
   stack, pushing the second addr. The resulting stack is 's * addr *)

type 's binary_operator =
  ('s,         's * addr        ) code ->
  ('s * addr, ('s * addr) * addr) code ->
  ('s,         's * addr        ) code

(* Unary arithmetic operations have a similar, but simpler, signature *)
type 's unary_operator =
  ('s,         's * addr        ) code ->
  ('s,         's * addr        ) code

module Expr : sig
  (* Convenient infix syntax for DWARF expressions *)

  (* Integer constants *)
  val int    : int -> ('s, 's * addr) code
  (* Arithmetic *)
  val (+)    : 's binary_operator
  val (-)    : 's binary_operator
  val (~-)   : 's unary_operator
  val abs    : 's unary_operator
  val ( * )  : 's binary_operator
  val (mod)  : 's binary_operator
  val (/)    : 's binary_operator
  (* Shifts *)
  val (lsr)  : 's binary_operator
  val (asr)  : 's binary_operator
  val (lsl)  : 's binary_operator
  (* Bitwise operations *)
  val (land) : 's binary_operator
  val (lor)  : 's binary_operator
  val (lxor) : 's binary_operator
  val lnot   : 's unary_operator
  (* Pointer dereference *)
  val (!)    : 's unary_operator
end


(* On a pure stack-machine, it is a little tricky to reuse the same
   value multiple times. Naively using OCaml 'let' bindings results in
   duplicate code, for instance:

       let r = breg 0 in
       add r r

   is the same as:

       add (breg 0) (breg 0)


   In order to use the same value twice, it is necessary to pick the
   result out of the stack, a task made easier by [var] and [bind].

   Variables stored somewhere in the stack are referred to by the type
   ['a var]. These variables can be picked out of the stack using
   [var]. New variables are introduced with [bind].

   For example, this code evaluates to double the value of register 0,
   while only evaluating [breg 0] once:

       bind (breg 0) @@ fun r ->
       add (var r) (var r)
*)

type 'a var
val var :
   'a var -> ('s,       's * 'a)       code

val bind :
             ('s,       's * 'a      ) code ->
  ('a var -> ('s * 'a, ('s * 'a) * 'b) code) ->
             ('s,       's * 'b      ) code

val lam :
  ('a var -> ('s * 'a, ('s * 'a) * 'b) code) ->
             ('s * 'a,  's * 'b      ) code

val apply :
             ('s * 'a,  's * 'b      ) code ->
             ('s,       's * 'a      ) code ->
             ('s,       's * 'b      ) code
