type ('s, 't) code
type addr

val compile : ('s, 't) code -> string

val const : Int64.t -> ('s, 's * addr) code
val fbreg : ('s, 's * addr) code
val breg : int -> ('s, 's * addr) code

type 's binary_operator =
  ('s,         's * addr        ) code ->
  ('s * addr, ('s * addr) * addr) code ->
  ('s,         's * addr        ) code

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
