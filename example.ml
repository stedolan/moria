(*

adapted from amd64.S:

 /* Expects target function in %rax. */
FUNCTION(G(caml_enter_c))
CFI_STARTPROC
        .cfi_signal_frame
        .cfi_escape DW_CFA_val_expression, 0x7, 40, \
            /* DWARF_stack = [cfa] */ \
          DW_OP_dup, DW_OP_const1u, 24, DW_OP_plus, DW_OP_deref, DW_OP_const1u, 0x03, DW_OP_shl, \
            /* DWARF_stack = [sp offset (bytes); cfa] */ \
          DW_OP_swap, DW_OP_const1u, 32, DW_OP_plus, DW_OP_deref, DW_OP_deref, \
            /* DWARF_stack = [stack; offset] */ \
          DW_OP_dup, DW_OP_const1u, 0x08, DW_OP_minus, DW_OP_deref, \
            /* DWARF_stack = [Hd_val(stack); stack; offset] */ \
          DW_OP_const1u, 0x0a, DW_OP_shr, DW_OP_const1u, 0x03, DW_OP_shl, DW_OP_plus, \
            /* DWARF_stack = [Stack_high(stack) || Stack_high(stack) + 8; offset] */ \
          DW_OP_const8u, 0xf0,0xff,0xff,0xff,0xff,0xff,0xff,0xff, DW_OP_and, \
            /* DWARF_stack = [Stack_high(stack); offset] */ \
          DW_OP_plus, DW_OP_const1u, 0x10, DW_OP_plus \
            /* DWARF_stack = [desired rsp] */
        ENTER_FUNCTION
        call    *%rax
        LEAVE_FUNCTION
        ret
CFI_ENDPROC

*)

open Dwarf
open Dwarf.Expr

let hd_val block =
  ! (block - int 8)

let wosize_hd hd =
  hd lsr int 10

let bsize_wsize hd =
  hd lsl int 3

let block_length_bytes block =
  bsize_wsize (wosize_hd (hd_val block))

let caml_enter_c_dwarf () =
  lam @@ fun cfa ->
     let sp_offset = !(var cfa + int 24) lsl int 3 in
     bind (! !(var cfa + int 32)) @@ fun stack ->
     let stack_high = var stack + block_length_bytes (var stack) in
     let stack_high = stack_high land const 0xfffffffffffffff0L in
     var stack + stack_high + sp_offset


let () =
  Dwarf.compile (caml_enter_c_dwarf ()) |> print_endline
