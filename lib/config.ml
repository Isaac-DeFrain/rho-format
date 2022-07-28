(* configuration *)

type config =
  { width : int
  ; indent : int
  ; align_join : bool
  ; new_line_eof : bool
  ; new_line_new : bool
  ; new_line_par : bool
  ; new_line_join : bool
  ; new_line_brace : bool
  ; fluffy_ite : bool
  ; fluffy_for : bool
  ; fluffy_par : bool
  ; fluffy_paren : bool
  ; fluffy_brace : bool
  }

(* default config *)
let std_cfg =
  { width = 80
  ; indent = 2
  ; align_join = true
  ; new_line_eof = true
  ; new_line_new = true
  ; new_line_par = true
  ; new_line_join = true
  ; new_line_brace = true
  ; fluffy_ite = true
  ; fluffy_for = true
  ; fluffy_par = true
  ; fluffy_paren = false
  ; fluffy_brace = true
  }
