[@@@warning "-11"]

[@@@warning "-27"]

open! Base
open! Ast
open! Config

(* viewing functions *)

open Caml.Printf

let rec view_proc ind ?(cfg = std_cfg) ?(new_line = false) =
  let indent = String.make ind ' ' in
  function
  | Nil -> indent ^ "Nil"
  | BoolProc b -> indent ^ Bool.to_string b
  | IntProc i -> indent ^ Int.to_string i
  | StrProc s -> indent ^ "\"" ^ s ^ "\""
  | PVar p -> indent ^ p
  | Brace p -> indent ^ wrap_with_cfg ind cfg Braces @@ view_proc ind ~cfg p
  | Paren p -> indent ^ wrap_with_cfg ind cfg Parens @@ view_proc ind ~cfg p
  | New ((h, tl), p) ->
    indent
    ^ wrap_with_cfg ind cfg (WNew (view_names ind cfg @@ Names (h :: tl)))
    @@ view_proc (ind + cfg.indent) ~cfg p
  | Bundle (true, false, p) ->
    indent ^ wrap_with_cfg ind cfg BundlePlus @@ view_proc ind ~cfg p
  | Bundle (false, true, p) ->
    indent ^ wrap_with_cfg ind cfg BundleMinus @@ view_proc ind ~cfg p
  | Bundle (false, false, p) ->
    indent ^ wrap_with_cfg ind cfg BundleZero @@ view_proc ind ~cfg p
  | Bundle (true, true, p) -> indent ^ view_proc ind ~cfg p
  | Consume (bs, p) ->
    indent
    ^ wrap_with_cfg ind cfg (For (view_binds ind cfg bs))
    @@ view_proc ind ~cfg p
  | Produce (per, ch, ps) ->
    indent
    ^ wrap_with_cfg ind cfg (Send (per, view_name ind cfg ch))
    @@ view_procs 0 cfg (Procs ps)
  | Match (p, mcs) -> (
    indent
    ^
    match mcs with
    | Cases [] -> "Nil"
    | Cases _ ->
      sprintf "match %s {%s\n%s\n}" (view_proc 0 ~cfg p)
        (String.make (ind + cfg.indent) ' ')
        (view_match_cases (ind + cfg.indent) cfg mcs))
  | List ps -> indent ^ sprintf "[%s]" @@ view_procs ind cfg (Procs ps)
  | Set ps -> indent ^ view_set ind cfg ps
  | Map kvs -> indent ^ view_map ind cfg kvs
  | Par ps ->
    indent ^ String.concat ~sep:" | " @@ List.map ~f:(view_proc ind ~cfg) ps
  | Eval n -> indent ^ "*" ^ view_name ind cfg n
  | Paren p -> indent ^ sprintf "(%s)" @@ view_proc ind ~cfg p
  | Brace p -> indent ^ sprintf "{%s}" @@ view_proc ind ~cfg p

and view_match_case ind cfg (pat, p) =
  sprintf "%s => {\n%s\n%s}"
    (view_proc_pat ind cfg pat)
    (view_proc (ind + cfg.indent) ~cfg p)
    (String.make ind ' ')

and view_match_cases ind cfg (Cases l) =
  String.concat ~sep:"\n" @@ List.map ~f:(view_match_case ind cfg) l

and view_bind ind cfg np n = function
  | Peek -> sprintf "%s <<- %s" (view_name_pats ind cfg np) (view_name 0 cfg n)
  | Linear -> sprintf "%s <- %s" (view_name_pats ind cfg np) (view_name 0 cfg n)
  | Persistent ->
    sprintf "%s <= %s" (view_name_pats ind cfg np) (view_name 0 cfg n)

and view_binds ind cfg (Binds (bk, bc, l)) =
  String.concat ~sep:(view_bind_combinator bc)
    @@ List.map ~f:(fun (x, y) -> view_bind ind cfg x y bk) l

and view_bind_combinator = function
  | Sequential -> " ; "
  | Parallel -> " & "

and view_procs ind cfg (Procs ps) =
  sprintf "%s%s" (String.make ind ' ')
    (String.concat ~sep:", " @@ List.map ~f:(view_proc 0 ~cfg) ps)

and view_proc_pat ind cfg =
  let indent = String.make ind ' ' in
  function
  | BoolPat -> indent ^ "Bool"
  | IntPat -> indent ^ "Int"
  | StrPat -> indent ^ "String"
  | PWildcard -> indent ^ "_"
  | Proc p -> indent ^ view_proc ind ~cfg p
  | BundlePat (true, true, p) -> view_proc_pat ind cfg p
  | BundlePat (true, false, p) ->
    wrap_with_cfg ind cfg BundlePlus @@ view_proc_pat ind cfg p
  | BundlePat (false, true, p) ->
    wrap_with_cfg ind cfg BundleMinus @@ view_proc_pat ind cfg p
  | BundlePat (false, false, p) ->
    wrap_with_cfg ind cfg BundleZero @@ view_proc_pat ind cfg p
  | ConsumePat (bs, p) ->
    wrap_with_cfg ind cfg (For (view_bind_pats ind cfg bs))
    @@ view_proc_pat ind cfg p
  | ProducePat (per, npat, pats) ->
    wrap_with_cfg ind cfg (Send (per, view_name_pat ind cfg npat))
    @@ view_proc_pats ind cfg pats
  | And (p, q) ->
    sprintf "%s /\\ %s" (view_proc_pat ind cfg p) (view_proc_pat 0 cfg q)
  | Or (p, q) ->
    sprintf "%s \\/ %s" (view_proc_pat ind cfg p) (view_proc_pat 0 cfg q)
  | Not p -> indent ^ "~" ^ view_proc_pat 0 cfg p

and view_proc_pats ind cfg (Pats pats) =
  String.concat ~sep:", " @@ List.map ~f:(view_proc_pat ind cfg) pats

and view_name ind cfg = function
  | NVar n -> n
  | Quote p -> "@" ^ view_proc ind ~cfg p
  | Unforg (m, n) -> sprintf "x(%d, %d)" m n

and view_names ind cfg (Names ns) =
  String.concat ~sep:", " @@ List.map ~f:(view_name ind cfg) ns

and view_name_pat ind cfg = function
  | QuotePat pat -> "@" ^ view_proc_pat ind cfg pat
  | NWilcdcard -> "_"
  | Name n -> view_name ind cfg n

and view_name_pats ind cfg (NPats nps) =
  String.concat ~sep:", " @@ List.map ~f:(view_name_pat ind cfg) nps

and view_set ind cfg ps =
  sprintf "Set(%s)"
  @@ view_procs ind cfg (Procs (List.dedup_and_sort ~compare:compare_proc ps))

and view_map ind cfg kvs = sprintf "{%s}" @@ view_kvs ind cfg kvs

and view_kvs ind cfg kvs =
  String.concat ~sep:",\n" @@ List.map ~f:(view_kv ind cfg) kvs

and view_kv ind cfg (k, v) =
  sprintf "%s : %s" (view_proc (ind + cfg.indent) ~cfg k) (view_proc 0 ~cfg v)

and view_bind_pat ind cfg np n =
  let vnps = view_name_pats ind cfg np in
  let vnp = view_name_pat 0 cfg n in
  function
  | Peek -> sprintf "%s <<- %s" vnps vnp
  | Linear -> sprintf "%s <- %s" vnps vnp
  | Persistent -> sprintf "%S <= %s" vnps vnp

and view_bind_pats ind cfg (BindPats (bk, bc, l)) =
  String.concat ~sep:(view_bind_combinator bc)
    @@ List.map ~f:(fun (x, y) -> view_bind_pat ind cfg x y bk) l

and wrap_with_cfg ind cfg ?(new_line = false) =
  let one_or_zero cfg_opt = if cfg_opt then " " else "" in
  function
  | Braces ->
    fun s ->
      let sp =
        if cfg.new_line_brace then "\n" else one_or_zero cfg.fluffy_brace
      in
      sprintf "{%s%s%s}" sp s sp
  | Parens ->
    fun s ->
      let sp = one_or_zero cfg.fluffy_paren in
      sprintf "(%s%s%s)" sp s sp
  | For bs ->
    fun s ->
      let sp = one_or_zero cfg.fluffy_for in
      let sp1 = one_or_zero cfg.fluffy_brace in
      sprintf "for%s(%s)%s{%s%s%s}" sp bs sp sp1 s sp1
  | Send (per, ch) ->
    fun s ->
      let sp = one_or_zero cfg.fluffy_paren in
      sprintf "%s%s(%s%s%s)" ch (if per then "!!" else "!") sp s sp
  | WNew nl ->
    fun s -> sprintf "new %s%sin {\n%s\n}" nl (if new_line then "\n" else " ") s
  | _ -> fun _ -> ""
