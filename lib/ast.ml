[@@@warning "-11"]

[@@@warning "-27"]

open! Base
open! Config

(* Abstract syntax tree *)

(* Processes *)
type proc =
  | Nil
  | Bool of bool
  | Int of int
  | Str of string
  | PVar of string
  | New of (name * name list) * proc
  | Bundle of bool * bool * proc
  | Consume of binds * proc
  | Bang of bool * name * proc list
  | Match of proc * match_cases
  | List of proc list
  | Set of proc list
  | Map of (proc * proc) list
  | Par of proc list
  (* | IfThenElse of proc * proc * proc *)
  | Eval of name
  | Paren of proc
  | Brace of proc
[@@deriving compare]

and bind_kind =
  | Peek
  | Linear
  | Persistent

and bind_combinator =
  | Sequential
  | Parallel

and match_case = proc_pat * proc

and match_cases = Cases of match_case list

and binds = Binds of bind_kind * bind_combinator * (name_pats * name) list

and procs = Procs of proc list

and proc_pat =
  | BoolPat
  | IntPat
  | StrPat
  (* wildcard pattern *)
  | PWildcard
  (* processes are patterns too *)
  | Proc of proc
  | BundlePat of bool * bool * proc_pat
  | ConsumePat of bind_pats * proc_pat
  | BangPat of bool * name_pat * proc_pats
  (* logical patterns *)
  | And of proc_pat * proc_pat
  | Or of proc_pat * proc_pat
  | Not of proc_pat

and bind_pats = BindPats of bind_kind * (name_pats * name_pat) list

and proc_pats = Pats of proc_pat list

and name =
  | NVar of string
  | Quote of proc
  (* Rholang v1.1 *)
  (* | Ret_no_args of string *)
  (* | Ret_args of string * proc list *)
  (* DeBruijn indices *)
  | Unforg of int * int

and names = Names of name list

and name_pat =
  | QuotePat of proc_pat
  | NWilcdcard

and name_pats = NPats of name_pat list

and wrappers =
  | Braces
  | Parens
  | BundlePlus
  | BundleMinus
  | BundleZero
  | For of string
  | Send of bool * string
  | WNew of string

(* viewing functions *)

open Caml.Printf

let rec view_proc ind ?(cfg = std_cfg) ?(new_line = false) = function
  | Nil -> String.make ind ' ' ^ "Nil"
  | Bool b -> String.make ind ' ' ^ Bool.to_string b
  | Int i -> String.make ind ' ' ^ Int.to_string i
  | Str s -> String.make ind ' ' ^ "\"" ^ s ^ "\""
  | PVar p -> String.make ind ' ' ^ p
  | Brace p ->
    String.make ind ' ' ^ wrap_with_cfg ind cfg Braces @@ view_proc ind ~cfg p
  | Paren p ->
    String.make ind ' ' ^ wrap_with_cfg ind cfg Parens @@ view_proc ind ~cfg p
  | New ((h, tl), p) ->
    wrap_with_cfg ind cfg (WNew (view_names ind cfg @@ Names (h :: tl)))
    @@ view_proc (ind + cfg.indent) ~cfg p
  | Bundle (true, false, p) ->
    wrap_with_cfg ind cfg BundlePlus @@ view_proc ind ~cfg p
  | Bundle (false, true, p) ->
    wrap_with_cfg ind cfg BundleMinus @@ view_proc ind ~cfg p
  | Bundle (false, false, p) ->
    wrap_with_cfg ind cfg BundleZero @@ view_proc ind ~cfg p
  | Bundle (true, true, p) -> view_proc ind ~cfg p
  | Consume (bs, p) ->
    wrap_with_cfg ind cfg (For (view_binds ind cfg bs)) @@ view_proc ind ~cfg p
  | Bang (per, ch, ps) ->
    wrap_with_cfg ind cfg (Send (per, view_name ind cfg ch))
    @@ view_procs ind cfg (Procs ps)
  | Match (p, mcs) -> (
    match mcs with
    | Cases [] -> "Nil"
    | Cases _ ->
      "match" ^ view_proc ind ~cfg p ^ "{" ^ view_match_cases ind cfg mcs ^ "}")
  | List ps -> "[" ^ view_procs ind cfg (Procs ps) ^ "]"
  | Set ps -> view_set ind cfg ps
  | Map kvs -> view_map ind cfg kvs
  | Par ps -> String.concat ~sep:" | " @@ List.map ~f:(view_proc ind ~cfg) ps
  | Eval n -> "*" ^ view_name ind cfg n
  | Paren p -> "(" ^ view_proc ind ~cfg p ^ ")"
  | Brace p -> "{" ^ view_proc ind ~cfg p ^ "}"

and view_match_case ind cfg (pat, p) =
  view_proc_pat ind cfg pat ^ " => {" ^ view_proc ind ~cfg p ^ "}"

and view_match_cases ind cfg (Cases l) =
  String.concat ~sep:"\n" @@ List.map ~f:(view_match_case ind cfg) l

and view_bind ind cfg np n = function
  | Peek -> view_name_pats ind cfg np ^ " <<- " ^ view_name ind cfg n
  | Linear -> view_name_pats ind cfg np ^ " <- " ^ view_name ind cfg n
  | Persistent -> view_name_pats ind cfg np ^ " <= " ^ view_name ind cfg n

and view_binds ind cfg (Binds (bk, bc, l)) =
  match l with
  | [] -> ""
  | (np, n) :: tl ->
    view_bind ind cfg np n bk
    ^ String.concat ~sep:(view_bind_combinator bc)
    @@ List.map ~f:(fun (x, y) -> view_bind ind cfg x y bk) tl

and view_bind_combinator = function
  | Sequential -> "; "
  | Parallel -> " & "

and view_procs ind cfg (Procs ps) =
  match ps with
  | [] -> ""
  | p :: tl ->
    sprintf "%s, %s" (view_proc ind ~cfg p)
    @@ String.concat ~sep:", "
    @@ List.map ~f:(view_proc ind ~cfg) tl

and view_proc_pat ind cfg = function
  | BoolPat -> String.make ind ' ' ^ "Bool"
  | IntPat -> String.make ind ' ' ^ "Int"
  | StrPat -> String.make ind ' ' ^ "String"
  | PWildcard -> String.make ind ' ' ^ "_"
  | Proc p -> String.make ind ' ' ^ view_proc ind ~cfg p
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
  | BangPat (per, npat, pats) ->
    wrap_with_cfg ind cfg (Send (per, view_name_pat ind cfg npat))
    @@ view_proc_pats ind cfg pats
  | And (p, q) -> view_proc_pat ind cfg p ^ " /\\ " ^ view_proc_pat ind cfg q
  | Or (p, q) -> view_proc_pat ind cfg p ^ " \\/ " ^ view_proc_pat ind cfg q
  | Not p -> "~" ^ view_proc_pat ind cfg p

and view_proc_pats ind cfg (Pats pats) =
  String.concat ~sep:", " @@ List.map ~f:(view_proc_pat ind cfg) pats

and view_name ind cfg = function
  | NVar n -> n
  | Quote p -> "@" ^ view_proc ind ~cfg p
  | Unforg (m, n) -> "x(" ^ Int.to_string m ^ " " ^ Int.to_string n ^ ")"

and view_names ind cfg (Names ns) =
  String.concat ~sep:", " @@ List.map ~f:(view_name ind cfg) ns

and view_name_pat ind cfg = function
  | QuotePat pat -> "@" ^ view_proc_pat ind cfg pat
  | NWilcdcard -> "_"

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

and view_bind_pat ind cfg np n = function
  | Peek -> view_name_pats ind cfg np ^ " <<- " ^ view_name_pat ind cfg n
  | Linear -> view_name_pats ind cfg np ^ " <- " ^ view_name_pat ind cfg n
  | Persistent -> view_name_pats ind cfg np ^ " <= " ^ view_name_pat ind cfg n

and view_bind_pats ind cfg (BindPats (bk, l)) =
  match l with
  | [] -> ""
  | (np, n) :: tl ->
    view_bind_pat ind cfg np n bk
    ^ String.concat ~sep:"; "
    @@ List.map ~f:(fun (x, y) -> view_bind_pat ind cfg x y bk) tl

and wrap_with_cfg ind cfg ?(new_line = false) = function
  | Braces ->
    fun s ->
      let sp =
        if cfg.new_line_brace then "\n"
        else if cfg.fluffy_brace then " "
        else sprintf ""
      in
      sprintf "{%s%s%s}" sp s sp
  | Parens ->
    fun s ->
      let sp = if cfg.fluffy_paren then " " else "" in
      sprintf "(%s%s%s)" sp s sp
  | For bs ->
    fun s ->
      let sp = if cfg.fluffy_for then " " else "" in
      let sp1 = if cfg.fluffy_brace then " " else "" in
      sprintf "for%s(%s)%s{%s%s%s}" sp bs sp sp1 s sp1
  | Send (per, ch) ->
    fun s ->
      let sp = if cfg.fluffy_paren then " " else "" in
      sprintf "%s%s(%s%s%s)" ch (if per then "!!" else "!") sp s sp
  | WNew nl ->
    fun s -> sprintf "new %s%sin {\n%s\n}" nl (if new_line then "\n" else " ") s
  | _ -> fun _ -> ""

type formatted_proc =
  { config : config
  ; proc : proc
  }
