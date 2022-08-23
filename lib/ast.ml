open! Base
open! Config

(* Abstract syntax tree *)

(* Processes *)
type proc =
  | Nil
  | BoolProc of bool
  | IntProc of int
  | StrProc of string
  | PVar of string
  | New of (name * name list) * proc
  | Bundle of bool * bool * proc
  | Consume of binds * proc
  | Produce of bool * name * proc list
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
  | ProducePat of bool * name_pat * proc_pats
  (* logical patterns *)
  | And of proc_pat * proc_pat
  | Or of proc_pat * proc_pat
  | Not of proc_pat

and bind_pats =
  | BindPats of bind_kind * bind_combinator * (name_pats * name_pat) list

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
  | Name of name

and name_pats = NPats of name_pat list

and comment =
  | Line_comment of string
  | Block_comment of string

and wrappers =
  | Braces
  | Parens
  | BundlePlus
  | BundleMinus
  | BundleZero
  | For of string
  | Send of bool * string
  | WNew of string

type formatted_proc =
  { config : config
  ; proc : proc
  }

(* TODO include comments *)
