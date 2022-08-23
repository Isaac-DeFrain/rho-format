open! Base
open! Rho_format
open! Config
open! View
open! Ast

let%expect_test "view_proc Nil" =
  Stdio.print_endline @@ view_proc 0 Nil;
  [%expect {| Nil |}]

let%expect_test "view_proc l_send" =
  let p = Produce (false, Quote (PVar "a"), [ IntProc 1; BoolProc true ]) in
  Stdio.print_endline @@ view_proc 0 p;
  [%expect {| @a!(1, true) |}]

let%expect_test "view_proc match p_send" =
  let p =
    Match (PVar "x", Cases [ (IntPat, Produce (true, NVar "a", [ Nil ])) ])
  in
  Stdio.print_endline @@ view_proc 0 p;
  [%expect {|
    match x {
      Int => {
        a!!(Nil)
      }
    } |}]

let%expect_test "view_proc l_consume" =
  let p =
    Consume
      ( Binds
          ( Linear
          , Sequential
          , [ ( NPats [ NWilcdcard; QuotePat (Proc Nil); Name (NVar "y") ]
              , Quote (PVar "x") )
            ] )
      , Produce (false, Quote (IntProc 42), [ Nil; Eval (NVar "y") ]) )
  in
  Stdio.print_endline @@ view_proc 0 p;
  [%expect {| for (_, @Nil, y <- @x) { @42!(Nil, *y) } |}]

let%expect_test "view_proc l_consume2" =
  let p =
    Consume
      ( Binds
          ( Linear
          , Parallel
          , [ ( NPats [ Name (NVar "x") ]
              , Quote (PVar "a") );
              ( NPats [ Name (NVar "y") ]
              , Quote (PVar "b") )
            ] )
      , Produce (false, NVar "z", [ Eval (NVar "x"); Eval (NVar "y") ]) )
  in
  Stdio.print_endline @@ view_proc 0 p;
  [%expect {| for (x <- @a & y <- @b) { z!(*x, *y) } |}]

(* TODO add others *)
