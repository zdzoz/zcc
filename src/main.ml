open Lpg

(* Lexer *)
let lexical =
  let open Lexer in
  let lex = lexer "src/source.c" in
  print_endline "[Lexical]";
  print_lexical lex;
  lex
;;

print_newline ()

(* AST *)
let ast =
  let open Parser in
  let ast = parse lexical in
  print_endline "[AST]";
  print_endline @@ show_program ast;
  ast
;;

ast;;
print_newline ()

(* Generate Assembly *)

let () =
  let open Generate in
  generate ()
