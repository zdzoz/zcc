type program = Program of func_decl
and func_decl = Function of string * statement

and statement =
  | Statement of statement * statement
  | Return of exp

and exp = Const of int

val pp_program : Format.formatter -> program -> unit
val show_program : program -> string
val pp_func_decl : Format.formatter -> func_decl -> unit
val show_func_decl : func_decl -> string
val pp_statement : Format.formatter -> statement -> unit
val show_statement : statement -> string
val pp_exp : Format.formatter -> exp -> unit
val show_exp : exp -> string

module ParserPrivate : sig
  val is_function : Lexer.token list -> bool
  val parse_exp : Lexer.token list -> exp * Lexer.token list

  val parse_stmt :
    Lexer.token list -> statement * Lexer.token list
end

val parse : Lexer.token list -> program
