type id = string

val pp_id : Format.formatter -> id -> unit
val show_id : id -> string

type token =
  | Illegal
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Semi
  | Num of int
  | Real of float
  | Id of id
  | If
  | RET
  | Int

val pp_token : Format.formatter -> token -> unit
val show_token : token -> string

module LexerPrivate : sig
  val is_identifier : id -> bool
  val is_num : id -> bool
  val tokenify : id list -> token list
end

val lexer : string -> token list
val print_lexical : token list -> unit
val advance : 'a list -> 'a list
val fastforward : int -> 'a list -> 'a list
val peak_token : 'a list -> 'a

