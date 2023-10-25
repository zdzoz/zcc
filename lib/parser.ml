open Lexer

(* AST *)
type program = Program of func_decl
and func_decl = Function of id * statement
and statement = Statement of statement * statement | Return of exp
and exp = Const of int [@@deriving show { with_path = false }]

module ParserPrivate = struct
  let is_function lex = advance lex |> peak_token = LParen

  let parse_exp lex =
    let ret =
      match peak_token lex with Num x -> Const x | _ -> failwith "TODO"
    in
    (ret, advance lex)

  let parse_stmt lex =
    let ret, lex =
      match peak_token lex with
      (* | STMNT *)
      | RET -> advance lex |> parse_exp
      (* | Int -> advance lex |> parse_exp *)
      | error -> Fmt.failwith "Parse failed with: %s" @@ show_token error
    in
    match peak_token lex with
    | Semi -> (Return ret, advance lex)
    | _ -> failwith "invalid statement: missing semicolon"
end

let parse lexical =
  let open ParserPrivate in
  let get_function lex =
    match peak_token lex with
    | Int -> (
        let lex = advance lex in
        match peak_token lex with
        | Id id when is_function lex -> (
            let lex = advance lex in
            (* HACK: Temporary fastforward to skip () *)
            let lex = fastforward 2 lex in
            match peak_token lex with
            | LBrace -> (
                let stmnt, lex = parse_stmt @@ advance lex in
                match peak_token lex with
                | RBrace -> Function (id, stmnt)
                | _ -> failwith "implement recurse statements")
            | x -> Fmt.failwith "Missing semicolon (%s)" @@ show_token x)
        | _ -> failwith "Parsing function failed")
    | _ -> failwith "Failed to get return type"
  in
  Program (get_function lexical)
