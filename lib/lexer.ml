type id = string [@@deriving show]

type token =
  | Illegal
  (* Delimeters *)
  | LParen
  | RParen
  | LBrace
  | RBrace
  | Semi
  (* Types *)
  | Num of int
  | Real of float
  (* Keywords *)
  | Id of id
  | If
  | RET
  | Int
[@@deriving show { with_path = false }]

module LexerPrivate = struct
  let is_identifier tok =
    Str.string_match (Str.regexp {|[a-zA-Z][a-zA-Z0-9_]*|}) tok 0

  let is_num tok = Str.string_match (Str.regexp {|[0-9]+|}) tok 0

  let tokenify s =
    let get_tok s' =
      match s' with
      | "(" -> LParen
      | ")" -> RParen
      | "{" -> LBrace
      | "}" -> RBrace
      | ";" -> Semi
      | "int" -> Int
      | "return" -> RET
      | "if" -> If
      | tok when is_identifier tok -> Id (Str.matched_string tok)
      | tok when is_num tok -> Num (int_of_string (Str.matched_string tok))
      | tok -> Fmt.failwith "unknown token: %s" tok
    in
    let tokens = ref [] in
    let rec each = function
      | [] -> ()
      | h :: t ->
          each t;
          tokens := get_tok h :: !tokens
    in
    each s;
    !tokens
end

let lexer file =
  let open LexerPrivate in
  let f = open_in file in
  let try_read () =
    try
      let line = input_line f in
      match Str.string_match (Str.regexp {|//|}) line 0 with
      (* FIXME: make comments work only after // *)
      | true -> Some []
      | false ->
          Some
            (line
            |> Str.global_replace (Str.regexp {|(|}) " ( "
            |> Str.global_replace (Str.regexp {|)|}) " ) "
            |> Str.global_replace (Str.regexp {|{|}) " { "
            |> Str.global_replace (Str.regexp {|}|}) " } "
            |> Str.global_replace (Str.regexp {|;|}) " ; "
            |> Str.split (Str.regexp {| +|})
            |> tokenify)
    with End_of_file -> None
  in
  let rec get_toks toks =
    match try_read () with
    | Some sl -> get_toks (toks @ sl)
    | None ->
        close_in f;
        toks
  in
  get_toks []

let print_lexical li =
  let rec f = function
    | [] -> print_endline "]"
    | h :: t when t = [] ->
        print_string (show_token h);
        f t
    | h :: t ->
        print_string (show_token h ^ "; ");
        f t
  in
  print_string "[";
  f li

let advance = function _ :: t -> t | [] -> raise Not_found

let rec fastforward n lex =
  match n with 0 -> lex | _ -> fastforward (n - 1) (advance lex)

let peak_token = function [] -> raise Not_found | h :: _ -> h
