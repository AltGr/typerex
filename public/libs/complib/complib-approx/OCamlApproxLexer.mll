(******************************************************************************)
(*                                                                            *)
(*                          TypeRex OCaml Tools                               *)
(*                                                                            *)
(*                               OCamlPro                                     *)
(*                                                                            *)
(*    Copyright 2011-2012 OCamlPro                                            *)
(*    All rights reserved.  See accompanying files for the terms under        *)
(*    which this file is distributed. In doubt, contact us at                 *)
(*    contact@ocamlpro.com (http://www.ocamlpro.com/)                         *)
(*                                                                            *)
(******************************************************************************)

{

(* Instead of raising an error when a CHAR, INT, INT32, INT64 or NATIVEINT
overflows, we just changed the returned value to take that into account. *)

type 'a overflow =
  | InRange of 'a
  | Overflow of string

module type TokenSig = sig

  type token =
  | AMPERAMPER
  | AMPERSAND
  | AND
  | AS
  | ASSERT
  | BACKQUOTE
  | BANG
  | BAR
  | BARBAR
  | BARRBRACKET
  | BEGIN
  | CHAR of (char overflow)
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
  | COMMENT of (int * int)
  | CONSTRAINT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | END
  | EOF
  | EOF_IN_COMMENT of (int)
  | EOF_IN_STRING of (int)
  | EQUAL
  | EXCEPTION
  | EXTERNAL
  | FALSE
  | FLOAT of (string)
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | IF
  | ILLEGAL_CHAR of (char)
  | IN
  | INCLUDE
  | INFIXOP0 of (string)
  | INFIXOP1 of (string)
  | INFIXOP2 of (string)
  | INFIXOP3 of (string)
  | INFIXOP4 of (string)
  | INHERIT
  | INITIALIZER
  | INT of (int overflow)
  | INT32 of (int32 overflow)
  | INT64 of (int64 overflow)
  | LABEL of (string)
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | LBRACKETGREATER
  | LESS
  | LESSMINUS
  | LET
  | LIDENT of (string)
  | LPAREN
  | MATCH
  | METHOD
  | MINUS
  | MINUSDOT
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NATIVEINT of (nativeint overflow)
  | NEW
  | OBJECT
  | OF
  | OPEN
  | OPTLABEL of (string)
  | OR
  | PLUS
  | PLUSDOT
  | PREFIXOP of (string)
  | PRIVATE
  | QUESTION
  | QUESTIONQUESTION
  | QUOTE
  | RBRACE
  | RBRACKET
  | REC
  | RPAREN
  | SEMI
  | SEMISEMI
  | SHARP
  | SIG
  | STAR
  | STRING of (string)
  | STRUCT
  | THEN
  | TILDE
  | TO
  | TRUE
  | TRY
  | TYPE
  | UIDENT of (string)
  | UNDERSCORE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH

end


module TokenStruct : TokenSig = struct

  type token =
  | AMPERAMPER
  | AMPERSAND
  | AND
  | AS
  | ASSERT
  | BACKQUOTE
  | BANG
  | BAR
  | BARBAR
  | BARRBRACKET
  | BEGIN
  | CHAR of (char overflow)
  | CLASS
  | COLON
  | COLONCOLON
  | COLONEQUAL
  | COLONGREATER
  | COMMA
  | COMMENT of (int * int)
  | CONSTRAINT
  | DO
  | DONE
  | DOT
  | DOTDOT
  | DOWNTO
  | ELSE
  | END
  | EOF
  | EOF_IN_COMMENT of (int)
  | EOF_IN_STRING of (int)
  | EQUAL
  | EXCEPTION
  | EXTERNAL
  | FALSE
  | FLOAT of (string)
  | FOR
  | FUN
  | FUNCTION
  | FUNCTOR
  | GREATER
  | GREATERRBRACE
  | GREATERRBRACKET
  | IF
  | ILLEGAL_CHAR of (char)
  | IN
  | INCLUDE
  | INFIXOP0 of (string)
  | INFIXOP1 of (string)
  | INFIXOP2 of (string)
  | INFIXOP3 of (string)
  | INFIXOP4 of (string)
  | INHERIT
  | INITIALIZER
  | INT of (int overflow)
  | INT32 of (int32 overflow)
  | INT64 of (int64 overflow)
  | LABEL of (string)
  | LAZY
  | LBRACE
  | LBRACELESS
  | LBRACKET
  | LBRACKETBAR
  | LBRACKETLESS
  | LBRACKETGREATER
  | LESS
  | LESSMINUS
  | LET
  | LIDENT of (string)
  | LPAREN
  | MATCH
  | METHOD
  | MINUS
  | MINUSDOT
  | MINUSGREATER
  | MODULE
  | MUTABLE
  | NATIVEINT of (nativeint overflow)
  | NEW
  | OBJECT
  | OF
  | OPEN
  | OPTLABEL of (string)
  | OR
  | PLUS
  | PLUSDOT
  | PREFIXOP of (string)
  | PRIVATE
  | QUESTION
  | QUESTIONQUESTION
  | QUOTE
  | RBRACE
  | RBRACKET
  | REC
  | RPAREN
  | SEMI
  | SEMISEMI
  | SHARP
  | SIG
  | STAR
  | STRING of (string)
  | STRUCT
  | THEN
  | TILDE
  | TO
  | TRUE
  | TRY
  | TYPE
  | UIDENT of (string)
  | UNDERSCORE
  | VAL
  | VIRTUAL
  | WHEN
  | WHILE
  | WITH

end


module StringOfToken(S : TokenSig) = struct

  open S
let string_of_token token =
  match token with
    | AMPERAMPER -> "AMPERAMPER"
    | AMPERSAND -> "AMPERSAND"
    | AND -> "AND"
    | AS -> "AS"
    | ASSERT -> "ASSERT"
    | BACKQUOTE -> "BACKQUOTE"
    | BANG -> "BANG"
    | BAR -> "BAR"
    | BARBAR -> "BARBAR"
    | BARRBRACKET -> "BARRBRACKET"
    | BEGIN -> "BEGIN"
    | CHAR char -> "CHAR of (char Approx_common.overflow)"
    | CLASS -> "CLASS"
    | COLON -> "COLON"
    | COLONCOLON -> "COLONCOLON"
    | COLONEQUAL -> "COLONEQUAL"
    | COLONGREATER -> "COLONGREATER"
    | COMMA -> "COMMA"
    | COMMENT (begin_pos, end_pos) -> "COMMENT of (int * int)"
    | CONSTRAINT -> "CONSTRAINT"
    | DO -> "DO"
    | DONE -> "DONE"
    | DOT -> "DOT"
    | DOTDOT -> "DOTDOT"
    | DOWNTO -> "DOWNTO"
    | ELSE -> "ELSE"
    | END -> "END"
    | EOF -> "EOF"
    | EOF_IN_COMMENT begin_pos -> "EOF_IN_COMMENT of (int)"
    | EOF_IN_STRING begin_pos -> "EOF_IN_STRING of (int)"
    | EQUAL -> "EQUAL"
    | EXCEPTION -> "EXCEPTION"
    | EXTERNAL -> "EXTERNAL"
    | FALSE -> "FALSE"
    | FLOAT float -> "FLOAT of (string)"
    | FOR -> "FOR"
    | FUN -> "FUN"
    | FUNCTION -> "FUNCTION"
    | FUNCTOR -> "FUNCTOR"
    | GREATER -> "GREATER"
    | GREATERRBRACE -> "GREATERRBRACE"
    | GREATERRBRACKET -> "GREATERRBRACKET"
    | IF -> "IF"
    | ILLEGAL_CHAR(char) -> "ILLEGAL_CHAR(char)"
    | IN -> "IN"
    | INCLUDE -> "INCLUDE"
    | INFIXOP0(op) -> Printf.sprintf "INFIXOP0(%s)" op
    | INFIXOP1(op) -> Printf.sprintf "INFIXOP1(%s)" op
    | INFIXOP2(op) -> Printf.sprintf "INFIXOP2(%s)" op
    | INFIXOP3(op) -> Printf.sprintf "INFIXOP3(%s)" op
    | INFIXOP4(op) -> Printf.sprintf "INFIXOP4(%s)" op
    | INHERIT -> "INHERIT"
    | INITIALIZER -> "INITIALIZER"
    | INT int -> "INT(int Approx_common.overflow)"
    | INT32(int32) -> "INT32(int32 Approx_common.overflow)"
    | INT64(int64) -> "INT64(int64 Approx_common.overflow)"
    | LABEL(string) -> Printf.sprintf "LABEL(%s)" string
    | LAZY -> "LAZY"
    | LBRACE -> "LBRACE"
    | LBRACELESS -> "LBRACELESS"
    | LBRACKET -> "LBRACKET"
    | LBRACKETBAR -> "LBRACKETBAR"
    | LBRACKETLESS -> "LBRACKETLESS"
    | LBRACKETGREATER -> "LBRACKETGREATER"
    | LESS -> "LESS"
    | LESSMINUS -> "LESSMINUS"
    | LET -> "LET"
    | LIDENT string -> Printf.sprintf "LIDENT(%s)" string
    | LPAREN -> "LPAREN"
    | MATCH -> "MATCH"
    | METHOD -> "METHOD"
    | MINUS -> "MINUS"
    | MINUSDOT -> "MINUSDOT"
    | MINUSGREATER -> "MINUSGREATER"
    | MODULE -> "MODULE"
    | MUTABLE -> "MUTABLE"
    | NATIVEINT(nativeint ) -> "NATIVEINT(nativeint Approx_common.overflow)"
    | NEW -> "NEW"
    | OBJECT -> "OBJECT"
    | OF -> "OF"
    | OPEN -> "OPEN"
    | OPTLABEL(string) -> Printf.sprintf "OPTLABEL(%s)" string
    | OR -> "OR"
    | PLUS -> "PLUS"
    | PLUSDOT -> "PLUSDOT"
    | PREFIXOP(string) -> Printf.sprintf "PREFIXOP(%s)" string
    | PRIVATE -> "PRIVATE"
    | QUESTION -> "QUESTION"
    | QUESTIONQUESTION -> "QUESTIONQUESTION"
    | QUOTE -> "QUOTE"
    | RBRACE -> "RBRACE"
    | RBRACKET -> "RBRACKET"
    | REC -> "REC"
    | RPAREN -> "RPAREN"
    | SEMI -> "SEMI"
    | SEMISEMI -> "SEMISEMI"
    | SHARP -> "SHARP"
    | SIG -> "SIG"
    | STAR -> "STAR"
    | STRING(string) -> Printf.sprintf "STRING(%s)" (String.escaped string)
    | STRUCT -> "STRUCT"
    | THEN -> "THEN"
    | TILDE -> "TILDE"
    | TO -> "TO"
    | TRUE -> "TRUE"
    | TRY -> "TRY"
    | TYPE -> "TYPE"
    | UIDENT string -> Printf.sprintf  "UIDENT(%s)" string
    | UNDERSCORE -> "UNDERSCORE"
    | VAL -> "VAL"
    | VIRTUAL -> "VIRTUAL"
    | WHEN -> "WHEN"
    | WHILE -> "WHILE"
    | WITH -> "WITH"

end



module Make(Tokens : TokenSig) = struct

  open Tokens
  open OcpLang
  open Lexing

  let comment_stack = ref []
  let lines_starts = ref []

  let init () =
    comment_stack := [];
    lines_starts := []

  let comments () =
    List.rev !comment_stack

(* The table of keywords *)

  let create_hashtable n list =
    let t = Hashtbl.create n in
    List.iter (fun (x,y) -> Hashtbl.add t x y) list;
    t

  let keyword_table =
    create_hashtable 149 [
      "and", AND;
      "as", AS;
      "assert", ASSERT;
      "begin", BEGIN;
      "class", CLASS;
      "constraint", CONSTRAINT;
      "do", DO;
      "done", DONE;
      "downto", DOWNTO;
      "else", ELSE;
      "end", END;
      "exception", EXCEPTION;
      "external", EXTERNAL;
      "false", FALSE;
      "for", FOR;
      "fun", FUN;
      "function", FUNCTION;
      "functor", FUNCTOR;
      "if", IF;
      "in", IN;
      "include", INCLUDE;
      "inherit", INHERIT;
      "initializer", INITIALIZER;
      "lazy", LAZY;
      "let", LET;
      "match", MATCH;
      "method", METHOD;
      "module", MODULE;
      "mutable", MUTABLE;
      "new", NEW;
      "object", OBJECT;
      "of", OF;
      "open", OPEN;
      "or", OR;
    (*  "parser", PARSER; *)
      "private", PRIVATE;
      "rec", REC;
      "sig", SIG;
      "struct", STRUCT;
      "then", THEN;
      "to", TO;
      "true", TRUE;
      "try", TRY;
      "type", TYPE;
      "val", VAL;
      "virtual", VIRTUAL;
      "when", WHEN;
      "while", WHILE;
      "with", WITH;

      "mod", INFIXOP3("mod");
      "land", INFIXOP3("land");
      "lor", INFIXOP3("lor");
      "lxor", INFIXOP3("lxor");
      "lsl", INFIXOP4("lsl");
      "lsr", INFIXOP4("lsr");
      "asr", INFIXOP4("asr")
    ]

(* To buffer string literals *)


let initial_string_buffer = String.create 256
let string_buff = ref initial_string_buffer
let string_index = ref 0

let reset_string_buffer () =
  string_buff := initial_string_buffer;
  string_index := 0

let store_string_char c =
  if !string_index >= String.length (!string_buff) then begin
    let new_buff = String.create (String.length (!string_buff) * 2) in
      String.blit (!string_buff) 0 new_buff 0 (String.length (!string_buff));
      string_buff := new_buff
  end;
  String.unsafe_set (!string_buff) (!string_index) c;
  incr string_index

let get_stored_string () =
  let s = String.sub (!string_buff) 0 (!string_index) in
  string_buff := initial_string_buffer;
  s

(* To store the position of the beginning of a string and comment *)
let string_start_loc = ref (-1);;
let comment_start_loc = ref [];;
let in_comment () = !comment_start_loc <> [];;

(* To translate escape sequences *)

let char_for_backslash = function
  | 'n' -> '\010'
  | 'r' -> '\013'
  | 'b' -> '\008'
  | 't' -> '\009'
  | c   -> c

let can_overflow f lexbuf =
  let s = Lexing.lexeme lexbuf in
  try InRange (f s) with Failure _ -> Overflow s

let char_for_decimal_code i s =
  let c = 100 * (Char.code(s.[i]) - 48) +
           10 * (Char.code(s.[i+1]) - 48) +
                (Char.code(s.[i+2]) - 48) in
  if (c < 0 || c > 255) then
    failwith "Bad escaped decimal char"
  else Char.chr c

let char_for_hexadecimal_code lexbuf i =
  let d1 = Char.code (Lexing.lexeme_char lexbuf i) in
  let val1 = if d1 >= 97 then d1 - 87
    else if d1 >= 65 then d1 - 55
    else d1 - 48
  in
  let d2 = Char.code (Lexing.lexeme_char lexbuf (i+1)) in
  let val2 = if d2 >= 97 then d2 - 87
    else if d2 >= 65 then d2 - 55
    else d2 - 48
  in
  Char.chr (val1 * 16 + val2)

(* To convert integer literals, allowing max_int + 1 (PR#4210) *)

let cvt_int_literal s =
  - int_of_string ("-" ^ s)
let cvt_int32_literal s =
  Int32.neg (Int32.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_int64_literal s =
  Int64.neg (Int64.of_string ("-" ^ String.sub s 0 (String.length s - 1)))
let cvt_nativeint_literal s =
  Nativeint.neg (Nativeint.of_string ("-" ^ String.sub s 0 (String.length s - 1)))

(* Remove underscores from float literals *)

let remove_underscores s =
  let l = String.length s in
  let rec remove src dst =
    if src >= l then
      if dst >= l then s else String.sub s 0 dst
    else
      match s.[src] with
          '_' -> remove (src + 1) dst
        |  c  -> s.[dst] <- c; remove (src + 1) (dst + 1)
  in remove 0 0

(* Update the current location with file name and line number. *)

let update_loc lexbuf file line absolute chars =
  let pos = lexbuf.lex_curr_p in
  let new_file = match file with
    | None -> pos.pos_fname
    | Some s -> s
  in
  lexbuf.lex_curr_p <- { pos with
    pos_fname = new_file;
    pos_lnum = if absolute then line else pos.pos_lnum + line;
    pos_bol = pos.pos_cnum - chars;
  };
  lines_starts := (lexbuf.lex_curr_p.pos_lnum, lexbuf.lex_curr_p.pos_bol) :: !lines_starts;
;;

}

let newline = ('\010' | '\013' | "\013\010")
let blank = [' ' '\009' '\012']
let lowercase = ['a'-'z' '\223'-'\246' '\248'-'\255' '_']
let uppercase = ['A'-'Z' '\192'-'\214' '\216'-'\222']
let identchar =
  ['A'-'Z' 'a'-'z' '_' '\192'-'\214' '\216'-'\246' '\248'-'\255' '\'' '0'-'9']
let symbolchar =
  ['!' '$' '%' '&' '*' '+' '-' '.' '/' ':' '<' '=' '>' '?' '@' '^' '|' '~']
let decimal_literal =
  ['0'-'9'] ['0'-'9' '_']*
    let hex_literal =
      '0' ['x' 'X'] ['0'-'9' 'A'-'F' 'a'-'f']['0'-'9' 'A'-'F' 'a'-'f' '_']*
        let oct_literal =
          '0' ['o' 'O'] ['0'-'7'] ['0'-'7' '_']*
            let bin_literal =
              '0' ['b' 'B'] ['0'-'1'] ['0'-'1' '_']*
                let int_literal =
                  decimal_literal | hex_literal | oct_literal | bin_literal
let float_literal =
  ['0'-'9'] ['0'-'9' '_']*
    ('.' ['0'-'9' '_']* )?
    (['e' 'E'] ['+' '-']? ['0'-'9'] ['0'-'9' '_']*)?

    rule token = parse
      | newline
          { update_loc lexbuf None 1 false 0;
            token lexbuf
          }
      | blank +
          { token lexbuf }
      | "_"
          { UNDERSCORE }
      | "~"
          { TILDE }
      | "~" lowercase identchar * ':'
          { let s = Lexing.lexeme lexbuf in
            let name = String.sub s 1 (String.length s - 2) in
            (*
              if Hashtbl.mem keyword_table name then
              raise (Error(Keyword_as_label name, Location.curr lexbuf));
            *)
            LABEL name }
      | "?"  { QUESTION }
      | "??" { QUESTIONQUESTION }
      | "?" lowercase identchar * ':'
          { let s = Lexing.lexeme lexbuf in
            let name = String.sub s 1 (String.length s - 2) in
            (*
              if Hashtbl.mem keyword_table name then
              raise (Error(Keyword_as_label name, Location.curr lexbuf));
            *)
            OPTLABEL name }
      | lowercase identchar *
          { let s = Lexing.lexeme lexbuf in
            try
              Hashtbl.find keyword_table s
            with Not_found ->
              LIDENT s }
      | uppercase identchar *
          { UIDENT(Lexing.lexeme lexbuf) }      (* No capitalized keywords *)
      | int_literal
          { INT (can_overflow cvt_int_literal lexbuf) }
      | float_literal
          { FLOAT (remove_underscores(Lexing.lexeme lexbuf)) }
      | int_literal "l"
          { INT32 (can_overflow cvt_int32_literal lexbuf) }
      | int_literal "L"
          { INT64 (can_overflow cvt_int64_literal lexbuf) }
      | int_literal "n"
          { NATIVEINT (can_overflow cvt_nativeint_literal lexbuf) }
      | "\""
          { reset_string_buffer();
            let string_start = lexbuf.lex_start_p in
            string_start_loc := Lexing.lexeme_start lexbuf;
            let token = string lexbuf in
            lexbuf.lex_start_p <- string_start;
            token }
      | "'" newline "'"
          { update_loc lexbuf None 1 false 1;
            CHAR (InRange (Lexing.lexeme_char lexbuf 1)) }
      | "'" [^ '\\' '\'' '\010' '\013'] "'"
          { CHAR( InRange (Lexing.lexeme_char lexbuf 1)) }
      | "'\\" ['\\' '\'' '"' 'n' 't' 'b' 'r' ' '] "'"
          { CHAR( InRange (char_for_backslash (Lexing.lexeme_char lexbuf 2))) }
      | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
          { CHAR(can_overflow (char_for_decimal_code 2) lexbuf) }
      | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
          { CHAR( InRange (char_for_hexadecimal_code lexbuf 3)) }
      | "'\\" _
          { let l = Lexing.lexeme lexbuf in
            CHAR ( Overflow l )
          }
      | "(*"
          {
            let comment_start = lexbuf.lex_start_p in
            comment_start_loc := [Lexing.lexeme_start lexbuf];
            let token= comment lexbuf in
            lexbuf.lex_start_p <- comment_start;
            token
          }
      | "*)"
          {
        (*      let loc = Location.curr lexbuf in *)
        (*        Location.prerr_warning loc Warnings.Comment_not_end; *)
            lexbuf.Lexing.lex_curr_pos <- lexbuf.Lexing.lex_curr_pos - 1;
            let curpos = lexbuf.lex_curr_p in
            lexbuf.lex_curr_p <- { curpos with pos_cnum = curpos.pos_cnum - 1 };
            STAR
          }
      | "#" [' ' '\t']* (['0'-'9']+ as num) [' ' '\t']*
          ("\"" ([^ '\010' '\013' '"' ] * as name) "\"")?
          [^ '\010' '\013'] * newline
          { update_loc lexbuf name (int_of_string num) true 0;
            token lexbuf
          }
      | "#"  { SHARP }
      | "&"  { AMPERSAND }
      | "&&" { AMPERAMPER }
      | "`"  { BACKQUOTE }
      | "'"  { QUOTE }
      | "("  { LPAREN }
      | ")"  { RPAREN }
      | "*"  { STAR }
      | ","  { COMMA }
      | "->" { MINUSGREATER }
      | "."  { DOT }
      | ".." { DOTDOT }
      | ":"  { COLON }
      | "::" { COLONCOLON }
      | ":=" { COLONEQUAL }
      | ":>" { COLONGREATER }
      | ";"  { SEMI }
      | ";;" { SEMISEMI }
      | "<"  { LESS }
      | "<-" { LESSMINUS }
      | "="  { EQUAL }
      | "["  { LBRACKET }
      | "[|" { LBRACKETBAR }
      | "[<" { LBRACKETLESS }
      | "[>" { LBRACKETGREATER }
      | "]"  { RBRACKET }
      | "{"  { LBRACE }
      | "{<" { LBRACELESS }
      | "|"  { BAR }
      | "||" { BARBAR }
      | "|]" { BARRBRACKET }
      | ">"  { GREATER }
      | ">]" { GREATERRBRACKET }
      | "}"  { RBRACE }
      | ">}" { GREATERRBRACE }
      | "!"  { BANG }

      | "!=" { INFIXOP0 "!=" }
      | "+"  { PLUS }
      | "+." { PLUSDOT }
      | "-"  { MINUS }
      | "-." { MINUSDOT }

      | "!" symbolchar +
          { PREFIXOP(Lexing.lexeme lexbuf) }
      | ['~' '?'] symbolchar +
          { PREFIXOP(Lexing.lexeme lexbuf) }
      | ['=' '<' '>' '|' '&' '$'] symbolchar *
          { INFIXOP0(Lexing.lexeme lexbuf) }
      | ['@' '^'] symbolchar *
          { INFIXOP1(Lexing.lexeme lexbuf) }
      | ['+' '-'] symbolchar *
          { INFIXOP2(Lexing.lexeme lexbuf) }
      | "**" symbolchar *
          { INFIXOP4(Lexing.lexeme lexbuf) }
      | ['*' '/' '%'] symbolchar *
          { INFIXOP3(Lexing.lexeme lexbuf) }
      | eof { EOF }
      | _
          { ILLEGAL_CHAR (Lexing.lexeme_char lexbuf 0)      }

    and comment = parse
    "(*"
      { comment_start_loc := (Lexing.lexeme_start lexbuf) :: !comment_start_loc;
        comment lexbuf;
      }
      | "*)"
          { match !comment_start_loc with
            | [] -> assert false
            | [x] ->
              comment_start_loc := [];
              comment_stack := (x, Lexing.lexeme_end lexbuf) :: !comment_stack;
              COMMENT (x, Lexing.lexeme_end lexbuf)
            | _ :: l -> comment_start_loc := l;
              comment lexbuf;
          }
      | "\""
          { reset_string_buffer();
            string_start_loc := Lexing.lexeme_start lexbuf;
            let s = string lexbuf in
            reset_string_buffer ();
            match s with
              | EOF_IN_STRING _ ->
                let pos =  List.last !comment_start_loc in
                comment_start_loc := [];
                EOF_IN_COMMENT pos
              | STRING _ -> comment lexbuf
              | _ -> assert false

          }
      | "''"
          { comment lexbuf }
      | "'" newline "'"
          { update_loc lexbuf None 1 false 1;
            comment lexbuf
          }
      | "'" [^ '\\' '\'' '\010' '\013' ] "'"
          { comment lexbuf }
      | "'\\" ['\\' '"' '\'' 'n' 't' 'b' 'r' ' '] "'"
          { comment lexbuf }
      | "'\\" ['0'-'9'] ['0'-'9'] ['0'-'9'] "'"
          { comment lexbuf }
      | "'\\" 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F'] "'"
          { comment lexbuf }
      | eof
          {
            let pos = List.last !comment_start_loc in
            comment_start_loc := [];
            EOF_IN_COMMENT pos
          }
      | newline
          { update_loc lexbuf None 1 false 0;
            comment lexbuf
          }
      | _
          { comment lexbuf }

    and string = parse
    '"'
      { STRING (get_stored_string ()) }
      | '\\' newline ([' ' '\t'] * as space)
          { update_loc lexbuf None 1 false (String.length space);
            string lexbuf
          }
      | '\\' ['\\' '\'' '"' 'n' 't' 'b' 'r' ' ']
          { store_string_char(char_for_backslash(Lexing.lexeme_char lexbuf 1));
            string lexbuf }
      | '\\' ['0'-'9'] ['0'-'9'] ['0'-'9']
          { store_string_char(char_for_decimal_code 1 (Lexing.lexeme lexbuf));
            string lexbuf }
      | '\\' 'x' ['0'-'9' 'a'-'f' 'A'-'F'] ['0'-'9' 'a'-'f' 'A'-'F']
          { store_string_char(char_for_hexadecimal_code lexbuf 2);
            string lexbuf }
      | '\\' _
          { if in_comment ()
            then string lexbuf
            else begin
          (*  Should be an error, but we are very lax.
              raise (Error (Illegal_escape (Lexing.lexeme lexbuf),
              Location.curr lexbuf))
          *)
              store_string_char (Lexing.lexeme_char lexbuf 0);
              store_string_char (Lexing.lexeme_char lexbuf 1);
              string lexbuf
            end
          }
      | newline
          {
            update_loc lexbuf None 1 false 0;
            let s = Lexing.lexeme lexbuf in
            for i = 0 to String.length s - 1 do
              store_string_char s.[i];
            done;
            string lexbuf
          }
      | eof
          { EOF_IN_STRING !string_start_loc }
      | _
          { store_string_char(Lexing.lexeme_char lexbuf 0);
            string lexbuf }

          {

          let rec token_locs lexbuf =
            match token lexbuf with
                COMMENT _ -> token_locs lexbuf
              | EOF_IN_COMMENT _ ->
                EOF, ( lexbuf.lex_start_p, lexbuf.lex_start_p)
              | EOF_IN_STRING _ ->
                EOF, ( lexbuf.lex_start_p, lexbuf.lex_start_p)
              | token ->
                token, ( lexbuf.lex_start_p, lexbuf.lex_curr_p)

          let rec token_pos lexbuf =
            match token lexbuf with
                COMMENT _ -> token_pos lexbuf
              | EOF_IN_COMMENT _ ->
                EOF, ( lexbuf.lex_start_p.pos_cnum, lexbuf.lex_start_p.pos_cnum)
              | EOF_IN_STRING _ ->
                EOF, ( lexbuf.lex_start_p.pos_cnum, lexbuf.lex_start_p.pos_cnum)
              | token ->
                token, ( lexbuf.lex_start_p.pos_cnum, lexbuf.lex_curr_p.pos_cnum)


          let token_locs_and_comments lexbuf =
              let token = token lexbuf in
              token,  ( lexbuf.lex_start_p, lexbuf.lex_curr_p)

          let get_token = token

          let token_with_comments = get_token

          let rec token lexbuf =
            match get_token lexbuf with
                COMMENT _ -> token lexbuf
              | EOF_IN_COMMENT _
              | EOF_IN_STRING _ -> EOF
              | tok -> tok

	  let tokens_of_file filename =
	    let ic = open_in filename in
	    try
	      init ();
	      let lexbuf = Lexing.from_channel ic in
	      let rec iter tokens =
		let token = token_pos lexbuf in
		match token with
		    (EOF, _) -> List.rev tokens
		  | _ -> iter (token :: tokens)
	      in
	      let tokens = iter [] in
	      close_in ic;
	      tokens
	    with e -> close_in ic; raise e

          let tokens_with_loc_of_string s =
	      init ();
	      let lexbuf = Lexing.from_string s in
	      let rec iter tokens =
		let token = token_pos lexbuf in
		match token with
		    (EOF, _) -> List.rev tokens
		  | _ -> iter (token :: tokens)
	      in
	      let tokens = iter [] in
	      tokens

          let tokens_of_string s =
	      init ();
	      let lexbuf = Lexing.from_string s in
	      let rec iter tokens =
		let token = token lexbuf in
		match token with
		    (EOF) -> List.rev tokens
		  | _ -> iter (token :: tokens)
	      in
	      let tokens = iter [] in
	      tokens

	  let lines () = List.rev ( !lines_starts )

          include StringOfToken(Tokens)

	   end

    include TokenStruct
    include (Make(TokenStruct))
}
