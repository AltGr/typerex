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

(* ADMIN: fabrice *)

module type Sig = sig

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
  | CHAR of (char Approx_common.overflow)
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
  | INT of (int Approx_common.overflow)
  | INT32 of (int32 Approx_common.overflow)
  | INT64 of (int64 Approx_common.overflow)
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
  | NATIVEINT of (nativeint Approx_common.overflow)
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


module Struct : Sig = struct

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
  | CHAR of (char Approx_common.overflow)
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
  | INT of (int Approx_common.overflow)
  | INT32 of (int32 Approx_common.overflow)
  | INT64 of (int64 Approx_common.overflow)
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
  | NATIVEINT of (nativeint Approx_common.overflow)
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


module StringOfToken(S : Sig) = struct

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
