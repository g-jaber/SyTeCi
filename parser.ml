
module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | VAR of (
# 8 "parser.mly"
       (Syntax.id)
# 11 "parser.ml"
  )
    | UNIT
    | TUNIT
    | TRUE
    | TINT
    | THEN
    | TBOOL
    | SEMICOLON
    | RPAR
    | REF
    | REC
    | PLUS
    | NOT
    | NEQ
    | MULT
    | MINUS
    | LPAR
    | LOR
    | LET
    | LESSEQ
    | LESS
    | LAND
    | INT of (
# 7 "parser.mly"
       (int)
# 37 "parser.ml"
  )
    | IN
    | IF
    | GREATEQ
    | GREAT
    | FUN
    | FIX
    | FALSE
    | EQ
    | EOF
    | ELSE
    | DIV
    | DEREF
    | COMMA
    | COLON
    | ASSIGN
    | ARROW
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState103
  | MenhirState99
  | MenhirState97
  | MenhirState90
  | MenhirState88
  | MenhirState87
  | MenhirState86
  | MenhirState85
  | MenhirState81
  | MenhirState78
  | MenhirState76
  | MenhirState72
  | MenhirState70
  | MenhirState68
  | MenhirState66
  | MenhirState64
  | MenhirState62
  | MenhirState60
  | MenhirState58
  | MenhirState56
  | MenhirState54
  | MenhirState52
  | MenhirState50
  | MenhirState48
  | MenhirState46
  | MenhirState44
  | MenhirState38
  | MenhirState36
  | MenhirState35
  | MenhirState34
  | MenhirState32
  | MenhirState31
  | MenhirState29
  | MenhirState24
  | MenhirState22
  | MenhirState19
  | MenhirState18
  | MenhirState14
  | MenhirState9
  | MenhirState8
  | MenhirState6
  | MenhirState5
  | MenhirState4
  | MenhirState0

# 1 "parser.mly"
  
  open Syntax

# 120 "parser.ml"

let rec _menhir_run22 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | REF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TBOOL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | TUNIT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState22
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState22

and _menhir_run24 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | REF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | TBOOL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | TINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | TUNIT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState24
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState24

and _menhir_run44 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState44
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState44 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState44

and _menhir_run46 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState46
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState46 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState46

and _menhir_run54 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState54
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState54 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState54

and _menhir_run48 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState48
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState48 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState48

and _menhir_run56 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState56
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState56 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState56

and _menhir_run58 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState58
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState58 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState58

and _menhir_run62 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState62
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState62 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState62

and _menhir_run64 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState64
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState64 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState64

and _menhir_run60 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState60
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState60 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState60

and _menhir_run66 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState66
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState66 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState66

and _menhir_run68 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState68
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState68 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState68

and _menhir_run70 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState70
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState70 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState70

and _menhir_run52 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState52
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState52 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState52

and _menhir_run72 : _menhir_env -> 'ttv_tail * _menhir_state * 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState72
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState72 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState72

and _menhir_goto_ty : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_ty -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv391 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv387 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv385 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_ty = 
# 105 "parser.mly"
                 ( _2 )
# 692 "parser.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv386)) : 'freshtv388)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv389 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv390)) : 'freshtv392)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv395 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv393 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), _, (_3 : 'tv_ty)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_ty = 
# 104 "parser.mly"
                 ( TProd (_1, _3) )
# 712 "parser.ml"
         in
        _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv394)) : 'freshtv396)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv401 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | MULT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | ARROW | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv397 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_ty)), _, (_3 : 'tv_ty)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_ty = 
# 103 "parser.mly"
                ( TArrow (_1, _3) )
# 731 "parser.ml"
             in
            _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv398)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv399 * _menhir_state * 'tv_ty)) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv400)) : 'freshtv402)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv405 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv403 * _menhir_state) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_ty)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_ty = 
# 102 "parser.mly"
                ( TRef _2 )
# 751 "parser.ml"
         in
        _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv404)) : 'freshtv406)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv413 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 759 "parser.ml"
        ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            _menhir_run24 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv409 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 773 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv407 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 780 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), (_2 : (
# 8 "parser.mly"
       (Syntax.id)
# 785 "parser.ml"
            ))), _, (_4 : 'tv_ty)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_typed_ident = 
# 92 "parser.mly"
                           ( (_2,_4) )
# 793 "parser.ml"
             in
            _menhir_goto_typed_ident _menhir_env _menhir_stack _menhir_s _v) : 'freshtv408)) : 'freshtv410)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv411 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 803 "parser.ml"
            ))) * _menhir_state * 'tv_ty) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv412)) : 'freshtv414)
    | _ ->
        _menhir_fail ()

and _menhir_goto_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv201 * _menhir_state) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_typed_ident)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | IN | RPAR | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv197 * _menhir_state) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_typed_ident)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_typed_ident)), _, (_3 : 'tv_typed_ident)), _, (_5 : 'tv_expr)) = _menhir_stack in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 51 "parser.mly"
    ( Fix (_2,_3, _5) )
# 857 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv198)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv199 * _menhir_state) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_typed_ident)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv200)) : 'freshtv202)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv207 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | IN | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv203 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 47 "parser.mly"
                                ( Seq (_1, _3) )
# 907 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv204)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv205 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv206)) : 'freshtv208)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv213 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | EQ | GREAT | GREATEQ | IN | LAND | LESS | LESSEQ | LOR | MINUS | NEQ | PLUS | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv209 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 61 "parser.mly"
                       ( Plus (_1, _3) )
# 935 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv210)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv211 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv212)) : 'freshtv214)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv217 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv215 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 63 "parser.mly"
                       ( Mult (_1, _3) )
# 955 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv216)) : 'freshtv218)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv221 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv219 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
        let _2 = () in
        let _v : 'tv_expr = 
# 64 "parser.mly"
                       ( Div (_1, _3) )
# 968 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv220)) : 'freshtv222)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv227 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | IN | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv223 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 69 "parser.mly"
                      ( NEqual (_1, _3) )
# 997 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv224)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv225 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv226)) : 'freshtv228)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv233 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | EQ | GREAT | GREATEQ | IN | LAND | LESS | LESSEQ | LOR | MINUS | NEQ | PLUS | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv229 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 62 "parser.mly"
                       ( Minus (_1, _3) )
# 1025 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv230)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv231 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv232)) : 'freshtv234)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv239 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | EQ | GREAT | GREATEQ | IN | LESS | LESSEQ | LOR | NEQ | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv235 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 67 "parser.mly"
                       ( Or (_1, _3) )
# 1059 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv236)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv237 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv238)) : 'freshtv240)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv245 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | EQ | GREAT | GREATEQ | IN | LAND | LESS | LESSEQ | LOR | NEQ | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv241 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 66 "parser.mly"
                       ( And (_1, _3) )
# 1091 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv242)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv243 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv244)) : 'freshtv246)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv251 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | IN | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv247 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 73 "parser.mly"
                      ( LessEq (_1, _3) )
# 1127 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv248)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv249 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv250)) : 'freshtv252)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv257 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | IN | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv253 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 72 "parser.mly"
                      ( Less (_1, _3) )
# 1163 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv254)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv255 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv256)) : 'freshtv258)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv263 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | IN | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv259 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 71 "parser.mly"
                       ( GreatEq (_1, _3) )
# 1199 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv260)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv261 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv262)) : 'freshtv264)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv269 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | IN | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv265 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 70 "parser.mly"
                       ( Great (_1, _3) )
# 1235 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv266)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv267 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv268)) : 'freshtv270)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv275 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | IN | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv271 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 68 "parser.mly"
                      ( Equal (_1, _3) )
# 1271 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv272)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv273 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv274)) : 'freshtv276)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv281 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | IN | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv277 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_expr)), _, (_3 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : 'tv_expr = 
# 59 "parser.mly"
                     ( Assign (_1,_3) )
# 1319 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv278)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv279 * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv280)) : 'freshtv282)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv287 * _menhir_state) * _menhir_state * 'tv_typed_ident)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | IN | RPAR | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv283 * _menhir_state) * _menhir_state * 'tv_typed_ident)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_typed_ident)), _, (_4 : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 49 "parser.mly"
                               ( Fun (_2, _4) )
# 1372 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv284)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv285 * _menhir_state) * _menhir_state * 'tv_typed_ident)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv286)) : 'freshtv288)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv293 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv289 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | FALSE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | FIX ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | LPAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | UNIT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState76
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState76 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState76) : 'freshtv290)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv291 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv292)) : 'freshtv294)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv299 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | ELSE ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv295 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | FALSE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | FIX ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | LPAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | UNIT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState78
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState78 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState78) : 'freshtv296)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv297 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv298)) : 'freshtv300)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv305 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | IN | RPAR | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv301 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_4 : 'tv_expr)), _, (_6 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 48 "parser.mly"
                                       ( If (_2, _4, _6) )
# 1580 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv302)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((('freshtv303 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv304)) : 'freshtv306)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv311 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 1595 "parser.ml"
        )) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv307 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 1615 "parser.ml"
            )) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | FALSE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | FIX ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | LPAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | UNIT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState81
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState81 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState81) : 'freshtv308)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv309 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 1675 "parser.ml"
            )) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv310)) : 'freshtv312)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv317 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 1684 "parser.ml"
        )) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | IN | RPAR | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv313 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 1722 "parser.ml"
            )) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((((((_menhir_stack, _menhir_s), (_2 : (
# 8 "parser.mly"
       (Syntax.id)
# 1727 "parser.ml"
            ))), _, (_3 : 'tv_list_ident)), _), _, (_5 : 'tv_expr)), _, (_7 : 'tv_expr)) = _menhir_stack in
            let _6 = () in
            let _4 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 53 "parser.mly"
    ( Let (_2, List.fold_left (fun expr var -> Fun (var,expr)) _5 _3, _7) )
# 1735 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv314)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv315 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 1745 "parser.ml"
            )) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv316)) : 'freshtv318)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv323 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 1754 "parser.ml"
        )) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv319 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 1774 "parser.ml"
            )) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | FALSE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | FIX ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | LPAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | UNIT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState90
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState90 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState90) : 'freshtv320)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((('freshtv321 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 1834 "parser.ml"
            )) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv322)) : 'freshtv324)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv329 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 1843 "parser.ml"
        )) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | IN | RPAR | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv325 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 1881 "parser.ml"
            )) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((((((_menhir_stack, _menhir_s), (_3 : (
# 8 "parser.mly"
       (Syntax.id)
# 1886 "parser.ml"
            ))), _, (_4 : 'tv_typed_ident)), _, (_5 : 'tv_list_ident)), _), _, (_7 : 'tv_expr)), _, (_9 : 'tv_expr)) = _menhir_stack in
            let _8 = () in
            let _6 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 55 "parser.mly"
    ( Let (_3, Fix ((_3,TUndef),_4, List.fold_left (fun expr var -> Fun (var,expr)) _7 _5), _9) )
# 1895 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv326)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((((((('freshtv327 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 1905 "parser.ml"
            )) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv328)) : 'freshtv330)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv335 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 1914 "parser.ml"
        ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 1918 "parser.ml"
        )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | IN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv331 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 1938 "parser.ml"
            ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 1942 "parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | FALSE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | FIX ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | LPAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | UNIT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState99
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState99 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState99) : 'freshtv332)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((('freshtv333 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 2002 "parser.ml"
            ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 2006 "parser.ml"
            )))) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv334)) : 'freshtv336)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((((('freshtv341 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 2015 "parser.ml"
        ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 2019 "parser.ml"
        )))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | COMMA | ELSE | EOF | IN | RPAR | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv337 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 2057 "parser.ml"
            ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 2061 "parser.ml"
            )))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((((_menhir_stack, _menhir_s), (_3 : (
# 8 "parser.mly"
       (Syntax.id)
# 2066 "parser.ml"
            ))), (_5 : (
# 8 "parser.mly"
       (Syntax.id)
# 2070 "parser.ml"
            ))), _, (_8 : 'tv_expr)), _, (_10 : 'tv_expr)) = _menhir_stack in
            let _9 = () in
            let _7 = () in
            let _6 = () in
            let _4 = () in
            let _2 = () in
            let _1 = () in
            let _v : 'tv_expr = 
# 57 "parser.mly"
    (LetPair (_3,_5,_8,_10))
# 2081 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv338)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((((((((('freshtv339 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 2091 "parser.ml"
            ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 2095 "parser.ml"
            )))) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv340)) : 'freshtv342)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv351 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | COMMA ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv343 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | FALSE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | FIX ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | LPAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | UNIT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState103
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState103 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState103) : 'freshtv344)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv347 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv345 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_simple_expr = 
# 87 "parser.mly"
                     ( _2 )
# 2179 "parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv346)) : 'freshtv348)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv349 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv350)) : 'freshtv352)
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv359 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | RPAR ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv355 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv353 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)), _, (_4 : 'tv_expr)) = _menhir_stack in
            let _5 = () in
            let _3 = () in
            let _1 = () in
            let _v : 'tv_simple_expr = 
# 85 "parser.mly"
                                ( Pair (_2, _4) )
# 2236 "parser.ml"
             in
            _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv354)) : 'freshtv356)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ((('freshtv357 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv358)) : 'freshtv360)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv365 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | ASSIGN | COMMA | ELSE | EOF | IN | RPAR | SEMICOLON | THEN ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv361 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
            let _1 = () in
            let _v : 'tv_expr = 
# 65 "parser.mly"
                      ( Not (_2) )
# 2286 "parser.ml"
             in
            _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv362)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv363 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv364)) : 'freshtv366)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv369 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv367 * _menhir_state) * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _, (_2 : 'tv_expr)) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_expr = 
# 58 "parser.mly"
                     ( Newref _2 )
# 2306 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv368)) : 'freshtv370)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv383 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ASSIGN ->
            _menhir_run72 _menhir_env (Obj.magic _menhir_stack)
        | DIV ->
            _menhir_run52 _menhir_env (Obj.magic _menhir_stack)
        | EOF ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv379 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv377 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, (_1 : 'tv_expr)) = _menhir_stack in
            let _2 = () in
            let _v : (
# 38 "parser.mly"
      (Syntax.exprML)
# 2329 "parser.ml"
            ) = 
# 43 "parser.mly"
                 ( _1 )
# 2333 "parser.ml"
             in
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv375) = _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 38 "parser.mly"
      (Syntax.exprML)
# 2341 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv373) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let (_v : (
# 38 "parser.mly"
      (Syntax.exprML)
# 2349 "parser.ml"
            )) = _v in
            ((let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv371) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = _menhir_s in
            let ((_1 : (
# 38 "parser.mly"
      (Syntax.exprML)
# 2357 "parser.ml"
            )) : (
# 38 "parser.mly"
      (Syntax.exprML)
# 2361 "parser.ml"
            )) = _v in
            (Obj.magic _1 : 'freshtv372)) : 'freshtv374)) : 'freshtv376)) : 'freshtv378)) : 'freshtv380)
        | EQ ->
            _menhir_run70 _menhir_env (Obj.magic _menhir_stack)
        | GREAT ->
            _menhir_run68 _menhir_env (Obj.magic _menhir_stack)
        | GREATEQ ->
            _menhir_run66 _menhir_env (Obj.magic _menhir_stack)
        | LAND ->
            _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
        | LESS ->
            _menhir_run64 _menhir_env (Obj.magic _menhir_stack)
        | LESSEQ ->
            _menhir_run62 _menhir_env (Obj.magic _menhir_stack)
        | LOR ->
            _menhir_run58 _menhir_env (Obj.magic _menhir_stack)
        | MINUS ->
            _menhir_run56 _menhir_env (Obj.magic _menhir_stack)
        | MULT ->
            _menhir_run48 _menhir_env (Obj.magic _menhir_stack)
        | NEQ ->
            _menhir_run54 _menhir_env (Obj.magic _menhir_stack)
        | PLUS ->
            _menhir_run46 _menhir_env (Obj.magic _menhir_stack)
        | SEMICOLON ->
            _menhir_run44 _menhir_env (Obj.magic _menhir_stack)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : 'freshtv381 * _menhir_state * 'tv_expr) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv382)) : 'freshtv384)
    | _ ->
        _menhir_fail ()

and _menhir_goto_list_ident : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_list_ident -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv191 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 2407 "parser.ml"
        )) * _menhir_state * 'tv_list_ident) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv189 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 2417 "parser.ml"
            )) * _menhir_state * 'tv_list_ident) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState9 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | FALSE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | FIX ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | LPAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | UNIT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState29
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState29 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState29) : 'freshtv190)
        | LPAR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | UNIT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState9
        | VAR _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState9 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState9) : 'freshtv192)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv195 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 2469 "parser.ml"
        )) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_list_ident) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | EQ ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (((('freshtv193 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 2479 "parser.ml"
            )) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_list_ident) = Obj.magic _menhir_stack in
            let (_menhir_s : _menhir_state) = MenhirState87 in
            ((let _menhir_stack = (_menhir_stack, _menhir_s) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | FALSE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | FIX ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | LPAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | UNIT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState88
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState88 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState88) : 'freshtv194)
        | LPAR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | UNIT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState87
        | VAR _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState87 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState87) : 'freshtv196)
    | _ ->
        _menhir_fail ()

and _menhir_goto_typed_ident : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_typed_ident -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv173 * _menhir_state) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv169 * _menhir_state) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | FALSE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | FIX ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | LPAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | UNIT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState34
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState34 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState34) : 'freshtv170)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv171 * _menhir_state) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv172)) : 'freshtv174)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv175 * _menhir_state) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | LPAR ->
            _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | UNIT ->
            _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState36
        | VAR _v ->
            _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState36 _v
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState36) : 'freshtv176)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv181 * _menhir_state) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
        ((assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | ARROW ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv177 * _menhir_state) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | DEREF ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | FALSE ->
                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | FIX ->
                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | FUN ->
                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | IF ->
                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | INT _v ->
                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | LET ->
                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | LPAR ->
                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | NOT ->
                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | REF ->
                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | TRUE ->
                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | UNIT ->
                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState38
            | VAR _v ->
                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState38 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState38) : 'freshtv178)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : (('freshtv179 * _menhir_state) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv180)) : 'freshtv182)
    | MenhirState87 | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv185 * _menhir_state * 'tv_list_ident) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv183 * _menhir_state * 'tv_list_ident) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s, (_1 : 'tv_list_ident)), _, (_2 : 'tv_typed_ident)) = _menhir_stack in
        let _v : 'tv_list_ident = 
# 96 "parser.mly"
                           (_2::_1)
# 2656 "parser.ml"
         in
        _menhir_goto_list_ident _menhir_env _menhir_stack _menhir_s _v) : 'freshtv184)) : 'freshtv186)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv187 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 2664 "parser.ml"
        )) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
        (_menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState86 : 'freshtv188)
    | _ ->
        _menhir_fail ()

and _menhir_run15 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv167) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_ty = 
# 99 "parser.mly"
                 ( TUnit )
# 2680 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv168)

and _menhir_run16 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv165) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_ty = 
# 101 "parser.mly"
                 ( TInt )
# 2694 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv166)

and _menhir_run17 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv163) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_ty = 
# 100 "parser.mly"
                 ( TBool )
# 2708 "parser.ml"
     in
    _menhir_goto_ty _menhir_env _menhir_stack _menhir_s _v) : 'freshtv164)

and _menhir_run18 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | REF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TBOOL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | TUNIT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState18
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState18

and _menhir_run19 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | REF ->
        _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | TBOOL ->
        _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | TINT ->
        _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | TUNIT ->
        _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState19
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState19

and _menhir_fail : unit -> 'a =
  fun () ->
    Printf.fprintf Pervasives.stderr "Internal failure -- please contact the parser generator's developers.\n%!";
    assert false

and _menhir_goto_app_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_app_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv161 * _menhir_state * 'tv_app_expr) = Obj.magic _menhir_stack in
    ((assert (not _menhir_env._menhir_error);
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState50
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState50 _v
    | ASSIGN | COMMA | DIV | ELSE | EOF | EQ | GREAT | GREATEQ | IN | LAND | LESS | LESSEQ | LOR | MINUS | MULT | NEQ | PLUS | RPAR | SEMICOLON | THEN ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv159 * _menhir_state * 'tv_app_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_app_expr)) = _menhir_stack in
        let _v : 'tv_expr = 
# 46 "parser.mly"
             ( _1 )
# 2788 "parser.ml"
         in
        _menhir_goto_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv160)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState50) : 'freshtv162)

and _menhir_reduce26 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : 'tv_list_ident = 
# 95 "parser.mly"
     ( [] )
# 2801 "parser.ml"
     in
    _menhir_goto_list_ident _menhir_env _menhir_stack _menhir_s _v

and _menhir_run10 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (Syntax.id)
# 2808 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv157) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 8 "parser.mly"
       (Syntax.id)
# 2818 "parser.ml"
    )) : (
# 8 "parser.mly"
       (Syntax.id)
# 2822 "parser.ml"
    )) = _v in
    ((let _v : 'tv_typed_ident = 
# 91 "parser.mly"
        ( (_1,TUndef) )
# 2827 "parser.ml"
     in
    _menhir_goto_typed_ident _menhir_env _menhir_stack _menhir_s _v) : 'freshtv158)

and _menhir_run11 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv155) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_typed_ident = 
# 90 "parser.mly"
         ( let var = fresh_evar () in (var,TUnit) )
# 2841 "parser.ml"
     in
    _menhir_goto_typed_ident _menhir_env _menhir_stack _menhir_s _v) : 'freshtv156)

and _menhir_run12 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv151 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "parser.mly"
       (Syntax.id)
# 2857 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | COLON ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv147 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 2868 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAR ->
                _menhir_run19 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | REF ->
                _menhir_run18 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | TBOOL ->
                _menhir_run17 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | TINT ->
                _menhir_run16 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | TUNIT ->
                _menhir_run15 _menhir_env (Obj.magic _menhir_stack) MenhirState14
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState14) : 'freshtv148)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv149 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 2894 "parser.ml"
            )) = Obj.magic _menhir_stack in
            ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv150)) : 'freshtv152)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv153 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv154)

and _menhir_goto_simple_expr : _menhir_env -> 'ttv_tail -> _menhir_state -> 'tv_simple_expr -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    match _menhir_s with
    | MenhirState0 | MenhirState4 | MenhirState5 | MenhirState103 | MenhirState6 | MenhirState99 | MenhirState97 | MenhirState90 | MenhirState88 | MenhirState81 | MenhirState29 | MenhirState78 | MenhirState76 | MenhirState31 | MenhirState34 | MenhirState72 | MenhirState70 | MenhirState68 | MenhirState66 | MenhirState64 | MenhirState62 | MenhirState60 | MenhirState58 | MenhirState56 | MenhirState54 | MenhirState52 | MenhirState48 | MenhirState46 | MenhirState44 | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv141) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_simple_expr) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv139) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let ((_1 : 'tv_simple_expr) : 'tv_simple_expr) = _v in
        ((let _v : 'tv_app_expr = 
# 76 "parser.mly"
                ( _1 )
# 2921 "parser.ml"
         in
        _menhir_goto_app_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv140)) : 'freshtv142)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv145 * _menhir_state * 'tv_app_expr) = Obj.magic _menhir_stack in
        let (_menhir_s : _menhir_state) = _menhir_s in
        let (_v : 'tv_simple_expr) = _v in
        ((let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv143 * _menhir_state * 'tv_app_expr) = Obj.magic _menhir_stack in
        let (_ : _menhir_state) = _menhir_s in
        let ((_2 : 'tv_simple_expr) : 'tv_simple_expr) = _v in
        ((let (_menhir_stack, _menhir_s, (_1 : 'tv_app_expr)) = _menhir_stack in
        let _v : 'tv_app_expr = 
# 77 "parser.mly"
                                 ( App (_1, _2) )
# 2937 "parser.ml"
         in
        _menhir_goto_app_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv144)) : 'freshtv146)
    | _ ->
        _menhir_fail ()

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState103 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv51 * _menhir_state) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv52)
    | MenhirState99 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((((('freshtv53 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 2956 "parser.ml"
        ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 2960 "parser.ml"
        )))) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv54)
    | MenhirState97 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((((('freshtv55 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 2969 "parser.ml"
        ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 2973 "parser.ml"
        )))) = Obj.magic _menhir_stack in
        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv56)
    | MenhirState90 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((((('freshtv57 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 2982 "parser.ml"
        )) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv58)
    | MenhirState88 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv59 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 2991 "parser.ml"
        )) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_list_ident) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv60)
    | MenhirState87 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv61 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 3000 "parser.ml"
        )) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_list_ident) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv62)
    | MenhirState86 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv63 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 3009 "parser.ml"
        )) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv64)
    | MenhirState85 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv65 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 3018 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv66)
    | MenhirState81 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((((('freshtv67 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 3027 "parser.ml"
        )) * _menhir_state * 'tv_list_ident) * _menhir_state) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv68)
    | MenhirState78 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (((('freshtv69 * _menhir_state) * _menhir_state * 'tv_expr)) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv70)
    | MenhirState76 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv71 * _menhir_state) * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv72)
    | MenhirState72 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv73 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv74)
    | MenhirState70 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv75 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv76)
    | MenhirState68 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv77 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv78)
    | MenhirState66 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv79 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv80)
    | MenhirState64 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv81 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv82)
    | MenhirState62 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv83 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv84)
    | MenhirState60 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv85 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv86)
    | MenhirState58 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv87 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv88)
    | MenhirState56 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv89 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv90)
    | MenhirState54 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv91 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv92)
    | MenhirState52 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv93 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv94)
    | MenhirState50 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv95 * _menhir_state * 'tv_app_expr) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv96)
    | MenhirState48 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv97 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv98)
    | MenhirState46 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv99 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv100)
    | MenhirState44 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv101 * _menhir_state * 'tv_expr)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv102)
    | MenhirState38 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv103 * _menhir_state) * _menhir_state * 'tv_typed_ident) * _menhir_state * 'tv_typed_ident)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv104)
    | MenhirState36 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv105 * _menhir_state) * _menhir_state * 'tv_typed_ident) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv106)
    | MenhirState35 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv107 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv108)
    | MenhirState34 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv109 * _menhir_state) * _menhir_state * 'tv_typed_ident)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv110)
    | MenhirState32 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv111 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv112)
    | MenhirState31 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv113 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv114)
    | MenhirState29 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ((('freshtv115 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 3151 "parser.ml"
        )) * _menhir_state * 'tv_list_ident) * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv116)
    | MenhirState24 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv117 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv118)
    | MenhirState22 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv119 * _menhir_state * 'tv_ty)) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv120)
    | MenhirState19 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv121 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv122)
    | MenhirState18 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv123 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv124)
    | MenhirState14 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv125 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 3180 "parser.ml"
        ))) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv126)
    | MenhirState9 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : (('freshtv127 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 3189 "parser.ml"
        )) * _menhir_state * 'tv_list_ident) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s, _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv128)
    | MenhirState8 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : ('freshtv129 * _menhir_state) * (
# 8 "parser.mly"
       (Syntax.id)
# 3198 "parser.ml"
        )) = Obj.magic _menhir_stack in
        ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv130)
    | MenhirState6 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv131 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv132)
    | MenhirState5 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv133 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv134)
    | MenhirState4 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv135 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv136)
    | MenhirState0 ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv137) = Obj.magic _menhir_stack in
        (raise _eRR : 'freshtv138)

and _menhir_run1 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 8 "parser.mly"
       (Syntax.id)
# 3225 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv49) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 8 "parser.mly"
       (Syntax.id)
# 3235 "parser.ml"
    )) : (
# 8 "parser.mly"
       (Syntax.id)
# 3239 "parser.ml"
    )) = _v in
    ((let _v : 'tv_simple_expr = 
# 80 "parser.mly"
                    ( Var _1 )
# 3244 "parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv50)

and _menhir_run2 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv47) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_simple_expr = 
# 81 "parser.mly"
                    ( Unit )
# 3258 "parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv48)

and _menhir_run3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv45) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_simple_expr = 
# 83 "parser.mly"
                    ( Bool true )
# 3272 "parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv46)

and _menhir_run4 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState4
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState4 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState4

and _menhir_run5 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState5
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState5 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState5

and _menhir_run6 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState6
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState6 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState6

and _menhir_run7 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv33 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | VAR _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv29 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 8 "parser.mly"
       (Syntax.id)
# 3405 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | COMMA ->
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv25 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 3416 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                match _tok with
                | VAR _v ->
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv21 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 3426 "parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    let (_v : (
# 8 "parser.mly"
       (Syntax.id)
# 3431 "parser.ml"
                    )) = _v in
                    ((let _menhir_stack = (_menhir_stack, _v) in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    match _tok with
                    | RPAR ->
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv17 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 3442 "parser.ml"
                        ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 3446 "parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        match _tok with
                        | EQ ->
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((((('freshtv13 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 3456 "parser.ml"
                            ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 3460 "parser.ml"
                            ))) = Obj.magic _menhir_stack in
                            ((let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            match _tok with
                            | DEREF ->
                                _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                            | FALSE ->
                                _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                            | FIX ->
                                _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                            | FUN ->
                                _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                            | IF ->
                                _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                            | INT _v ->
                                _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
                            | LET ->
                                _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                            | LPAR ->
                                _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                            | NOT ->
                                _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                            | REF ->
                                _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                            | TRUE ->
                                _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                            | UNIT ->
                                _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState97
                            | VAR _v ->
                                _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState97 _v
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState97) : 'freshtv14)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let (_menhir_env : _menhir_env) = _menhir_env in
                            let (_menhir_stack : ((((('freshtv15 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 3502 "parser.ml"
                            ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 3506 "parser.ml"
                            ))) = Obj.magic _menhir_stack in
                            ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv16)) : 'freshtv18)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let (_menhir_env : _menhir_env) = _menhir_env in
                        let (_menhir_stack : (((('freshtv19 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 3517 "parser.ml"
                        ))) * (
# 8 "parser.mly"
       (Syntax.id)
# 3521 "parser.ml"
                        )) = Obj.magic _menhir_stack in
                        ((let (((_menhir_stack, _menhir_s), _), _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv20)) : 'freshtv22)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let (_menhir_env : _menhir_env) = _menhir_env in
                    let (_menhir_stack : ((('freshtv23 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 3532 "parser.ml"
                    ))) = Obj.magic _menhir_stack in
                    ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv24)) : 'freshtv26)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let (_menhir_env : _menhir_env) = _menhir_env in
                let (_menhir_stack : (('freshtv27 * _menhir_state)) * (
# 8 "parser.mly"
       (Syntax.id)
# 3543 "parser.ml"
                )) = Obj.magic _menhir_stack in
                ((let ((_menhir_stack, _menhir_s), _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv28)) : 'freshtv30)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv31 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv32)) : 'freshtv34)
    | REC ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv39 * _menhir_state) = Obj.magic _menhir_stack in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | VAR _v ->
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv35 * _menhir_state)) = Obj.magic _menhir_stack in
            let (_v : (
# 8 "parser.mly"
       (Syntax.id)
# 3566 "parser.ml"
            )) = _v in
            ((let _menhir_stack = (_menhir_stack, _v) in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            match _tok with
            | LPAR ->
                _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | UNIT ->
                _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState85
            | VAR _v ->
                _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState85 _v
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState85) : 'freshtv36)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let (_menhir_env : _menhir_env) = _menhir_env in
            let (_menhir_stack : ('freshtv37 * _menhir_state)) = Obj.magic _menhir_stack in
            ((let (_menhir_stack, _menhir_s) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv38)) : 'freshtv40)
    | VAR _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv41 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "parser.mly"
       (Syntax.id)
# 3595 "parser.ml"
        )) = _v in
        ((let _menhir_stack = (_menhir_stack, _v) in
        let _menhir_env = _menhir_discard _menhir_env in
        _menhir_reduce26 _menhir_env (Obj.magic _menhir_stack) MenhirState8) : 'freshtv42)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv43 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv44)

and _menhir_run30 : _menhir_env -> 'ttv_tail -> _menhir_state -> (
# 7 "parser.mly"
       (int)
# 3611 "parser.ml"
) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv11) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    let ((_1 : (
# 7 "parser.mly"
       (int)
# 3621 "parser.ml"
    )) : (
# 7 "parser.mly"
       (int)
# 3625 "parser.ml"
    )) = _v in
    ((let _v : 'tv_simple_expr = 
# 82 "parser.mly"
                    ( Int _1 )
# 3630 "parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv12)

and _menhir_run31 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState31
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState31 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState31

and _menhir_run32 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | UNIT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState32
    | VAR _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState32 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState32

and _menhir_run35 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | LPAR ->
        _menhir_run12 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | UNIT ->
        _menhir_run11 _menhir_env (Obj.magic _menhir_stack) MenhirState35
    | VAR _v ->
        _menhir_run10 _menhir_env (Obj.magic _menhir_stack) MenhirState35 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState35

and _menhir_run39 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_env = _menhir_discard _menhir_env in
    let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv9) = Obj.magic _menhir_stack in
    let (_menhir_s : _menhir_state) = _menhir_s in
    ((let _1 = () in
    let _v : 'tv_simple_expr = 
# 84 "parser.mly"
                    ( Bool false )
# 3715 "parser.ml"
     in
    _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv10)

and _menhir_run40 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _menhir_stack = (_menhir_stack, _menhir_s) in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | VAR _v ->
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv5 * _menhir_state) = Obj.magic _menhir_stack in
        let (_v : (
# 8 "parser.mly"
       (Syntax.id)
# 3731 "parser.ml"
        )) = _v in
        ((let _menhir_env = _menhir_discard _menhir_env in
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv3 * _menhir_state) = Obj.magic _menhir_stack in
        let ((_2 : (
# 8 "parser.mly"
       (Syntax.id)
# 3739 "parser.ml"
        )) : (
# 8 "parser.mly"
       (Syntax.id)
# 3743 "parser.ml"
        )) = _v in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        let _1 = () in
        let _v : 'tv_simple_expr = 
# 86 "parser.mly"
                    ( Deref (Var _2) )
# 3750 "parser.ml"
         in
        _menhir_goto_simple_expr _menhir_env _menhir_stack _menhir_s _v) : 'freshtv4)) : 'freshtv6)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let (_menhir_env : _menhir_env) = _menhir_env in
        let (_menhir_stack : 'freshtv7 * _menhir_state) = Obj.magic _menhir_stack in
        ((let (_menhir_stack, _menhir_s) = _menhir_stack in
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s) : 'freshtv8)

and _menhir_discard : _menhir_env -> _menhir_env =
  fun _menhir_env ->
    let lexer = _menhir_env._menhir_lexer in
    let lexbuf = _menhir_env._menhir_lexbuf in
    let _tok = lexer lexbuf in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
    }

and prog : (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (
# 38 "parser.mly"
      (Syntax.exprML)
# 3776 "parser.ml"
) =
  fun lexer lexbuf ->
    let _menhir_env =
      let (lexer : Lexing.lexbuf -> token) = lexer in
      let (lexbuf : Lexing.lexbuf) = lexbuf in
      ((let _tok = Obj.magic () in
      {
        _menhir_lexer = lexer;
        _menhir_lexbuf = lexbuf;
        _menhir_token = _tok;
        _menhir_error = false;
      }) : _menhir_env)
    in
    Obj.magic (let (_menhir_env : _menhir_env) = _menhir_env in
    let (_menhir_stack : 'freshtv1) = ((), _menhir_env._menhir_lexbuf.Lexing.lex_curr_p) in
    ((let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | DEREF ->
        _menhir_run40 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FALSE ->
        _menhir_run39 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FIX ->
        _menhir_run35 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | FUN ->
        _menhir_run32 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | IF ->
        _menhir_run31 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | INT _v ->
        _menhir_run30 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | LET ->
        _menhir_run7 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | LPAR ->
        _menhir_run6 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | NOT ->
        _menhir_run5 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | REF ->
        _menhir_run4 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | TRUE ->
        _menhir_run3 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | UNIT ->
        _menhir_run2 _menhir_env (Obj.magic _menhir_stack) MenhirState0
    | VAR _v ->
        _menhir_run1 _menhir_env (Obj.magic _menhir_stack) MenhirState0 _v
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) MenhirState0) : 'freshtv2))

# 107 "parser.mly"
  

# 3829 "parser.ml"

# 219 "/usr/share/menhir/standard.mly"
  


# 3835 "parser.ml"
