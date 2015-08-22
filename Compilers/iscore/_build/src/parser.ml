exception Error

type _menhir_env = {
  _menhir_lexer: Lexing.lexbuf -> Lexer.token;
  _menhir_lexbuf: Lexing.lexbuf;
  _menhir_token: Lexer.token;
  mutable _menhir_error: bool
}

and _menhir_state = 
  | MenhirState34
  | MenhirState6

  
  open Lexer
  open Types
  let relations = ref []
  let hash_events = Hashtbl.create  100
  let counter_condition = ref 0

  let getInterval n s e min max =
    if min = max then
      Rigid (n, s, e, string2time min)
    else begin
      if max = "0u" then
        Flexible (n, s, e, string2time min)
      else
        Semi (n, s, e, string2time min, string2time max)
    end
  let string2bool s =
    if s = "1" then true
    else false

  let setCondition condition event trigger default id_c =
    let time_e = getTimeEvent (Hashtbl.find hash_events event) in
    Hashtbl.replace hash_events event (ConditionalEvent (event, time_e, Some (condition, trigger, string2bool default, id_c)))
let _eRR =
  Error

let rec _menhir_goto_interval : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let i = _v in
    let (_menhir_stack, _menhir_s, b) = _menhir_stack in
    let _v : (Types.scenario_t) =                           ( b ) in
    _menhir_goto_body _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_event : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let e = _v in
    let (_menhir_stack, _menhir_s, b) = _menhir_stack in
    let _v : (Types.scenario_t) =                           ( b ) in
    _menhir_goto_body _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_automation : _menhir_env -> 'ttv_tail -> (unit) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let a = _v in
    let (_menhir_stack, _menhir_s, b) = _menhir_stack in
    let _v : (Types.scenario_t) =                           ( b ) in
    _menhir_goto_body _menhir_env _menhir_stack _menhir_s _v

and _menhir_goto_scenario : _menhir_env -> 'ttv_tail -> (Types.temporal_object_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _v ->
    let _menhir_stack = Obj.magic _menhir_stack in
    let _menhir_stack = Obj.magic _menhir_stack in
    let s = _v in
    let (_menhir_stack, _menhir_s, b) = _menhir_stack in
    let _v : (Types.scenario_t) =                           ( s::b ) in
    _menhir_goto_body _menhir_env _menhir_stack _menhir_s _v

and _menhir_run9 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lexer.NAME ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Lexer.EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Lexer.STRING _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Lexer.DATE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Lexer.EQ ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | Lexer.STRING _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | Lexer.SLASH ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | Lexer.GREATER ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let ((_menhir_stack, n), d) = _menhir_stack in
                                    let _v : (unit) =                                                                     ( Hashtbl.add hash_events n (TimeEvent (n, string2time d)) ) in
                                    _menhir_goto_event _menhir_env _menhir_stack _v
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    raise _eRR)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                raise _eRR)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_errorcase : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    match _menhir_s with
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run22 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lexer.NAME ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Lexer.EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Lexer.STRING _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Lexer.START ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Lexer.EQ ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | Lexer.STRING _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | Lexer.END ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | Lexer.EQ ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _tok = _menhir_env._menhir_token in
                                    (match _tok with
                                    | Lexer.STRING _v ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_stack = (_menhir_stack, _v) in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _tok = _menhir_env._menhir_token in
                                        (match _tok with
                                        | Lexer.GREATER ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState34
                                        | Lexer.SLASH ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | Lexer.GREATER ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let (((_menhir_stack, n), s), e) = _menhir_stack in
                                                let _v : (Types.temporal_object_t) =                                                                                     ( Texture (n, s, e)) in
                                                _menhir_goto_scenario _menhir_env _menhir_stack _v
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                raise _eRR)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            raise _eRR)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        raise _eRR)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    raise _eRR)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                raise _eRR)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run40 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lexer.NAME ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Lexer.EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Lexer.STRING _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Lexer.START ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Lexer.EQ ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | Lexer.STRING _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | Lexer.END ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | Lexer.EQ ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _tok = _menhir_env._menhir_token in
                                    (match _tok with
                                    | Lexer.STRING _v ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_stack = (_menhir_stack, _v) in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _tok = _menhir_env._menhir_token in
                                        (match _tok with
                                        | Lexer.DMIN ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | Lexer.EQ ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _tok = _menhir_env._menhir_token in
                                                (match _tok with
                                                | Lexer.STRING _v ->
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    let _menhir_stack = (_menhir_stack, _v) in
                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                    let _tok = _menhir_env._menhir_token in
                                                    (match _tok with
                                                    | Lexer.DMAX ->
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                        let _tok = _menhir_env._menhir_token in
                                                        (match _tok with
                                                        | Lexer.EQ ->
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            let _menhir_env = _menhir_discard _menhir_env in
                                                            let _tok = _menhir_env._menhir_token in
                                                            (match _tok with
                                                            | Lexer.STRING _v ->
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                let _menhir_stack = (_menhir_stack, _v) in
                                                                let _menhir_env = _menhir_discard _menhir_env in
                                                                let _tok = _menhir_env._menhir_token in
                                                                (match _tok with
                                                                | Lexer.SLASH ->
                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                                    let _tok = _menhir_env._menhir_token in
                                                                    (match _tok with
                                                                    | Lexer.GREATER ->
                                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                                        let (((((_menhir_stack, n), s), e), dmin), dmax) = _menhir_stack in
                                                                        let _v : (unit) =                                                                                                                            ( relations := (getInterval n s e dmin dmax)::!relations) in
                                                                        _menhir_goto_interval _menhir_env _menhir_stack _v
                                                                    | _ ->
                                                                        assert (not _menhir_env._menhir_error);
                                                                        _menhir_env._menhir_error <- true;
                                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                                        raise _eRR)
                                                                | _ ->
                                                                    assert (not _menhir_env._menhir_error);
                                                                    _menhir_env._menhir_error <- true;
                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                    raise _eRR)
                                                            | _ ->
                                                                assert (not _menhir_env._menhir_error);
                                                                _menhir_env._menhir_error <- true;
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                raise _eRR)
                                                        | _ ->
                                                            assert (not _menhir_env._menhir_error);
                                                            _menhir_env._menhir_error <- true;
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            raise _eRR)
                                                    | _ ->
                                                        assert (not _menhir_env._menhir_error);
                                                        _menhir_env._menhir_error <- true;
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        raise _eRR)
                                                | _ ->
                                                    assert (not _menhir_env._menhir_error);
                                                    _menhir_env._menhir_error <- true;
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    raise _eRR)
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                raise _eRR)
                                        | Lexer.SLASH ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | Lexer.GREATER ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let (((_menhir_stack, n), s), e) = _menhir_stack in
                                                let _v : (unit) =                                                                                    ( relations := Flexible (n,s,e,0)::!relations) in
                                                _menhir_goto_interval _menhir_env _menhir_stack _v
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                raise _eRR)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            raise _eRR)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        raise _eRR)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    raise _eRR)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                raise _eRR)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run60 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lexer.NAME ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Lexer.EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Lexer.STRING _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Lexer.DATE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Lexer.EQ ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | Lexer.STRING _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | Lexer.CONDITION ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | Lexer.EQ ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _tok = _menhir_env._menhir_token in
                                    (match _tok with
                                    | Lexer.STRING _v ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_stack = (_menhir_stack, _v) in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _tok = _menhir_env._menhir_token in
                                        (match _tok with
                                        | Lexer.SLASH ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | Lexer.GREATER ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let (((_menhir_stack, n), d), _) = _menhir_stack in
                                                let _v : (unit) =                                                                                    ( Hashtbl.add hash_events n (ConditionalEvent (n, string2time d, None) )) in
                                                _menhir_goto_event _menhir_env _menhir_stack _v
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                raise _eRR)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            raise _eRR)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        raise _eRR)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    raise _eRR)
                            | Lexer.GREATER ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | Lexer.LESS ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _tok = _menhir_env._menhir_token in
                                    (match _tok with
                                    | Lexer.COMMAND ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _tok = _menhir_env._menhir_token in
                                        (match _tok with
                                        | Lexer.ADDRESS ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | Lexer.EQ ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _tok = _menhir_env._menhir_token in
                                                (match _tok with
                                                | Lexer.STRING _v ->
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    let _menhir_stack = (_menhir_stack, _v) in
                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                    let _tok = _menhir_env._menhir_token in
                                                    (match _tok with
                                                    | Lexer.GREATER ->
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                        let _tok = _menhir_env._menhir_token in
                                                        (match _tok with
                                                        | Lexer.INT _v ->
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            let _menhir_stack = (_menhir_stack, _v) in
                                                            let _menhir_env = _menhir_discard _menhir_env in
                                                            let _tok = _menhir_env._menhir_token in
                                                            (match _tok with
                                                            | Lexer.LESS ->
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                let _menhir_env = _menhir_discard _menhir_env in
                                                                let _tok = _menhir_env._menhir_token in
                                                                (match _tok with
                                                                | Lexer.SLASH ->
                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                                    let _tok = _menhir_env._menhir_token in
                                                                    (match _tok with
                                                                    | Lexer.COMMAND ->
                                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                                        let _tok = _menhir_env._menhir_token in
                                                                        (match _tok with
                                                                        | Lexer.GREATER ->
                                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                                            let _menhir_env = _menhir_discard _menhir_env in
                                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                                            let ((_menhir_stack, a), v) = _menhir_stack in
                                                                            let _v : (unit) =                                                                              ( ) in
                                                                            let _menhir_stack = (_menhir_stack, _v) in
                                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                                            assert (not _menhir_env._menhir_error);
                                                                            let _tok = _menhir_env._menhir_token in
                                                                            (match _tok with
                                                                            | Lexer.LESS ->
                                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                                let _menhir_env = _menhir_discard _menhir_env in
                                                                                let _tok = _menhir_env._menhir_token in
                                                                                (match _tok with
                                                                                | Lexer.SLASH ->
                                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                                                    let _tok = _menhir_env._menhir_token in
                                                                                    (match _tok with
                                                                                    | Lexer.EVENT ->
                                                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                                                        let _tok = _menhir_env._menhir_token in
                                                                                        (match _tok with
                                                                                        | Lexer.GREATER ->
                                                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                                                            let _menhir_env = _menhir_discard _menhir_env in
                                                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                                                            let (((_menhir_stack, n), d), c) = _menhir_stack in
                                                                                            let _v : (unit) =                                                                                             ( Hashtbl.add hash_events n (CommandEvent (n, string2time d))) in
                                                                                            _menhir_goto_event _menhir_env _menhir_stack _v
                                                                                        | _ ->
                                                                                            assert (not _menhir_env._menhir_error);
                                                                                            _menhir_env._menhir_error <- true;
                                                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                                                            raise _eRR)
                                                                                    | _ ->
                                                                                        assert (not _menhir_env._menhir_error);
                                                                                        _menhir_env._menhir_error <- true;
                                                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                                                        raise _eRR)
                                                                                | _ ->
                                                                                    assert (not _menhir_env._menhir_error);
                                                                                    _menhir_env._menhir_error <- true;
                                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                                    raise _eRR)
                                                                            | _ ->
                                                                                assert (not _menhir_env._menhir_error);
                                                                                _menhir_env._menhir_error <- true;
                                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                                raise _eRR)
                                                                        | _ ->
                                                                            assert (not _menhir_env._menhir_error);
                                                                            _menhir_env._menhir_error <- true;
                                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                                            raise _eRR)
                                                                    | _ ->
                                                                        assert (not _menhir_env._menhir_error);
                                                                        _menhir_env._menhir_error <- true;
                                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                                        raise _eRR)
                                                                | _ ->
                                                                    assert (not _menhir_env._menhir_error);
                                                                    _menhir_env._menhir_error <- true;
                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                    raise _eRR)
                                                            | _ ->
                                                                assert (not _menhir_env._menhir_error);
                                                                _menhir_env._menhir_error <- true;
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                raise _eRR)
                                                        | _ ->
                                                            assert (not _menhir_env._menhir_error);
                                                            _menhir_env._menhir_error <- true;
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            raise _eRR)
                                                    | _ ->
                                                        assert (not _menhir_env._menhir_error);
                                                        _menhir_env._menhir_error <- true;
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        raise _eRR)
                                                | _ ->
                                                    assert (not _menhir_env._menhir_error);
                                                    _menhir_env._menhir_error <- true;
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    raise _eRR)
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                raise _eRR)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            raise _eRR)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        raise _eRR)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    raise _eRR)
                            | Lexer.SLASH ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | Lexer.GREATER ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let ((_menhir_stack, n), d) = _menhir_stack in
                                    let _v : (unit) =                                                                ( Hashtbl.add hash_events n (TimeEvent (n, string2time d)) ) in
                                    _menhir_goto_event _menhir_env _menhir_stack _v
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    raise _eRR)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                raise _eRR)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run91 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lexer.NAME ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Lexer.EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Lexer.STRING _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Lexer.DATE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Lexer.EQ ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | Lexer.STRING _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | Lexer.SLASH ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | Lexer.GREATER ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let ((_menhir_stack, n), d) = _menhir_stack in
                                    let _v : (unit) =                                                                   ( Hashtbl.add hash_events n (TimeEvent (n, string2time d)) ) in
                                    _menhir_goto_event _menhir_env _menhir_stack _v
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    raise _eRR)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                raise _eRR)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run100 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lexer.NAME ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Lexer.EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Lexer.STRING _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Lexer.DISPOSE ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Lexer.EQ ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | Lexer.STRING _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | Lexer.GREATER ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | Lexer.LESS ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _tok = _menhir_env._menhir_token in
                                    (match _tok with
                                    | Lexer.CASE ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _tok = _menhir_env._menhir_token in
                                        (match _tok with
                                        | Lexer.EVENT ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | Lexer.EQ ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _tok = _menhir_env._menhir_token in
                                                (match _tok with
                                                | Lexer.STRING _v ->
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    let _menhir_stack = (_menhir_stack, _v) in
                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                    let _tok = _menhir_env._menhir_token in
                                                    (match _tok with
                                                    | Lexer.TRIGGER ->
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                        let _tok = _menhir_env._menhir_token in
                                                        (match _tok with
                                                        | Lexer.EQ ->
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            let _menhir_env = _menhir_discard _menhir_env in
                                                            let _tok = _menhir_env._menhir_token in
                                                            (match _tok with
                                                            | Lexer.STRING _v ->
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                let _menhir_stack = (_menhir_stack, _v) in
                                                                let _menhir_env = _menhir_discard _menhir_env in
                                                                let _tok = _menhir_env._menhir_token in
                                                                (match _tok with
                                                                | Lexer.DEFAULT ->
                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                                    let _tok = _menhir_env._menhir_token in
                                                                    (match _tok with
                                                                    | Lexer.EQ ->
                                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                                        let _tok = _menhir_env._menhir_token in
                                                                        (match _tok with
                                                                        | Lexer.STRING _v ->
                                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                                            let _menhir_stack = (_menhir_stack, _v) in
                                                                            let _menhir_env = _menhir_discard _menhir_env in
                                                                            let _tok = _menhir_env._menhir_token in
                                                                            (match _tok with
                                                                            | Lexer.SLASH ->
                                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                                let _menhir_env = _menhir_discard _menhir_env in
                                                                                let _tok = _menhir_env._menhir_token in
                                                                                (match _tok with
                                                                                | Lexer.GREATER ->
                                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                                    let (((_menhir_stack, e), t), d) = _menhir_stack in
                                                                                    let _v : (Types.id_t * string * string) =                                                                                     ( (e,t,d) ) in
                                                                                    let _menhir_stack = (_menhir_stack, _v) in
                                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                                    assert (not _menhir_env._menhir_error);
                                                                                    let _tok = _menhir_env._menhir_token in
                                                                                    (match _tok with
                                                                                    | Lexer.LESS ->
                                                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                                                        let _tok = _menhir_env._menhir_token in
                                                                                        (match _tok with
                                                                                        | Lexer.SLASH ->
                                                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                                                            let _menhir_env = _menhir_discard _menhir_env in
                                                                                            let _tok = _menhir_env._menhir_token in
                                                                                            (match _tok with
                                                                                            | Lexer.CONDITION ->
                                                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                                                let _menhir_env = _menhir_discard _menhir_env in
                                                                                                let _tok = _menhir_env._menhir_token in
                                                                                                (match _tok with
                                                                                                | Lexer.GREATER ->
                                                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                                                    let (((_menhir_stack, n), _), c) = _menhir_stack in
                                                                                                    let _v : (unit) =                                                                                                 ( let (e,t,d) = c in setCondition n e t d !counter_condition; counter_condition := !counter_condition + 1) in
                                                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                                                    let c = _v in
                                                                                                    let (_menhir_stack, _menhir_s, b) = _menhir_stack in
                                                                                                    let _v : (Types.scenario_t) =                           ( b ) in
                                                                                                    _menhir_goto_body _menhir_env _menhir_stack _menhir_s _v
                                                                                                | _ ->
                                                                                                    assert (not _menhir_env._menhir_error);
                                                                                                    _menhir_env._menhir_error <- true;
                                                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                                                    raise _eRR)
                                                                                            | _ ->
                                                                                                assert (not _menhir_env._menhir_error);
                                                                                                _menhir_env._menhir_error <- true;
                                                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                                                raise _eRR)
                                                                                        | _ ->
                                                                                            assert (not _menhir_env._menhir_error);
                                                                                            _menhir_env._menhir_error <- true;
                                                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                                                            raise _eRR)
                                                                                    | _ ->
                                                                                        assert (not _menhir_env._menhir_error);
                                                                                        _menhir_env._menhir_error <- true;
                                                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                                                        raise _eRR)
                                                                                | _ ->
                                                                                    assert (not _menhir_env._menhir_error);
                                                                                    _menhir_env._menhir_error <- true;
                                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                                    raise _eRR)
                                                                            | _ ->
                                                                                assert (not _menhir_env._menhir_error);
                                                                                _menhir_env._menhir_error <- true;
                                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                                raise _eRR)
                                                                        | _ ->
                                                                            assert (not _menhir_env._menhir_error);
                                                                            _menhir_env._menhir_error <- true;
                                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                                            raise _eRR)
                                                                    | _ ->
                                                                        assert (not _menhir_env._menhir_error);
                                                                        _menhir_env._menhir_error <- true;
                                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                                        raise _eRR)
                                                                | _ ->
                                                                    assert (not _menhir_env._menhir_error);
                                                                    _menhir_env._menhir_error <- true;
                                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                                    raise _eRR)
                                                            | _ ->
                                                                assert (not _menhir_env._menhir_error);
                                                                _menhir_env._menhir_error <- true;
                                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                                raise _eRR)
                                                        | _ ->
                                                            assert (not _menhir_env._menhir_error);
                                                            _menhir_env._menhir_error <- true;
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            raise _eRR)
                                                    | _ ->
                                                        assert (not _menhir_env._menhir_error);
                                                        _menhir_env._menhir_error <- true;
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        raise _eRR)
                                                | _ ->
                                                    assert (not _menhir_env._menhir_error);
                                                    _menhir_env._menhir_error <- true;
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    raise _eRR)
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                raise _eRR)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            raise _eRR)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        raise _eRR)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    raise _eRR)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                raise _eRR)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_run126 : _menhir_env -> 'ttv_tail -> 'ttv_return =
  fun _menhir_env _menhir_stack ->
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lexer.NAME ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Lexer.EQ ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Lexer.STRING _v ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_stack = (_menhir_stack, _v) in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Lexer.START ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Lexer.EQ ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | Lexer.STRING _v ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = (_menhir_stack, _v) in
                            let _menhir_env = _menhir_discard _menhir_env in
                            let _tok = _menhir_env._menhir_token in
                            (match _tok with
                            | Lexer.END ->
                                let _menhir_stack = Obj.magic _menhir_stack in
                                let _menhir_env = _menhir_discard _menhir_env in
                                let _tok = _menhir_env._menhir_token in
                                (match _tok with
                                | Lexer.EQ ->
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    let _menhir_env = _menhir_discard _menhir_env in
                                    let _tok = _menhir_env._menhir_token in
                                    (match _tok with
                                    | Lexer.STRING _v ->
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        let _menhir_stack = (_menhir_stack, _v) in
                                        let _menhir_env = _menhir_discard _menhir_env in
                                        let _tok = _menhir_env._menhir_token in
                                        (match _tok with
                                        | Lexer.GREATER ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _v : (unit) =   () in
                                            let _menhir_stack = (_menhir_stack, _v) in
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            assert (not _menhir_env._menhir_error);
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | Lexer.LESS ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _tok = _menhir_env._menhir_token in
                                                (match _tok with
                                                | Lexer.SLASH ->
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    let _menhir_env = _menhir_discard _menhir_env in
                                                    let _tok = _menhir_env._menhir_token in
                                                    (match _tok with
                                                    | Lexer.AUTOMATION ->
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        let _menhir_env = _menhir_discard _menhir_env in
                                                        let _tok = _menhir_env._menhir_token in
                                                        (match _tok with
                                                        | Lexer.GREATER ->
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            let _menhir_env = _menhir_discard _menhir_env in
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            let ((((_menhir_stack, n), s), e), c) = _menhir_stack in
                                                            let _v : (unit) =                                                                                                                       ( ) in
                                                            _menhir_goto_automation _menhir_env _menhir_stack _v
                                                        | _ ->
                                                            assert (not _menhir_env._menhir_error);
                                                            _menhir_env._menhir_error <- true;
                                                            let _menhir_stack = Obj.magic _menhir_stack in
                                                            raise _eRR)
                                                    | _ ->
                                                        assert (not _menhir_env._menhir_error);
                                                        _menhir_env._menhir_error <- true;
                                                        let _menhir_stack = Obj.magic _menhir_stack in
                                                        raise _eRR)
                                                | _ ->
                                                    assert (not _menhir_env._menhir_error);
                                                    _menhir_env._menhir_error <- true;
                                                    let _menhir_stack = Obj.magic _menhir_stack in
                                                    raise _eRR)
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                raise _eRR)
                                        | Lexer.SLASH ->
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            let _menhir_env = _menhir_discard _menhir_env in
                                            let _tok = _menhir_env._menhir_token in
                                            (match _tok with
                                            | Lexer.GREATER ->
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let _menhir_env = _menhir_discard _menhir_env in
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                let (((_menhir_stack, n), s), e) = _menhir_stack in
                                                let _v : (unit) =                                                                                      ( ) in
                                                _menhir_goto_automation _menhir_env _menhir_stack _v
                                            | _ ->
                                                assert (not _menhir_env._menhir_error);
                                                _menhir_env._menhir_error <- true;
                                                let _menhir_stack = Obj.magic _menhir_stack in
                                                raise _eRR)
                                        | _ ->
                                            assert (not _menhir_env._menhir_error);
                                            _menhir_env._menhir_error <- true;
                                            let _menhir_stack = Obj.magic _menhir_stack in
                                            raise _eRR)
                                    | _ ->
                                        assert (not _menhir_env._menhir_error);
                                        _menhir_env._menhir_error <- true;
                                        let _menhir_stack = Obj.magic _menhir_stack in
                                        raise _eRR)
                                | _ ->
                                    assert (not _menhir_env._menhir_error);
                                    _menhir_env._menhir_error <- true;
                                    let _menhir_stack = Obj.magic _menhir_stack in
                                    raise _eRR)
                            | _ ->
                                assert (not _menhir_env._menhir_error);
                                _menhir_env._menhir_error <- true;
                                let _menhir_stack = Obj.magic _menhir_stack in
                                raise _eRR)
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR

and _menhir_goto_body : _menhir_env -> 'ttv_tail -> _menhir_state -> (Types.scenario_t) -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s _v ->
    let _menhir_stack = (_menhir_stack, _menhir_s, _v) in
    match _menhir_s with
    | MenhirState6 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Lexer.LESS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Lexer.AUTOMATION ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.CONDITION ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.ENDEVENT ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.EVENT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.INTERVAL ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.SCENARIO ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.SLASH ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Lexer.SCENARIO ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Lexer.GREATER ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | Lexer.EOF ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let ((_menhir_stack, n), _, s) = _menhir_stack in
                            let _v : (Types.program) =                                                                                 ( (hash_events, !relations, Structure ("Main", "start", "end", s)) ) in
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _1 = _v in
                            Obj.magic _1
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | Lexer.STARTEVENT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
    | MenhirState34 ->
        let _menhir_stack = Obj.magic _menhir_stack in
        assert (not _menhir_env._menhir_error);
        let _tok = _menhir_env._menhir_token in
        match _tok with
        | Lexer.LESS ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Lexer.AUTOMATION ->
                _menhir_run126 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.CONDITION ->
                _menhir_run100 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.ENDEVENT ->
                _menhir_run91 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.EVENT ->
                _menhir_run60 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.INTERVAL ->
                _menhir_run40 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.SCENARIO ->
                _menhir_run22 _menhir_env (Obj.magic _menhir_stack)
            | Lexer.SLASH ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Lexer.SCENARIO ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Lexer.GREATER ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let ((((_menhir_stack, n), s), e), _, b) = _menhir_stack in
                        let _v : (Types.temporal_object_t) =                                                                                                                 ( Structure (n,s,e,b)) in
                        _menhir_goto_scenario _menhir_env _menhir_stack _v
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                        _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                    _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
            | Lexer.STARTEVENT ->
                _menhir_run9 _menhir_env (Obj.magic _menhir_stack)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                let (_menhir_stack, _menhir_s, _) = _menhir_stack in
                _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            let (_menhir_stack, _menhir_s, _) = _menhir_stack in
            _menhir_errorcase _menhir_env (Obj.magic _menhir_stack) _menhir_s

and _menhir_reduce3 : _menhir_env -> 'ttv_tail -> _menhir_state -> 'ttv_return =
  fun _menhir_env _menhir_stack _menhir_s ->
    let _v : (Types.scenario_t) =     ( [] ) in
    _menhir_goto_body _menhir_env _menhir_stack _menhir_s _v

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

and main : (Lexing.lexbuf -> Lexer.token) -> Lexing.lexbuf -> (Types.program) =
  fun lexer lexbuf ->
    let _menhir_env = let _tok = Obj.magic () in
    {
      _menhir_lexer = lexer;
      _menhir_lexbuf = lexbuf;
      _menhir_token = _tok;
      _menhir_error = false;
      } in
    Obj.magic (let _menhir_stack = () in
    let _menhir_env = _menhir_discard _menhir_env in
    let _tok = _menhir_env._menhir_token in
    match _tok with
    | Lexer.LESS ->
        let _menhir_stack = Obj.magic _menhir_stack in
        let _menhir_env = _menhir_discard _menhir_env in
        let _tok = _menhir_env._menhir_token in
        (match _tok with
        | Lexer.SCENARIO ->
            let _menhir_stack = Obj.magic _menhir_stack in
            let _menhir_env = _menhir_discard _menhir_env in
            let _tok = _menhir_env._menhir_token in
            (match _tok with
            | Lexer.NAME ->
                let _menhir_stack = Obj.magic _menhir_stack in
                let _menhir_env = _menhir_discard _menhir_env in
                let _tok = _menhir_env._menhir_token in
                (match _tok with
                | Lexer.EQ ->
                    let _menhir_stack = Obj.magic _menhir_stack in
                    let _menhir_env = _menhir_discard _menhir_env in
                    let _tok = _menhir_env._menhir_token in
                    (match _tok with
                    | Lexer.STRING _v ->
                        let _menhir_stack = Obj.magic _menhir_stack in
                        let _menhir_stack = (_menhir_stack, _v) in
                        let _menhir_env = _menhir_discard _menhir_env in
                        let _tok = _menhir_env._menhir_token in
                        (match _tok with
                        | Lexer.GREATER ->
                            let _menhir_stack = Obj.magic _menhir_stack in
                            let _menhir_env = _menhir_discard _menhir_env in
                            _menhir_reduce3 _menhir_env (Obj.magic _menhir_stack) MenhirState6
                        | _ ->
                            assert (not _menhir_env._menhir_error);
                            _menhir_env._menhir_error <- true;
                            let _menhir_stack = Obj.magic _menhir_stack in
                            raise _eRR)
                    | _ ->
                        assert (not _menhir_env._menhir_error);
                        _menhir_env._menhir_error <- true;
                        let _menhir_stack = Obj.magic _menhir_stack in
                        raise _eRR)
                | _ ->
                    assert (not _menhir_env._menhir_error);
                    _menhir_env._menhir_error <- true;
                    let _menhir_stack = Obj.magic _menhir_stack in
                    raise _eRR)
            | _ ->
                assert (not _menhir_env._menhir_error);
                _menhir_env._menhir_error <- true;
                let _menhir_stack = Obj.magic _menhir_stack in
                raise _eRR)
        | _ ->
            assert (not _menhir_env._menhir_error);
            _menhir_env._menhir_error <- true;
            let _menhir_stack = Obj.magic _menhir_stack in
            raise _eRR)
    | _ ->
        assert (not _menhir_env._menhir_error);
        _menhir_env._menhir_error <- true;
        let _menhir_stack = Obj.magic _menhir_stack in
        raise _eRR)



