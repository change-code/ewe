-module(ewe_connection).

-behaviour(supervisor_bridge).

-export([start_link/3]).

-export([init/1, terminate/2]).

-export([handle_connection/1, websocket_receiver/2]).

-include_lib("kernel/include/file.hrl").

-record(ssh_pty, {term, width, height}).


start_link(Listener, Ref, Socket) ->
    supervisor_bridge:start_link(?MODULE, [Listener, Ref, Socket]).


init([Listener, Ref, Socket]) ->
    Pid = spawn_link(?MODULE, handle_connection, [Socket]),
    Listener ! {handler, Ref, Pid},
    {ok, Pid, Socket}.


terminate(_Reason, _State) ->
    ok.


handle_connection(Socket) ->
    receive
        continue ->
            ok
    after 5000 ->
            throw(timeout)
    end,

    {ok, {http_request, Method, Path, Version}} = gen_tcp:recv(Socket, 0),
    Headers = recv_headers(Socket),
    ok = inet:setopts(Socket, [{packet, raw}]),
    handle_request(Socket, Method, Path, Version, Headers).


recv_headers(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, {http_header, _, Field, _, Value}} ->
            [{Field, Value}|recv_headers(Socket)];
        {ok, http_eoh} ->
            []
    end.


connection_upgrade(Headers) ->
    case proplists:lookup('Connection', Headers) of
        {'Connection', Connection} ->
            Tokens = [ string:strip(Token, both)
                       || Token <- string:tokens(Connection, ",")],

            case lists:member("Upgrade", Tokens) of
                true ->
                    proplists:lookup('Upgrade', Headers);
                false ->
                    none
            end;
        _ ->
            none
    end.


protocol_of(Headers) ->
    case connection_upgrade(Headers) of
        {'Upgrade', "websocket"} ->
            case proplists:get_value("Sec-Websocket-Version", Headers, 0) of
                "13" ->
                    case proplists:lookup("Sec-Websocket-Key", Headers) of
                        {"Sec-Websocket-Key", _} ->
                            websocket;
                        none ->
                            bad
                    end;
                _ ->
                    bad
            end;
        none ->
            http;
        _ ->
            bad
    end.


http_response(Socket, Code, ContentType, Body) ->
    ok =
        gen_tcp:send(
          Socket,
          [<<"HTTP/1.1 ">>, integer_to_list(Code), <<" ">>, httpd_util:reason_phrase(Code), <<"\r\n">>,
           <<"Connection: close\r\n">>,
           <<"Content-Type: ">>, ContentType, <<"\r\n">>,
           <<"Content-Length: ">>,
           <<"\r\n">>, integer_to_list(iolist_size(Body)),
           <<"\r\n\r\n">>, Body]),
    ok = gen_tcp:close(Socket).


handle_request(Socket, Method, Path, Version, Headers) ->
    case protocol_of(Headers) of
        bad ->
            handle_bad_request(Socket, Method, Path, Version, Headers);
        websocket ->
            handle_websocket_request(Socket, Method, Path, Version, Headers);
        http ->
            handle_http_request(Socket, Method, Path, Version, Headers)
    end.


handle_bad_request(Socket, _Method, _Path, _Version, _Headers) ->
    http_response(
      Socket,
      400,
      <<"text/html">>,
      <<"<h1>400 Bad Request</h1>\n">>).

handle_http_request(Socket, 'GET', {abs_path, "/"}, {1,1}, Headers) ->
    handle_http_request(Socket, 'GET', {abs_path, "/static/index.html"}, {1,1}, Headers);
handle_http_request(Socket, 'GET', {abs_path, "/static/" ++ Path}, {1,1}, _Headers) ->
    handle_file(Socket, [code:lib_dir(ewe, static), "/", Path]);
handle_http_request(Socket, 'GET', {abs_path, "/file/"}, {1,1}, _Headers) ->
    Dir = code:lib_dir(ewe, priv),
    {ok, Filenames} = file:list_dir(Dir),
    Filenames1 =
        lists:filter(
          fun (Name) ->
                  ".erl" =:= filename:extension(Name)
          end, Filenames),

    Filenames2 =
        [ io_lib:format("~p", [Name])
          || Name <- Filenames1],

    JSON = ["[", string:join(Filenames2, ", "), "]"],

    http_response(Socket, 200, <<"text/json">>, JSON);
handle_http_request(Socket, 'GET', {abs_path, "/file/" ++ Path}, {1,1}, _Headers) ->
    handle_file(Socket, [code:lib_dir(ewe, priv), "/", Path]);
handle_http_request(Socket, 'PUT', {abs_path, "/file/" ++ Path}, {1,1}, Headers) ->
    {'Content-Length', ContentLength} = proplists:lookup('Content-Length', Headers),
    case list_to_integer(ContentLength) of
        0 ->
            Bytes = <<>>;
        Length ->
            {ok, Bytes} = gen_tcp:recv(Socket, Length)
    end,
    Filename = [code:lib_dir(ewe, priv), "/", Path],
    case file:write_file(Filename, Bytes) of
        ok ->
            http_response(
              Socket,
              200,
              <<"text/html">>,
              <<"<h1>200 OK</h1>\n">>);
        _ ->
            http_response(
              Socket,
              500,
              <<"text/html">>,
              <<"<h1>500 Internal Server Error</h1>\n">>)
    end;
handle_http_request(Socket, "COMPILE", {abs_path, "/file/" ++ Path}, {1,1}, _Headers) ->
    handle_compile_result(Socket, compile:file(Path, [return]));
handle_http_request(Socket, "LOAD", {abs_path, "/file/" ++ Path}, {1,1}, _Headers) ->
    Module = list_to_atom(filename:basename(Path, ".erl")),
    code:purge(Module),
    case code:load_file(Module) of
        {module, Module} ->
            http_response(
              Socket,
              200,
              <<"text/html">>,
              <<"<h1>200 OK</h1>\n">>);
        {error, Reason} ->
            http_response(
              Socket,
              400,
              <<"text/plain">>,
              io_lib:format("~p~n", [Reason]))
    end.


handle_compile_result(Socket, {ok, _}) ->
    http_response(
      Socket,
      200,
      <<"text/plain">>,
      <<"">>);
handle_compile_result(Socket, {ok, _, Warnings}) ->
    http_response(
      Socket,
      200,
      <<"text/plain">>,
      format_compile_errors(<<"Warning">>, Warnings));
handle_compile_result(Socket, {error, Errors, Warnings}) ->
    http_response(
      Socket,
      400,
      <<"text/plain">>,
      [ format_compile_errors(<<"Error">>, Errors),
        format_compile_errors(<<"Warning">>, Warnings)]).


format_compile_error(Type, Name, Error) ->
    [ case L of
          none ->
              [Name, <<": ">>, Type, <<": ">>, M:format_error(E), <<"\n">>];
          _ ->
              [Name, <<":">>, integer_to_list(L), <<": ">>, Type, <<": ">>, M:format_error(E), <<"\n">>]
      end || {L,M,E} <- Error].

format_compile_errors(Type, Errors) ->
    [format_compile_error(Type, Name, Error)
     || {Name, Error} <- Errors].


handle_file(Socket, Filename) ->
    case filelib:is_file(Filename) of
        false ->
            http_response(
              Socket,
              404,
              <<"text/html">>,
              <<"<h1>404 Not Found</h1>\n">>);
        true ->
            {ok, #file_info{size=Size}} = file:read_file_info(Filename, []),
            Ext = filename:extension(Filename),
            MimeType =
                proplists:get_value(
                  Ext,
                  [{".html", <<"text/html;charset=utf-8">>},
                   {".js", <<"text/javascript;charset=utf-8">>},
                   {".css", <<"text/css;charset=utf-8">>},
                   {".erl", <<"text/plain">>}],
                  <<"application/octet-stream">>),

            ok = gen_tcp:send(
                   Socket,
                   [<<"HTTP/1.1 200 OK\r\n">>,
                    <<"Connection: close\r\n">>,
                    <<"Content-Type: ">>, MimeType, <<"\r\n">>,
                    <<"Content-Length: ">>, integer_to_binary(Size), <<"\r\n">>,
                    <<"\r\n">>]),
            {ok, _} = file:sendfile(Filename, Socket),
            ok = gen_tcp:close(Socket)
    end.


handle_websocket_request(Socket, _Method, _Path, _Version, Headers) ->
    Key = proplists:get_value("Sec-Websocket-Key", Headers),
    Accept = crypto:hash(sha, [Key, <<"258EAFA5-E914-47DA-95CA-C5AB0DC85B11">>]),
    ok = gen_tcp:send(
           Socket,
           [<<"HTTP/1.1 101 Switching Protocols\r\n">>,
            <<"Upgrade: websocket\r\n">>,
            <<"Connection: Upgrade\r\n">>,
            <<"Sec-WebSocket-Accept: ">>, base64:encode(Accept), <<"\r\n">>,
            <<"\r\n">>
           ]),

    _Pid = spawn_link(?MODULE, websocket_receiver, [self(), Socket]),
    Group = group:start(self(), {shell, start, []}),
    webshell_loop(Socket, Group, empty_buf(), #ssh_pty{term="", width=80, height=24}).


webshell_loop(Socket, Group, Buf, Pty) ->
    receive
        {websocket_msg, close, _} ->
            gen_tcp:close(Socket);
        {websocket_msg, text, Bin} ->
            Group ! {self(), {data, unicode:characters_to_list(Bin, utf8)}},
            webshell_loop(Socket, Group, Buf, Pty);
        {Group, get_unicode_state} ->
            Group ! {self(), get_unicode_state, false},
            webshell_loop(Socket, Group, Buf, Pty);
        {Group, {set_unicode_state, _}} ->
            Group ! {self(), set_unicode_state, false},
            webshell_loop(Socket, Group, Buf, Pty);
        {Group, tty_geometry} ->
            Group ! {self(), tty_geometry, {80,24}},
            webshell_loop(Socket, Group, Buf, Pty);
        {Group, Request} ->
            {Chars, NewBuf} = io_request(Request, Buf, Pty),
            websocket_send(Socket, Chars),
            webshell_loop(Socket, Group, NewBuf, Pty);
        Msg ->
            io:format("unknown msg: ~p~n", [Msg]),
            webshell_loop(Socket, Group, Buf, Pty)
    end.

encode_payload_len(Length)
  when Length < 126 ->
    <<Length:7>>;
encode_payload_len(Length)
    when Length < 16#10000 ->
    <<126:7, Length:16>>;
encode_payload_len(Length) ->
    <<127:7, Length:64>>.


websocket_send(Socket, Data) ->
    PAYLOAD_LEN = encode_payload_len(iolist_size(Data)),
    ok = gen_tcp:send(
           Socket,
           [<<1:1, 0:3, 1:4, 0:1, PAYLOAD_LEN/bits>>,
            Data]).


websocket_payload_len(Socket, PAYLOAD_LEN) ->
    case PAYLOAD_LEN of
        126 ->
            {ok, <<PAYLOAD_LEN2:16>>} = gen_tcp:recv(Socket, 2),
            PAYLOAD_LEN2;
        127 ->
            {ok, <<PAYLOAD_LEN2:64>>} = gen_tcp:recv(Socket, 8),
            PAYLOAD_LEN2;
        _ ->
            PAYLOAD_LEN
    end.


websocket_mask(Socket, MASK) ->
    <<Mask:32>> =
        case MASK of
            1 ->
                {ok, MaskBin} = gen_tcp:recv(Socket, 4),
                MaskBin;
            0 ->
                <<0,0,0,0>>
        end,
    Mask.


websocket_payload(Socket, Length, Mask) ->
    {ok, Payload} = gen_tcp:recv(Socket, Length),
    mask(Payload, Mask, <<>>).


websocket_opcode(0) ->
    continue;
websocket_opcode(1) ->
    text;
websocket_opcode(2) ->
    binary;
websocket_opcode(8) ->
    close;
websocket_opcode(9) ->
    ping;
websocket_opcode(10) ->
    pong.


websocket_receiver(Parent, Socket) ->
    {ok, <<FIN:1, 0:3, OPCODE:4, MASK:1, PAYLOAD_LEN:7>>} = gen_tcp:recv(Socket, 2),
    Opcode = websocket_opcode(OPCODE),
    Length = websocket_payload_len(Socket, PAYLOAD_LEN),
    Mask = websocket_mask(Socket, MASK),
    Payload = websocket_payload(Socket, Length, Mask),
    case FIN of
        0 ->
            websocket_receive_continuaton(Parent, Socket, Opcode, Payload);
        1 ->
            Parent ! {websocket_msg, Opcode, Payload},
            websocket_receiver(Parent, Socket)
    end.


websocket_receive_continuaton(Parent, Socket, Opcode, Acc) ->
    {ok, <<FIN:1, 0:3, 0:4, MASK:1, PAYLOAD_LEN:7>>} = gen_tcp:recv(Socket, 2),
    Length = websocket_payload_len(Socket, PAYLOAD_LEN),
    Mask = websocket_mask(Socket, MASK),
    Payload = websocket_payload(Socket, Length, Mask),
    case FIN of
        0 ->
            websocket_receive_continuaton(Parent, Socket, Opcode, <<Acc/binary, Payload/binary>>);
        1 ->
            Parent ! {websocket_msg, Opcode, <<Acc/binary, Payload/binary>>},
            websocket_receiver(Parent, Socket)
    end.


mask(<<Value:32, Rest/binary>>, Mask, Acc) ->
    Masked = Value bxor Mask,
    mask(Rest, Mask, <<Acc/binary, Masked:32>>);
mask(<<Value:24>>, Mask, Acc) ->
    <<Mask3:24, _:8>> = <<Mask:32>>,
    Masked = Value bxor Mask3,
    <<Acc/binary, Masked:24>>;
mask(<<Value:16>>, Mask, Acc) ->
    <<Mask2:16, _:16>> = <<Mask:32>>,
    Masked = Value bxor Mask2,
    <<Acc/binary, Masked:16>>;
mask(<<Value:8>>, Mask, Acc) ->
    <<Mask1:8, _:24>> = <<Mask:32>>,
    Masked = Value bxor Mask1,
    <<Acc/binary, Masked:8>>;
mask(<<>>, _, Acc) ->
    Acc.

%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2005-2013. All Rights Reserved.
%%
%% The contents of this file are subject to the Erlang Public License,
%% Version 1.1, (the "License"); you may not use this file except in
%% compliance with the License. You should have received a copy of the
%% Erlang Public License along with this software. If not, it can be
%% retrieved online at http://www.erlang.org/.
%%
%% Software distributed under the License is distributed on an "AS IS"
%% basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See
%% the License for the specific language governing rights and limitations
%% under the License.
%%
%% %CopyrightEnd%
%%

%%% io_request, handle io requests from the user process,
%%% Note, this is not the real I/O-protocol, but the mockup version
%%% used between edlin and a user_driver. The protocol tags are
%%% similar, but the message set is different.
%%% The protocol only exists internally between edlin and a character
%%% displaying device...
%%% We are *not* really unicode aware yet, we just filter away character
%%% beyond the latin1 range. We however handle the unicode binaries...
io_request({window_change, OldTty}, Buf, Tty) ->
    window_change(Tty, OldTty, Buf);
io_request({put_chars, Cs}, Buf, Tty) ->
    put_chars(binary:bin_to_list(Cs), Buf, Tty);
io_request({put_chars, unicode, Cs}, Buf, Tty) ->
    put_chars(unicode:characters_to_list(Cs,unicode), Buf, Tty);
io_request({insert_chars, Cs}, Buf, Tty) ->
    insert_chars(binary:bin_to_list(Cs), Buf, Tty);
io_request({insert_chars, unicode, Cs}, Buf, Tty) ->
    insert_chars(unicode:characters_to_list(Cs,unicode), Buf, Tty);
io_request({move_rel, N}, Buf, Tty) ->
    move_rel(N, Buf, Tty);
io_request({delete_chars,N}, Buf, Tty) ->
    delete_chars(N, Buf, Tty);
io_request(beep, Buf, _Tty) ->
    {[7], Buf};

%% New in R12
io_request({get_geometry,columns},Buf,Tty) ->
    {ok, Tty#ssh_pty.width, Buf};
io_request({get_geometry,rows},Buf,Tty) ->
    {ok, Tty#ssh_pty.height, Buf};
io_request({requests,Rs}, Buf, Tty) ->
    io_requests(Rs, Buf, Tty, []);
io_request(tty_geometry, Buf, Tty) ->
    io_requests([{move_rel, 0}, {put_chars, unicode, [10]}], Buf, Tty, []);
     %{[], Buf};
io_request(_R, Buf, _Tty) ->
    {[], Buf}.

io_requests([R|Rs], Buf, Tty, Acc) ->
    {Chars, NewBuf} = io_request(R, Buf, Tty),
    io_requests(Rs, NewBuf, Tty, [Acc|Chars]);
io_requests([], Buf, _Tty, Acc) ->
    {Acc, Buf}.


%%% return commands for cursor navigation, assume everything is ansi
%%% (vt100), add clauses for other terminal types if needed
ansi_tty(N, L) ->
    ["\e[", integer_to_list(N), L].

get_tty_command(up, N, _TerminalType) ->
    ansi_tty(N, $A);
get_tty_command(down, N, _TerminalType) ->
    ansi_tty(N, $B);
get_tty_command(right, N, _TerminalType) ->
    ansi_tty(N, $C);
get_tty_command(left, N, _TerminalType) ->
    ansi_tty(N, $D).


-define(PAD, 10).
-define(TABWIDTH, 8).

%% convert input characters to buffer and to writeout
%% Note that the buf is reversed but the buftail is not
%% (this is handy; the head is always next to the cursor)
conv_buf([], AccBuf, AccBufTail, AccWrite, Col) ->
    {AccBuf, AccBufTail, lists:reverse(AccWrite), Col};
conv_buf([13, 10 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl2(AccBufTail), [10, 13 | AccWrite], 0);
conv_buf([13 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl1(AccBufTail), [13 | AccWrite], 0);
conv_buf([10 | Rest], _AccBuf, AccBufTail, AccWrite, _Col) ->
    conv_buf(Rest, [], tl1(AccBufTail), [10, 13 | AccWrite], 0);
conv_buf([C | Rest], AccBuf, AccBufTail, AccWrite, Col) ->
    conv_buf(Rest, [C | AccBuf], tl1(AccBufTail), [C | AccWrite], Col + 1).


%%% put characters at current position (possibly overwriting
%%% characters after current position in buffer)
put_chars(Chars, {Buf, BufTail, Col}, _Tty) ->
    {NewBuf, NewBufTail, WriteBuf, NewCol} =
	conv_buf(Chars, Buf, BufTail, [], Col),
    {WriteBuf, {NewBuf, NewBufTail, NewCol}}.

%%% insert character at current position
insert_chars([], {Buf, BufTail, Col}, _Tty) ->
    {[], {Buf, BufTail, Col}};
insert_chars(Chars, {Buf, BufTail, Col}, Tty) ->
    {NewBuf, _NewBufTail, WriteBuf, NewCol} =
	conv_buf(Chars, Buf, [], [], Col),
    M = move_cursor(NewCol + length(BufTail), NewCol, Tty),
    {[WriteBuf, BufTail | M], {NewBuf, BufTail, NewCol}}.

%%% delete characters at current position, (backwards if negative argument)
delete_chars(0, {Buf, BufTail, Col}, _Tty) ->
    {[], {Buf, BufTail, Col}};
delete_chars(N, {Buf, BufTail, Col}, Tty) when N > 0 ->
    NewBufTail = nthtail(N, BufTail),
    M = move_cursor(Col + length(NewBufTail) + N, Col, Tty),
    {[NewBufTail, lists:duplicate(N, $ ) | M],
     {Buf, NewBufTail, Col}};
delete_chars(N, {Buf, BufTail, Col}, Tty) -> % N < 0
    NewBuf = nthtail(-N, Buf),
    NewCol = case Col + N of V when V >= 0 -> V; _ -> 0 end,
    M1 = move_cursor(Col, NewCol, Tty),
    M2 = move_cursor(NewCol + length(BufTail) - N, NewCol, Tty),
    {[M1, BufTail, lists:duplicate(-N, $ ) | M2],
     {NewBuf, BufTail, NewCol}}.

%%% Window change, redraw the current line (and clear out after it
%%% if current window is wider than previous)
window_change(Tty, OldTty, Buf)
  when OldTty#ssh_pty.width == Tty#ssh_pty.width ->
    {[], Buf};
window_change(Tty, OldTty, {Buf, BufTail, Col}) ->
    M1 = move_cursor(Col, 0, OldTty),
    N = erlang:max(Tty#ssh_pty.width - OldTty#ssh_pty.width, 0) * 2,
    S = lists:reverse(Buf, [BufTail | lists:duplicate(N, $ )]),
    M2 = move_cursor(length(Buf) + length(BufTail) + N, Col, Tty),
    {[M1, S | M2], {Buf, BufTail, Col}}.

%% move around in buffer, respecting pad characters
step_over(0, Buf, [?PAD | BufTail], Col) ->
    {[?PAD | Buf], BufTail, Col+1};
step_over(0, Buf, BufTail, Col) ->
    {Buf, BufTail, Col};
step_over(N, [C | Buf], BufTail, Col) when N < 0 ->
    N1 = ifelse(C == ?PAD, N, N+1),
    step_over(N1, Buf, [C | BufTail], Col-1);
step_over(N, Buf, [C | BufTail], Col) when N > 0 ->
    N1 = ifelse(C == ?PAD, N, N-1),
    step_over(N1, [C | Buf], BufTail, Col+1).

%%% an empty line buffer
empty_buf() -> {[], [], 0}.

%%% col and row from position with given width
col(N, W) -> N rem W.
row(N, W) -> N div W.

%%% move relative N characters
move_rel(N, {Buf, BufTail, Col}, Tty) ->
    {NewBuf, NewBufTail, NewCol} = step_over(N, Buf, BufTail, Col),
    M = move_cursor(Col, NewCol, Tty),
    {M, {NewBuf, NewBufTail, NewCol}}.

%%% give move command for tty
move_cursor(A, A, _Tty) ->
    [];
move_cursor(From, To, #ssh_pty{width=Width, term=Type}) ->
    Tcol = case col(To, Width) - col(From, Width) of
	       0 -> "";
	       I when I < 0 -> get_tty_command(left, -I, Type);
	       I -> get_tty_command(right, I, Type)
	end,
    Trow = case row(To, Width) - row(From, Width) of
	       0 -> "";
	       J when J < 0 -> get_tty_command(up, -J, Type);
	       J -> get_tty_command(down, J, Type)
	   end,
    [Tcol | Trow].


%%% tail, works with empty lists
tl1([_|A]) -> A;
tl1(_) -> [].

%%% second tail
tl2([_,_|A]) -> A;
tl2(_) -> [].

%%% nthtail as in lists, but no badarg if n > the length of list
nthtail(0, A) -> A;
nthtail(N, [_ | A]) when N > 0 -> nthtail(N-1, A);
nthtail(_, _) -> [].


ifelse(Cond, A, B) ->
    case Cond of
	true -> A;
	_ -> B
    end.
