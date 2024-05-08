-module(server).
-export([start/1, stop/1]).

start(Port) -> spawn(fun() -> server(Port) end).
stop(Server) -> Server ! stop.

server(Port) ->
    login:start(),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
    Room = spawn(fun() -> room([]) end),
    spawn(fun() -> acceptor(LSock, Room) end),
    receive stop -> ok end.

acceptor(LSock, Room) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Room) end),
    Room ! {enter, self()},
    user(Sock, Room).

process(Msg) ->
    Words = string:split(Msg, " ", all),
    [Car|Cdr] = Words,
    case Car of
        "signup" ->
            login:create_account(hd([Cdr]), tl(Cdr));
	    %game:start(hd([Cdr]));
        "delete" ->
            login:close_account(hd([Cdr]), tl(Cdr)),
	    game:del_game();
        "login" ->
            login:login(hd([Cdr]), tl(Cdr));
        "join" -> case list:member(Cdr, login:online()) of
			true -> game:join_game(Cdr);
			false -> ok
		  end;
        "logout" ->
            login:logout(hd([Cdr])),
	    game:stop_game();
        _ ->
            ok% boomer
    end.

room(Pids) ->
    receive
        {enter, Pid} ->
            io:format("user_entered~n", []),
            room([Pid | Pids]);
        {line, Data} ->
            io:format("received ~p~n", [Data]),
            [Msg | _] = string:split(string:to_lower(binary:bin_to_list(Data)), "\n"),
            process(Msg),
            %[Pid ! Msg || Pid <- Pids],
            room(Pids);
        {leave, Pid} ->
            io:format("user_left~n", []),
            room(Pids -- [Pid])
    end.

user(Sock, Room) ->
    receive
        {line, Data} ->
            gen_tcp:send(Sock, Data),
            user(Sock, Room);
        {tcp, _, Data} ->
            Room ! {line, Data},
            user(Sock, Room);
        {tcp_closed, _} ->
            Room ! {leave, self()};
        {tcp_error, _, _} ->
            Room ! {leave, self()}
    end.

