-module(server).
-export([start/1, stop/1]).

% erlc server.erl
% erl
% server:start(1234)
% outro terminal, nc localhost 1234
% outro terminal netstat -p tcp -na | grep 1234

start(Port) -> spawn(fun() -> server(Port) end).
% TODO importante, when a server closes it's open ports,
% all clients should be dropped, this doesn't seem to happen
% VULNERABILITY
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
        "\signup" ->
            login:create_account(hd([Cdr]), lists:last([Cdr]));
        "\delete" ->
            login:close_account(hd([Cdr]), lists:last([Cdr]));
        "\login" ->
            login:login(hd([Cdr]), lists:last([Cdr]));
        "\logout" ->
            login:logout(hd([Cdr]));
        _ ->
            ok
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

