-module(server).
-export([start/1, stop/1]).

start(Port) -> spawn(fun() -> server(Port) end).
stop(Server) -> Server ! stop.

server(Port) ->
    login:start(),
    game:start(),
    {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, line}, {reuseaddr, true}]),
    Room = spawn(fun() -> room({}) end),
    spawn(fun() -> acceptor(LSock, Room) end),
    receive stop -> ok end.

acceptor(LSock, Room) ->
    {ok, Sock} = gen_tcp:accept(LSock),
    spawn(fun() -> acceptor(LSock, Room) end),
    Room ! {enter, self()},
    user(Sock, Room).

parser(Msg, Pid) ->
    Words = string:split(Msg, " ", all),
    [Car|Cdr] = Words,
    case Car of
        "signup" ->
            T = login:create_account(hd(Cdr), tl(Cdr)),
            io_lib:format("~p~n", [T]);
        "delete" ->
            T = login:close_account(hd(Cdr)),
            io_lib:format("~p~n", [T]);
        "login" ->
            T = login:login(hd(Cdr), tl(Cdr)),
            io_lib:format("~p~n", [T]);
        "join" -> 
            ElemBin = list_to_binary(hd(Cdr)),
            Elem_str = << "\"", ElemBin/binary, "\"" >>,
            case lists:member(Elem_str, login:online()) of
                      true -> 
                          io:format("join no server~p~n", [Pid]),
                          game:join_game(Pid,hd(Cdr));
                      false -> "invalid"
                  end;
        "leaderboard" -> 
            ok;

        "logout" ->
            T = login:logout(hd(Cdr)),
            io_lib:format("~p~n", [T]);
        "key" -> 
            game:keyPressed(hd(Cdr),lists:last(Cdr)); % TODO verificar se já esta numa partida
        _ ->
            io_lib:format("~p~n", [no_command])
    end.

room(Pids) ->
    receive
        {enter, Pid} ->
            io:format("user_entered~p~n", [Pid]),
            room([Pid | Pids]);
        {leave, Pid} ->
            io:format("user_left~p~n", [Pid]),
            room(lists:delete(Pid, Pids))
    end.

user(Sock, Room) ->
    Player = self(),
    receive
        {in_match,Match} ->
            io:format("EI CAPUTA ~p~n", [Match]),
	    self() ! {update_data,Match};
            %TODO ver como guardar Partidas
        {matchover,Reason,Pid,Participants} ->
            case Reason of
                timeover ->
                    ok;
                has_winner ->
                    ok;
                all_lost -> 
                    ok
                %TODO aqui atualizar a informaçao geral do jogador
            end;

        {line, Data} ->
            gen_tcp:send(Sock, Data),
            user(Sock,Room);
        {tcp, _, Data} ->
            [Msg | _] = string:split(string:to_lower(binary:bin_to_list(Data)), "\n"),
            io:format("mensagem:~p~n", [Msg]),
            Player ! {line,list_to_binary([parser(Msg,self())])},
            user(Sock,Room);
        {tcp_closed, _} ->
            Room ! {leave, self()};
        {tcp_error, _, _} ->
            Room ! {leave, self()};
        {update_data, Match} ->
            JsonMatch = jsx:encode(Match),
            io:format("~p~n", [Match]),
            BinaryJsonMatch = list_to_binary(JsonMatch),
            gen_tcp:send(Sock, BinaryJsonMatch),
            user(Sock, Room);
        {match,Participants,Planets} ->
            self() ! {update_data, [Participants,Planets]},
            user(Sock, Room)
    end.
