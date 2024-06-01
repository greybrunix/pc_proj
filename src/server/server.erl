-module(process).
-export([parser/2]).

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
    user(Sock, Room, 0).

parser(Msg, Pid, MatchPid) when MatchPid == 0 ->

    Words = string:split(Msg, " ", all),
    [Car|Cdr] = Words,
    case Car of
        "signup" ->
            T = login:create_account(hd(Cdr), tl(Cdr)),
            io_lib:format("~p~n", [T]);
        "delete" ->
            T = login:close_account(hd(Cdr), tl(Cdr)),
            game:del_game(),
            io_lib:format("~p~n", [T]);
        "login" ->
            T = login:login(hd(Cdr), tl(Cdr)),
            io_lib:format("~p~n", [T]);
        "join" -> case lists:member(hd(Cdr), login:online()) of
                    true -> game:join_game(Pid);
                    false -> "ok"
                end;
        "key" -> 
            game:keyPressed(hd(Cdr),lists:last(Cdr));
            %TODO como 
            

        "leaderboard" -> 
            ok;

        "logout" ->
            T = login:logout(hd(Cdr)),
            game:stop_game(),
            io_lib:format("~p~n", [T]);
        _ ->
            io_lib:format("~p~n", [no_command])% boomer
    end.

parser(Msg, Pid, MatchPid) -> 
    Words = string:split(Msg, " ", all),
    [Car|Cdr] = Words,
    case Car of
        "key" -> 
            game:keyPressed(hd(Cdr),lists:last(Cdr));
    end.

room(Pids) ->
    receive
        {enter, Pid} ->
            io:format("user_entered~p~n", [{Pid, ""}]),
            room([{Pid,""} | Pids]);
        {line, Data, Pid} ->
            io:format("received ~p~n", [Data]),
            Pid ! {line, list_to_binary([process(Msg, Pid)])},
            room(Pids);
        {leave, Pid} ->
            io:format("user_left~p~n", [Pid]),
            room(Pids -- [{Pid,""}])
    end.

user(Sock, Room, MatchPid) ->
    Player = self(),
    receive
        {in_match,Match,Party} ->
            self() ! {line,"starting match...\n"};
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
            user(Sock,Room,MatchPid);
        {tcp, _, Data} ->
            [Msg | _] = string:split(string:to_lower(binary:bin_to_list(Data)), "\n"),
            Player ! {line,list_to_binary([parser(Msg,self(),MatchPid)])}
            user(Sock,Room,MatchPid);
        {tcp_closed, _} ->
            Room ! {leave, self()};
        {tcp_error, _, _} ->
            Room ! {leave, self()}
    end.
