-module(server).
-export([start/1, stop/0]).

start(Port) -> register(?MODULE, spawn(fun() -> server(Port) end)).
stop() -> ?MODULE ! stop.

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
	    Matches = game:get_matches(),
	    Match = [case maps:is_key(lists:last(Cdr),Participants) of
			 true -> [Participants]; false -> [] end|| [Participants,_]<-Matches],
	    case length(Match) of
		1 -> game:keyPressed(hd(Cdr),lists:last(Cdr),hd(Match)); % TODO verificar se já esta numa partida
		_ -> "invalid"
	    end;
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
            NewMatch  = formatMatch(Match),
            JsonMatch = jsx:encode(NewMatch),
            gen_tcp:send(Sock, <<JsonMatch/binary, "\n">>), % Adding newline for clarity
            user(Sock, Room);
        {match,Participants,Planets} ->
            self() ! {update_data, [Participants,Planets]},
            user(Sock, Room)
    end.

formatMatch(Match) ->
    Players = maps:get("Players", Match),
    Planets = maps:get("Planets", Match),

    Formatted_Plyr = formatMatchPlayer(Players, maps:keys(Players)),
    Formatted_Plnt = formatMatchPlanets(Planets, maps:keys(Planets)),

    #{<<"players">> => Formatted_Plyr, <<"planets">> => Formatted_Plnt}.

formatMatchPlayer(Map, []) ->
    Map;
formatMatchPlayer(Map, [Username | T]) ->
    {X,Y,_Vx,_Vy, Diameter, Angle, R,G,B, Fuel, _, InGame, GameOver, _} =
	maps:get(Username, Map),

    NewMap0 = maps:remove(Username, Map),
    NewMap  = NewMap0#{list_to_binary(Username) => #{<<"x">> => float_to_binary(X),
						     <<"y">> => float_to_binary(Y),
						     <<"diameter">> => float_to_binary(Diameter),
						     <<"angle">> => float_to_binary(Angle),
						     <<"r">> => float_to_binary(R),
						     <<"g">> => float_to_binary(G),
						     <<"b">> => float_to_binary(B),
						     <<"fuel">> => float_to_binary(Fuel),
						     <<"inGame">> => atom_to_binary(InGame),
						     <<"hasLost">> => atom_to_binary(GameOver)}},
    formatMatchPlayer(NewMap, T).

formatMatchPlanets(Map, []) ->
    Map;
formatMatchPlanets(Map, [Num | T]) ->
    {Dist, X, Y, _Vx, _Vy, Diameter, R, G, B} =
	maps:get(Num, Map),

    NewMap0 = maps:remove(Num, Map),
    NewMap  = NewMap0#{integer_to_binary(Num) => #{<<"distance">> => float_to_binary(Dist),
						<<"x">> => float_to_binary(X),
						<<"y">> => float_to_binary(Y),
						<<"diameter">> => float_to_binary(Diameter),
						<<"r">> => float_to_binary(R),
						<<"g">> => float_to_binary(G),
						<<"b">> => float_to_binary(B)}},
    formatMatchPlanets(NewMap, T).
