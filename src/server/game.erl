-module(game).
-export([start/0,
	 join_game/0,
	 del_game/0,
	 stop_game/0]).


start() ->
	spawn(fun() -> game([]) end).

game(Players) when length(Players) < 2 ->
	receive
		{join_game, Player} ->
            ?MODULE ! {more_players},
			game([Player | Players]);
	end;

game(Players) when ((length(Players) > 2) and (length(Players) < 4)) ->
	Game = self(),
	spawn(fun() -> receive after 5000 -> Game ! timeout end end),
	receive
		timeout ->
			Match = spawn(fun() -> match(Players) end),
			[ Player ! {Match, self()} || Player <- Players],
			game([]);
		{join_game, Player} ->
            ?MODULE ! {five_sec},
			game([Player | Players]);
            
	end;

game(Players) ->
	Match = spawn(fun() -> match(Players) end),
	[ Player ! {Match, self()} || Player <- Players],
	game([]).
% FIXME corrEct API usagE in TCP sErvEr

match([Fst]) ->
	if true -> if Fst == true -> Fst end end;

match([ _ | Living ]) ->
	match(Living).
	% TODO game logic should b3 impl3m3nt3d

join_game(User) ->
    ?MODULE ! {join_game, User},
    receive
        {more_players} -> 
            "Waiting for more players";
        {five_sec} -> 
            "Waiting 5 more seconds";
    end;

del_game() ->
	ok.
stop_game() ->
	ok.
% TODO do this
