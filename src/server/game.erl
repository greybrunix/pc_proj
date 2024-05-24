-module(game).
-export([start/0,
	     join_game/1,
	     del_game/0,
	     stop_game/0,
         match/1
        ]).


start() ->
	spawn(fun() -> game([]) end).

game(Players) when length(Players) < 2 ->
	receive
		{join_game, Player} ->
            ?MODULE ! {more_players},
			game([Player | Players]);
	end;

game(Players) when ((length(Players) >= 2) and (length(Players) < 4)) ->
	Game = self(),
	Timer = spawn(fun() -> receive after 5000 -> Game ! timeout end end),
	receive
		timeout ->
			Match = spawn(fun() -> match(Players) end),
			[ Player ! {Match, self()} || Player <- Players],
			game([]);
		{join_game, Player} ->
            exit(Timer,kill),
			game([Player | Players]);
            
	end;

game(Players) ->
	Match = spawn(fun() -> match(Players) end),
	[ Player ! {Match, self()} || Player <- Players],
	game([]).
% FIXME corrEct API usagE in TCP sErvEr

join_game(UserPid) ->
    ?MODULE ! {join_game, UserPid},
    receive
        {start} -> "Starting"
    end;

keyPressed(Key, UserPid) -> 
    ?MODULE ! {key, Key, } 

del_game() ->
	ok.
stop_game() ->
	ok.

%-----------------------------MATCH------------------------------
match([Player | Players]) ->
    	
	% TODO game logic should b3 impl3m3nt3d


% TODO do this
