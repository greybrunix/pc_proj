-module(game).
-export([start/0,
	     join_game/1,
	     del_game/0,
	     stop_game/0,
         match/1
        ]).


start() ->
	spawn(fun() -> game([]) end).


loop(State) ->
	receive
		{Request, From} ->
			{Result, NextState} = handle(Request, State),
			From ! {Result, ?MODULE},
			loop(NextState)
	end.

game(PlayersPids) when length(PlayersPids) < 2 ->
	receive
		{join_game, UserPid} ->
            ?MODULE ! {more_players},
			game([UserPid | PlayersPids]);
	end;

game(PlayersPids) when ((length(PlayersPids) >= 2) and (length(PlayersPids) < 4)) ->
	Game = self(),
	Timer = spawn(fun() -> receive after 5000 -> Game ! timeout end end),
	receive
		timeout ->
            Participants = through_players(PlayersPids),
			Match = spawn(fun() -> match(Participants) end),
			[ PlayerPid ! {Match, self()} || PlayerPid <- PlayersPids],
			game([]);

		{join_game, PlayerPid} ->
            exit(Timer,kill),
			game([PlayerPid | PlayersPids]);
            
	end;

game(PlayersPids) ->
    Participants = through_players(PlayersPids),
	Match = spawn(fun() -> match(Participants) end),

	[ PlayerPid ! {Match, self()} || PlayerPid <- PlayersPids],
	game([]).

% FIXME corrEct API usagE in TCP sErvEr

join_game(PlayerPid) ->
    ?MODULE ! {join_game, PlayerPid},
    receive
        {start} -> "Starting" end;
keyPressed(Key, PlayerPid) -> 
    ?MODULE ! {Request, Key},
    
del_game() ->
	ok.
stop_game() ->
	ok.

%-----------------------------MATCH------------------------------
match(Map) ->
    
	% TODO game logic should b3 impl3m3nt3d


% TODO do this

%-----------------------FUNCOES AUXILIARES----------------------
generate_planets(Int) -> 

    case Int of -> 
        0 ->




through_players(List) ->
    Planets = generate_planets(random:uniform(5)),
    Map = maps:new(),
    Map#{hd(List) => {x,y,10,
                      targetX,targetY,angle,
                      lineLength,lineEndX,lineEndY,
                      targetAngle,easingAngle,
                      r,g,b,
                      100,
                      false,true,false
                     }
        },
    through_players(tl(List),Map);

through_players(List,Map) when length(List) > 0 ->    
    Map#{hd(List) => {x,y,10,
                      targetX,targetY,angle,
                      lineLength,lineEndX,lineEndY,
                      targetAngle,easingAngle,
                      r,g,b,
                      100,
                      false,true,false
                     }
        },
    through_players(tl(List),Map);

through_players([],Map) -> 
    Map;
