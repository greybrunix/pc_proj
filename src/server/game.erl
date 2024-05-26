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
    
    Planets = generate_planets(randomNumRange(2,5)),
	% TODO game logic should b3 impl3m3nt3d


% TODO do this

%-----------------------FUNCOES AUXILIARES----------------------
randomNumRange(Small,Big) ->
    random:uniform(Big - Small + 1) + Small - 1;

generate_planets(Int) -> 
    Sistema = Map#{0 => {0,960,540,35,255,255,0}}, % here comes the sun
    generate_planets(Int, Sistema);

generate_planets(Int,Sistema) ->
    case Int of ->
         0 ->
            Sistema;
         _ -> 
            Distancia = Int * randomNumRange(40,100),
            % Ver se vale a pena colocar distancia sol-planeta
            Sistema = maps:put(Int,{Distancia,
                                    randomNumRange(300,1600),
                                    randomNumRange(200,850),
                                    randomNumRange(4,20),
                                    randomNumRange(90,255),
                                    randomNumRange(90,255),
                                    randomNumRange(90,255)}),
            generate_planets(Int-1,Sistema);


newPlayerPos(Player,Map) ->
    X = randomNumRange(300,1600),
    Y = randomNumRange(200,850),
    if
        X >= 960 -> X = 1750;
        X < 960 -> X = 150;
    end,

    if
        Y >= 540 -> Y = 900;
        Y < 540 -> Y = 100;
    end,

    Map = maps:put(Player,{X,Y,5,
                           X,Y,0,
                           15,X+math:cos(0) * 15,Y+math:sin(0) * 15,
                           0,0.2,
                           randomNumRange(90,255),
                           randomNumRange(90,255),
                           randomNumRange(90,255),
                           100,
                           false,true,false
                          }
                   Map),
    Map;

through_players(List) ->
    Map = maps:new(),
    newPlayerPos(hd(List),Map),
    through_players(tl(List),Map);

through_players(List,Map) when length(List) > 0 ->    
    newPlayerPos(hd(List),Map),
    through_players(tl(List),Map);

through_players([],Map) -> 
    Map;
