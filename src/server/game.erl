-module(game).
-export([start/0,
	 join_game/2,
	 get_matches/0
        ]).

start() ->
	spawn(fun() -> game([]) end).

game(PlayersPids,NiveldaSala) when length(PlayersPids) < 2 ->
	receive
		{join_game,Username,PlayerPid} ->
		case length(PlayersPids) > 0 of
		    true ->
			NivelPlayer = login:player_level(Username),
			if
			    (((NivelPlayer + 1) == NiveldaSala) or ((NivelPlayer-1) == NiveldaSala)) ->
				game([[PlayerPid,Username] | PlayersPids],NiveldaSala);
			    true -> 
				game([[PlayerPid,Username] | PlayersPids],login:player_level(Username))


			end
		end
	end;

game(PlayersPids,NiveldaSala) when ((length(PlayersPids) >= 2) and (length(PlayersPids) < 4)) ->
    Game = self(),
    Timer = spawn(fun() -> receive after 5000 -> Game ! timeout end end),
    receive
	timeout ->

            Participants = through_players(PlayersPids),
            Planets = generate_planets(randomNumRange(2,5)),

	    spawn(fun() -> initMatch(Participants,Planets) end),
	    [ PlayerPid ! {in_match, [Participants, Planets]} || [PlayerPid | _] <- PlayersPids],

            game([],0);

	{join_game,Username,PlayerPid} ->
            exit(Timer,kill),

	    game([[PlayerPid,Username] | PlayersPids],NiveldaSala)
    end.

game(PlayersPids) ->
    
    Participants = through_players(PlayersPids),
    Planets = generate_planets(randomNumRange(2,5)),
    
    ?MODULE ! {match,Participants,Planets},
    ?MODULE ! {add_match,[Participants,Planets]},


    spawn(fun() -> initMatch(Participants,Planets) end),

    game([]).

join_game(PlayerPid,Username) ->
    ?MODULE ! {join_game,Username,PlayerPid},
    receive
        {match,Participants,Planets} ->
            [Participants,Planets]
    end.

matchesOccurring(Matches) ->
    NewMatches =
        receive
            {request_matches} -> 
                ?MODULE ! {Matches},
                Matches;
            {add_match, Match} -> 
                [Match | Matches];
            {remove, Match} -> 
                lists:delete(Match, Matches)
        end,
    matchesOccurring(NewMatches).


get_matches() ->
    ?MODULE ! {request_matches}, % Completar onde vai receber isto
    receive
        {Matches} -> Matches % Completar isto
    end.

%-----------------------------MATCH------------------------------

getPid(Value) ->
    {_,_,_,_,
     _,_,_,_,
     _,_,_,Pid}= Value,
    Pid.


initMatch(Players, Planets) ->
    TestRemaining = length(maps:keys(maps:filter(true,Players))),
    TestWinner    = length(maps:keys(maps:filter(true,Planets))),
    Values = maps:values(Players),
    Pids = [getPid(Value) || Value  <- Values],
    if (TestRemaining == 0) -> ?MODULE ! {remove, [Players, Planets]}
    end,
    if
	(TestRemaining == 0) and (TestWinner == 1) ->
	    [Pid ! {tickrate, [Players, Planets]} || Pid <- Pids];
	(TestRemaining == 0) ->
	    [Pid ! {tickrate, [Players, Planets]} || Pid <- Pids];
	true ->
	    NewPlanets =
		updatePlanetsPos(Planets,
				 lists:len(maps:keys(Planets))),
	    NewPlayers =
		updatePlayerPos(Players,
				lists:len(maps:keys(Planets))),
	    [Pid ! {update_data, [Players, Planets]} || Pid <- Pids],
	    initMatch(NewPlayers, NewPlanets)

    end.

%----------------------------HANDLES----------------------------
handle({"UP", Pid},PlayersInfo) ->
    {X,Y,Vx0,Vy0,Diameter,Angle,
     R,G,B,Fuel,
     WaitingGame,InGame,GameOver, Pid}= maps:get(Pid,PlayersInfo),

    NewInfo = 0,
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),
    
    {ok,NextPlayers};

handle({"LEFT", Pid},PlayersInfo) ->
    {X,Y,Vx0, Vy0,Diameter,Angle,
     R,G,B,Fuel,
     WaitingGame,InGame,GameOver}= maps:get(Pid,PlayersInfo),

    NewInfo = 0,
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),
    
    {ok,NextPlayers};

handle({"RIGHT", Pid},PlayersInfo) ->
    {X,Y,Vx0, Vy0, Diameter,Angle,
     R,G,B,Fuel,
     WaitingGame,InGame,GameOver}= maps:get(Pid,PlayersInfo),

    NewInfo = 0,
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),

    {ok,NextPlayers}.


%-----------------------FUNCOES AUXILIARES----------------------
randomNumRange(Small,Big) ->
    % É preciso fazer seed no incio
    rand:uniform(Big - Small + 1) + Small - 1.

generate_planets(Int) -> 
    Sistema = #{0 => {0,960,540,35,255,255,0}}, % here comes the sun
    generate_planets(Int, Sistema).
generate_planets(Int,Sistema) ->
    Distancia = Int * randomNumRange(40,100),
    % Ver se vale a pena colocar distancia sol-planeta
    SistemaNovo = Sistema#{Int => {Distancia,
				   randomNumRange(300,1600),
				   randomNumRange(200,850),
				   randomNumRange(50,100),
				   randomNumRange(20,80),
				   randomNumRange(4,20),
				   randomNumRange(90,255),
				   randomNumRange(90,255),
				   randomNumRange(90,255)}},
    generate_planets(Int-1,SistemaNovo);
generate_planets(0, Sistema) ->
    Sistema.

newPlayerPos(Pid, Player,Map) ->
    X0 = randomNumRange(300,1600),
    Y0 = randomNumRange(200,850),
    if
        X0 >= 960 -> X = 1750;
        X0 < 960 -> X = 150
    end,

    if
        Y0 >= 540 -> Y = 900;
        Y0 < 540 -> Y = 100
    end,

    MapNew = Map#{Player => {X,Y,5,
			     0,0,
			     0,
			     randomNumRange(90,255),
			     randomNumRange(90,255),
			     randomNumRange(90,255),
			     100,
			     false,true,false, Pid}},
    MapNew.


through_players([]) ->
    #{};
through_players([[Pid | [Username|[]]] | T]) ->
    Map = newPlayerPos(Pid,Username,#{}),
    through_players(T,Map).

through_players([[Pid | [Username|[]]] | T],Map) ->
    MapNew = newPlayerPos(Pid,Username,Map),
    through_players(T,MapNew);

through_players([],Map) -> 
    Map.

updatePlanetsPos(Planets, 0) ->
    Planets;
updatePlanetsPos(Planets,NumPlanet) ->
%TODO calculos dos planetas por tick
    {Dist, X0, Y0, Vx0, Vy0, Diameter, R, G, B} =
	maps:get(Planets, NumPlanet),
    V = math:sqrt(Vx0*Vx0 + Vy0*Vy0),
    Ac = V*V/Dist,
    Theta = math:atan2(X0, Y0),
    Ax = -Ac * math:cos(Theta),
    Ay = -Ac * math:sin(Theta),
    X = X0 + Vx0*0.0021 + Ax*0.0021*0.0021/2,
    Y = Y0 + Vy0*0.0021 + Ay*0.0021*0.0021/2,
    Vx = Vx0 + Ax*0.0021,
    Vy = Vy0 + Ay*0.0021,
    NewPlanet = {Dist,
		 X,
		 Y,
		 Vx,
		 Vy,
		 Diameter,
		 R,G,B
		},
    NewPlanets = maps:update(NumPlanet, NewPlanet, Planets),
    updatePlanetsPos(NewPlanets,NumPlanet-1).

updatePlayerPos(Players, [Player | T]) ->
    {X0,Y0, Vx0, Vy0, Diameter, Angle, R,G,B,Fuel,
     WaitingGame,InGame,GameOver} = maps:get(Player, Players),
    
    NewPlayer = {X0,Y0,Vx0, Vy0, Diameter,Angle,

		 R,G,B,Fuel,WaitingGame,
		 InGame,GameOver},
    NewPlayers = maps:update(Player, NewPlayer, Players),
    updatePlayerPos(NewPlayers, T).

