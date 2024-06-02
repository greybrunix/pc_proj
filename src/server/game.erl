-module(game).
-export([start/0,
	 join_game/2,
	 keyPressed/3,
	 get_matches/0
        ]).

start() ->
	register(memory, spawn(fun() -> matchesOccurring([]) end)),
	register(?MODULE, spawn(fun() -> game([]) end)).

verifyPlayerLevel(Level, [[_|_], NiveldaSala]) ->
    Level1 = case is_binary(Level) of
                 true -> 
                     Level2 = binary_to_list(Level),
                     list_to_integer(Level2);
                 false -> list_to_integer(Level)
             end,
    NiveldaSala1 = case is_binary(NiveldaSala) of
                       true -> 
                           NiveldaSala2 = binary_to_list(NiveldaSala),
                           list_to_integer(NiveldaSala2);
                       false -> list_to_integer(NiveldaSala)
                   end,
    (((Level1 + 1) == NiveldaSala1) or ((Level1 - 1) == NiveldaSala1)) or
    (Level1 == NiveldaSala1).

game([]) ->
    receive
	{join_game,Username,PlayerPid} ->
            game([[[{PlayerPid,Username}],login:player_level(Username)]])
    end;

game([[Participants,NiveldaSala]|Salas]) ->

    Game = self(),
    Size = length(Participants),
    case ((Size>1) and (Size < 4)) of
        true ->
            Timer = spawn(fun() -> receive after 5000 -> Game ! {timeout} end end);
        false ->
            Timer = 0
    end,
        
    receive
	{join_game, Username, PlayerPid} ->
	    NivelPlayer = login:player_level(Username),
	    case verifyPlayerLevel(NivelPlayer, [Participants, NiveldaSala]) of
		true ->
		    ParticipantsLen = length(Participants),
		    case ParticipantsLen of
			4 ->
			    ParticipantsMap = through_players(Participants),
			    Planets = generate_planets(randomNumRange(2, 5)),

			    memory ! {add_match, [ParticipantsMap, Planets]},

			    spawn(fun() -> initMatch(ParticipantsMap, Planets) end),

			    [PlayerPidS ! {match, ParticipantsMap, Planets} || {PlayerPidS, _} <- Participants],

			    game(Salas);
		        _ when (ParticipantsLen > 1) and (ParticipantsLen < 4) ->
			    if (Timer /= 0) ->
				    exit(Timer, kill);
			       true -> ok
			    end,
			    game(Salas ++ [[[{PlayerPid, Username} | Participants], NiveldaSala]]);
			1 ->
			    game(Salas ++ [[[{PlayerPid, Username} | Participants], NiveldaSala]])
		    end;
		false ->
		    case length(Salas) of
			0 ->
			    game([[Participants, NiveldaSala] ++ [[{PlayerPid, Username}], NivelPlayer]]);
			_ ->
			    %% Handle other cases if necessary
			    game(Salas)
		    end
	    end;
	{timeout} ->
            ParticipantsMap = through_players(Participants),
            Planets = generate_planets(randomNumRange(2,5)),
            
	        
            MatchPid = spawn(fun() -> initMatch(ParticipantsMap,Planets) end),
           	memory ! {add_match,[ParticipantsMap,Planets, MatchPid]},
	    [ PlayerPid ! {in_match,ParticipantsMap,Planets} || {PlayerPid,_} <- Participants],
                                   
            game(Salas)
    end.

join_game(PlayerPid,Username) ->
    ?MODULE ! {join_game,Username,PlayerPid},
    "ok".

matchesOccurring(Matches) ->
    NewMatches =
        receive
            {request_matches, From} -> 
                From ! {response, Matches, memory},
                Matches;
            {add_match, Match} -> 
                [Match | Matches];
            {remove, Match} -> 
                lists:delete(Match, Matches)
        end,
    matchesOccurring(NewMatches).


get_matches() ->
    memory ! {request_matches, self()}, % Completar onde vai receber isto
    receive
        {response, Matches, memory} -> Matches % Completar isto
    end.

keyPressed(Key,Username,[Players, Planets, Pid]) -> 
    {ok,Next} = handle({Key,Username},Players),
    NewMatch = [Next,Planets],

    memory ! {remove,[Players,Planets]},
    memory ! {add_match,NewMatch},

    Pid ! {switch, Next, Planets}, % no initMatch, depois de fazer
                                       % a chamada recursiva mais recente, recebe
                                       % esta guarda, que substitui os argumentos da
                                       % chamada recursiva, pelos novos
    "ok".

%-----------------------------MATCH------------------------------

getPid(Value) ->
    {_,_,_,_,
     _,_,_,_,
     _,_,_,_,_,Pid}= Value,
    Pid.


initMatch(Players, Planets) ->

    % Filter to count players who are still in the game
    TestRemaining = length(maps:keys(maps:filter(fun(_Key, Value) ->
        {_, _, _, _, _, _, _, _, _, _, _, InGame, _,_} = Value,
        InGame
    end, Players))),
    
    % Filter to count players who are not GameOver and still in the game
    TestWinner = length(maps:keys(maps:filter(fun(_Key, Value) ->
        {_, _, _, _, _, _, _, _, _, _, _, InGame, GameOver,_} = Value,
        InGame and not GameOver
    end, Players))),
    
    Values = maps:values(Players),
    Pids = [getPid(Value) || Value <- Values],

    Tick = spawn(fun() -> receive after 20 -> self() ! {tickover,Players,Planets} end end),    
    receive
        {tickover,DoNotSwitchPlayers,DoNotSwitchPlanets} -> %do initMatch 
            Players = DoNotSwitchPlayers,
            Planets = DoNotSwitchPlanets;

        {switch, NextPlayers, NextPlanets} -> %do keyPressed
            exit(Tick,stop),
            Players = NextPlayers,
            Planets = NextPlanets
    end,

    if ((TestRemaining == 0) orelse (TestWinner == 1)) -> memory ! {remove, [Players, Planets]};
                                                          true -> ok
    end,

    if
        (TestRemaining == 0) and (TestWinner == 1) ->
            [Pid ! {tickrate, [Players, Planets]} || Pid <- Pids];
        (TestRemaining == 0) ->
            [Pid ! {tickrate, [Players, Planets]} || Pid <- Pids];
        true ->
            NewPlanets =
                updatePlanetsPos(Planets, length(maps:keys(Planets))-1),
            NewPlayers =
                updatePlayersPos({Planets, Players}, maps:keys(Players)),
            NewPlayers1 = 
                detectPlayerCollisions(NewPlayers, maps:keys(NewPlayers)),
            [Pid ! {update_data, #{"Players" => NewPlayers1, "Planets" => (NewPlanets)}} || Pid <- Pids],
            initMatch(NewPlayers1, NewPlanets)
    end.
%----------------------------HANDLES----------------------------
handle({"UP", Username},PlayersInfo) ->
    {X,Y,Vx0,Vy0,Diameter,Angle,
     R,G,B,Fuel,
     WaitingGame,InGame,GameOver, Pid_}= maps:get(Username,PlayersInfo),
    Vx = Vx0 + 0.5 * math:cos((Angle * math:pi()) / 180), % Apply linear acceleration over Angle direction
    Vy = Vy0 + 0.5 * math:sin((Angle * math:pi()) / 180), % Apply linear acceleration over Angle direction
    NewInfo = {X,Y,Vx,Vy,Diameter,Angle,
	       R,G,B,Fuel,WaitingGame,InGame,GameOver,Pid_},
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),
    
    {ok,NextPlayers};

handle({"LEFT", Username},PlayersInfo) ->
    {X,Y,Vx0, Vy0,Diameter,Angle0,
     R,G,B,Fuel,
     WaitingGame,InGame,GameOver,Pid_}= maps:get(Username,PlayersInfo),
    Angle = (Angle0 + 10) rem 360,
    Vx = Vx0 * math:cos((Angle * math:pi()) / 180) - Vy0 * math:sin((Angle * math:pi()) / 180),
    Vy = Vx0 * math:sin((Angle * math:pi()) / 180) + Vy0 * math:cos((Angle * math:pi()) / 180),
    NewInfo = {X,Y,Vx,Vy,Diameter,Angle,
	       R,G,B,Fuel,WaitingGame,InGame,GameOver,Pid_},
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),
    
    {ok,NextPlayers};

handle({"RIGHT", Username},PlayersInfo) ->
    {X,Y,Vx0, Vy0, Diameter,Angle0,
     R,G,B,Fuel,
     WaitingGame,InGame,GameOver, Pid_}= maps:get(Username,PlayersInfo),
    Angle = (Angle0 - 10) rem 360, % Compute new angle in clockwise direction
    Vx = Vx0 * math:cos((Angle * math:pi()) / 180) - Vy0 * math:sin((Angle * math:pi()) / 180),
    Vy = Vx0 * math:sin((Angle * math:pi()) / 180) + Vy0 * math:cos((Angle * math:pi()) / 180),

    NewInfo = {X,Y,Vx,Vy,Diameter,Angle,
	       R,G,B,Fuel-math:sqrt(Vx*Vx+Vy*Vy),WaitingGame,InGame,GameOver,Pid_},
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),

    {ok,NextPlayers}.


%-----------------------FUNCOES AUXILIARES----------------------
randomNumRange(Small,Big) ->
    % É preciso fazer seed no incio
    rand:uniform(Big - Small + 1) + Small - 1.

generate_planets(Int) -> 
    Sistema = #{0 => {0.0,960.0,540.0,0.0,0.0,35.0,255.0,255.0,0.0}}, % here comes the sun
    generate_planets(Int, Sistema).

generate_planets(0, Sistema) ->
    io:format("PlanetaZeroSistema~n"), 
    Sistema;
generate_planets(Int,Sistema) ->
    io:format("PlanetaIntSistema~n"), 
    Distancia = Int * randomNumRange(40,100),
    % Ver se vale a pena colocar distancia sol-planeta
    SistemaNovo = Sistema#{Int => {float(Distancia),
				   float(randomNumRange(300,1600)),
				   float(randomNumRange(200,850)),
				   float(randomNumRange(50,100)),
				   float(randomNumRange(20,80)),
				   float(randomNumRange(4,20)),
				   float(randomNumRange(90,255)),
				   float(randomNumRange(90,255)),
				   float(randomNumRange(90,255))}},
    generate_planets(Int-1,SistemaNovo).

newPlayerPos(Pid, Player,Map) ->
    X0 = float(randomNumRange(300,1600)),
    Y0 = float(randomNumRange(200,850)),
    if
        X0 >= 960.0 -> X = 1750.0;
        X0 < 960.0 -> X = 150.0
    end,

    if
        Y0 >= 540.0 -> Y = 900.0;
        Y0 < 540.0 -> Y = 100.0
    end,

    MapNew = Map#{Player => {float(X),float(Y),5.0,
			     0.0,0.0,
			     0.0,
			     float(randomNumRange(90,255)),
			     float(randomNumRange(90,255)),
			     float(randomNumRange(90,255)),
			     float(100),
			     false,true,false, Pid}},
    MapNew.


through_players([{Pid, Username} | T]) ->
    Map = newPlayerPos(Pid,Username,#{}),
    through_players(T,Map).

through_players([{Pid, Username} | T],Map) ->
    MapNew = newPlayerPos(Pid,Username,Map),
    through_players(T,MapNew);

through_players([],Map) -> 
    Map.

updatePlanetsPos(Planets, 0) ->
    Planets;
updatePlanetsPos(Planets,NumPlanet) ->
%TODO calculos dos planetas por tick
    {Dist, X0, Y0, Vx0, Vy0, Diameter, R, G, B} =
	maps:get(NumPlanet, Planets),
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

updatePlayersPos({_Planets,Players}, []) ->
    Players;
updatePlayersPos({Planets, Players}, [Player | T]) ->
    {X0,Y0, Vx0, Vy0, Diameter, Angle, R,G,B,Fuel,
     WaitingGame,InGame_,GameOver_, Pid} = maps:get(Player, Players),
    X = X0 + Vx0*0.0021,
    Y = Y0 + Vy0*0.0021,
    Vx = Vx0,
    Vy = Vy0,

   PlanetsList = maps:to_list(Planets),

    CollidingPlanets = lists:filter(fun({_, {_, PX, PY, _, _, PDiameter, _, _, _}}) ->
    Distance = math:sqrt((X - PX) * (X - PX) + (Y - PY) * (Y - PY)),
    RadiusSum = (Diameter / 2) + (PDiameter / 2),
    Distance < RadiusSum

    end, PlanetsList), 
    
    % Update game status based on collisions
    Collided = length(CollidingPlanets) > 0,
    if
        Collided ->
            GameOver = true,
            InGame = false;
        true ->
            GameOver = GameOver_,
            InGame = InGame_
    end,
    NewPlayer = {X,Y,Vx, Vy, Diameter,Angle,
		 R,G,B,Fuel,WaitingGame,
		 InGame,GameOver, Pid},
    NewPlayers = maps:update(Player, NewPlayer, Players),
    updatePlayersPos({Planets,NewPlayers}, T).


detectPlayerCollisions(Players, []) ->
    Players;

detectPlayerCollisions(Players, [ Player| T ]) ->
    UpdatedPlayers = detectPlayerCollisions(Players, Player, maps:keys(Players)),
    detectPlayerCollisions(UpdatedPlayers, T).
detectPlayerCollisions(Players, Player, []) ->
    Players;
detectPlayerCollisions(Players, Player, [PPlayer | T ]) ->
    {PX, PY, PVx0, PVy0, PDiameter, PAngle,
     PR, PG, PB, PFuel,
     PWaitingGame, PInGame, PGameOver, PPid} = maps:get(PPlayer, Players),
    {X,Y,Vx0,Vy0,Diameter,Angle,
     R,G,B,Fuel,WaitingGame,InGame,GameOver,Pid} = maps:get(Player, Players),
    DistanceCenters = math:sqrt((PX-X)*(PX-X)+(PY-Y)*(PY-Y)),
    Collide = (Player /= PPlayer) andalso ((DistanceCenters =< Diameter) orelse (DistanceCenters =< PDiameter)),
    if
	(DistanceCenters > 0) andalso Collide->
	    Nx0 = PX - X,
	    Ny0 = PY - Y,
	    Distance = math:sqrt(Nx0 * Nx0 + Ny0 * Ny0),
	    Nx = Nx0 / Distance,
	    Ny = Ny0 / Distance,

	    Vn1 = Vx0 * Nx + Vy0 * Ny,
	    Vn2 = PVx0 * Nx + PVy0 * Ny,
	    
	    Vn1New = Vn2,
	    Vn2New = Vn1,

	    Vx1New = Vx0 + (Vn1New - Vn1) * Nx,
	    Vy1New = Vy0 + (Vn1New - Vn1) * Ny,
	    Vx2New = PVx0 + (Vn2New - Vn2) * Nx,
	    Vy2New = PVy0 + (Vn2New - Vn2) * Ny,

	    NewPlayer = {X, Y, Vx1New, Vy1New, Diameter, Angle,
			 R, G, B, Fuel,
			 WaitingGame, InGame, GameOver, Pid},
	    NewOtherPlayer = {PX, PY, Vx2New, Vy2New, PDiameter, PAngle,
			      PR, PG, PB, PFuel,
			      PWaitingGame, PInGame, PGameOver,PPid},
    
	    AccPlayers1 = maps:update(Player, NewPlayer, Players),
	    AccPlayers2 = maps:update(PPlayer, NewOtherPlayer, AccPlayers1);
	true ->
	    AccPlayers2 = Players
    end,
    detectPlayerCollisions(AccPlayers2, Player, T).
