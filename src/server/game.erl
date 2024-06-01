-module(game).
-export([start/0,
	 join_game/2,
	 key_pressed/2,
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
            io:format("adiciona ao game ~p~n", [Username]),
            game([[[{PlayerPid,Username}],login:player_level(Username)]])
    end;

game([[Participants,NiveldaSala]|Salas]) ->

    Game = self(),
    Size = length(Participants),
    case ((Size>1) and (Size < 4)) of
        true ->
            Timer = spawn(fun() -> receive after 5000 -> Game ! {timeout}, io:format("Timeout~n") end end);
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

			    ?MODULE ! {add_match, [ParticipantsMap, Planets]},

			    spawn(fun() -> initMatch(ParticipantsMap, Planets) end),

			    [PlayerPid ! {match, ParticipantsMap, Planets} || {PlayerPid, _} <- Participants],

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
            io:format("TimeoutRecebido~n"), 
            ParticipantsMap = through_players(Participants),
            io:format("TimeoutParticipants~n"), 
            Planets = generate_planets(randomNumRange(2,5)),
            io:format("TimeoutPlanets~n"), 
            
            ?MODULE ! {add_match,[ParticipantsMap,Planets]},
	        
            io:format("Timeout~p~n", [ParticipantsMap]), 
            spawn(fun() -> initMatch(ParticipantsMap,Planets) end),
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
key_pressed(Key, Username) -> ok.
    
%-----------------------------MATCH------------------------------

getPid(Value) ->
    {_,_,_,_,
     _,_,_,_,
     _,_,_,Pid}= Value,
    Pid.


initMatch(Players, Planets) ->

    TestRemaining = length(maps:keys(maps:filter(fun(_Key, Value) ->
			     {_, _, _, _, _, _, _, _, _, _, _, InGame, _} = Value,
			     InGame
		     end, Players))),
    TestWinner    = length(maps:keys(maps:filter(fun(_Key, Value) ->
			   {_, _, _, _, _, _, _, _, _, _, _, InGame, GameOver} = Value,
			   GameOver and InGame
		   end, Players))),
    Values = maps:values(Players),
    Pids = [getPid(Value) || Value  <- Values],
    if (TestRemaining == 0 orelse (TestWinner == 1)) -> ?MODULE ! {remove, [Players, Planets]}
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
		updatePlayersPos({Planets,Players},
				lists:len(maps:keys(Planets))),
	    _ = [detectPlayerCollisions(Players, Player) || Player <- maps:to_list(Players)],
	    [Pid ! {update_data, [Players, Planets]} || Pid <- Pids],
	    initMatch(NewPlayers, NewPlanets)

    end.

%----------------------------HANDLES----------------------------
handle({"UP", Pid},PlayersInfo) ->
    {X,Y,Vx0,Vy0,Diameter,Angle,
     R,G,B,Fuel,
     WaitingGame,InGame,GameOver, Pid}= maps:get(Pid,PlayersInfo),
    Vx = Vx0 + 0.5 * math:cos((Angle * math:pi()) / 180), % Apply linear acceleration over Angle direction
    Vy = Vy0 + 0.5 * math:sin((Angle * math:pi()) / 180), % Apply linear acceleration over Angle direction
    NewInfo = {X,Y,Vx,Vy,Diameter,Angle,
	       R,G,B,Fuel,WaitingGame,InGame,GameOver},
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),
    
    {ok,NextPlayers};

handle({"LEFT", Pid},PlayersInfo) ->
    {X,Y,Vx0, Vy0,Diameter,Angle0,
     R,G,B,Fuel,
     WaitingGame,InGame,GameOver}= maps:get(Pid,PlayersInfo),
    Angle = (Angle0 + 10) rem 360,
    Vx = Vx0 * math:cos((Angle * math:pi()) / 180) - Vy0 * math:sin((Angle * math:pi()) / 180),
    Vy = Vx0 * math:sin((Angle * math:pi()) / 180) + Vy0 * math:cos((Angle * math:pi()) / 180),
    NewInfo = {X,Y,Vx,Vy,Diameter,Angle,
	       R,G,B,Fuel,WaitingGame,InGame,GameOver},
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),
    
    {ok,NextPlayers};

handle({"RIGHT", Pid},PlayersInfo) ->
    {X,Y,Vx0, Vy0, Diameter,Angle0,
     R,G,B,Fuel,
     WaitingGame,InGame,GameOver}= maps:get(Pid,PlayersInfo),
    Angle = (Angle0 - 10) rem 360, % Compute new angle in clockwise direction
    Vx = Vx0 * math:cos((Angle * math:pi()) / 180) - Vy0 * math:sin((Angle * math:pi()) / 180),
    Vy = Vx0 * math:sin((Angle * math:pi()) / 180) + Vy0 * math:cos((Angle * math:pi()) / 180),

    NewInfo = {X,Y,Vx,Vy,Diameter,Angle,
	       R,G,B,Fuel-math:sqrt(Vx*Vx+Vy*Vy),WaitingGame,InGame,GameOver},
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),

    {ok,NextPlayers}.


%-----------------------FUNCOES AUXILIARES----------------------
randomNumRange(Small,Big) ->
    % É preciso fazer seed no incio
    rand:uniform(Big - Small + 1) + Small - 1.

generate_planets(Int) -> 
    io:format("PlanetaInt~n"), 
    Sistema = #{0 => {0,960,540,35,255,255,0}}, % here comes the sun
    generate_planets(Int, Sistema).

generate_planets(0, Sistema) ->
    io:format("PlanetaZeroSistema~n"), 
    Sistema;
generate_planets(Int,Sistema) ->
    io:format("PlanetaIntSistema~n"), 
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
    generate_planets(Int-1,SistemaNovo).

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

updatePlayersPos({_Planets,Players}, []) ->
    Players;
updatePlayersPos({Planets, Players}, [Player | T]) ->
    {X0,Y0, Vx0, Vy0, Diameter, Angle, R,G,B,Fuel,
     WaitingGame,InGame_,GameOver_} = maps:get(Player, Players),
    X = X0 + Vx0*0.0021,
    Y = Y0 + Vy0*0.0021,
    Vx = Vx0,
    Vy = Vy0,

    Collided = lists:any(fun({_, {_, PX, PY, _, _, PDiameter, _, _, _}}) ->
        math:sqrt((X - PX) * (X - PX) + (Y - PY) * (Y - PY)) < (Diameter / 2 + PDiameter / 2)
    end, maps:to_list(Planets)),
    
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
		 InGame,GameOver},
    NewPlayers = maps:update(Player, NewPlayer, Players),
    updatePlayersPos({Planets,NewPlayers}, T).



detectPlayerCollisions(Players, Player) ->
    {X, Y, Vx0, Vy0, Diameter, Angle,
     R, G, B, Fuel,
     WaitingGame, InGame, GameOver} = maps:get(Player, Players),
    
    CollidedPlayers = lists:filter(fun({_, {PX, PY, _, _, PDiameter, _, _, _, _, _, _, _, _}}) ->
        math:sqrt((X - PX) * (X - PX) + (Y - PY) * (Y - PY)) < (Diameter / 2 + PDiameter / 2)
    end, maps:to_list(maps:remove(Player, Players))),
    
    UpdatedPlayers = lists:foldl(fun({OtherPlayer, {PX, PY, PVx0, PVy0, PDiameter, PAngle,
                                                    PR, PG, PB, PFuel,
                                                    PWaitingGame, PInGame, PGameOver}}, AccPlayers) ->
						% Calculate the normal vector
					 Nx0 = PX - X,
					 Ny0 = PY - Y,
					 Distance = math:sqrt(Nx0 * Nx0 + Ny0 * Ny0),
					 Nx = Nx0 / Distance,
					 Ny = Ny0 / Distance,

						% Calculate the velocities along the normal
					 Vn1 = Vx0 * Nx + Vy0 * Ny,
					 Vn2 = PVx0 * Nx + PVy0 * Ny,
					 
						% Exchange the normal velocities
					 Vn1New = Vn2,
					 Vn2New = Vn1,

						% Calculate the new velocities
					 Vx1New = Vx0 + (Vn1New - Vn1) * Nx,
					 Vy1New = Vy0 + (Vn1New - Vn1) * Ny,
					 Vx2New = PVx0 + (Vn2New - Vn2) * Nx,
					 Vy2New = PVy0 + (Vn2New - Vn2) * Ny,

						% Update both players
					 NewPlayer = {X, Y, Vx1New, Vy1New, Diameter, Angle,
						      R, G, B, Fuel,
						      WaitingGame, InGame, GameOver},
					 NewOtherPlayer = {PX, PY, Vx2New, Vy2New, PDiameter, PAngle,
							   PR, PG, PB, PFuel,
							   PWaitingGame, PInGame, PGameOver},
					 
					 AccPlayers1 = maps:update(Player, NewPlayer, AccPlayers),
					 AccPlayers2 = maps:update(OtherPlayer, NewOtherPlayer, AccPlayers1),
					 AccPlayers2
				 end, Players, CollidedPlayers),

    UpdatedPlayers.
