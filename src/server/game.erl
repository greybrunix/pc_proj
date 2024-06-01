-module(game).
-export([start/0,
	     join_game/1,
	     del_game/0,
	     stop_game/0
        ]).


start() ->
	spawn(fun() -> game([]) end).


game(PlayersPids) when length(PlayersPids) < 2 ->
	receive
		{join_game,PlayerPid} ->
            ?MODULE ! {start},
			game([PlayerPid | PlayersPids])
	end;

game(PlayersPids) when ((length(PlayersPids) >= 2) and (length(PlayersPids) < 4)) ->
	Game = self(),
	Timer = spawn(fun() -> receive after 5000 -> Game ! timeout end end),
	receive
		timeout ->
            
		Participants = through_players(PlayersPids),
		Planets = generate_planets(randomNumRange(2,5)),

		Match = spawn(fun() -> initMatch(Participants,Planets, PlayersPids) end),
		[ PlayerPid ! {in_match,Match,PlayersPids} || PlayerPid <- PlayersPids],
			
		game([]);

	    {join_game,PlayerPid} ->
		exit(Timer,kill),
		?MODULE ! {start},
		game([PlayerPid | PlayersPids])
            
	end;

game(PlayersPids) ->
    
    Participants = through_players(PlayersPids),
    Planets = generate_planets(randomNumRange(2,5)),

    spawn(fun() -> initMatch(Participants,Planets,PlayersPids) end),
	
    game([]).

% FIXME corrEct API usagE in TCP sErvEr

join_game(PlayerPid) ->
    ?MODULE ! {join_game, PlayerPid},
    receive
        {start} -> joined
    end.

keyPressed(Key, PlayerPid) -> ok.

    
del_game() ->
	ok.
stop_game() ->
	ok.

%-----------------------------MATCH------------------------------
initMatch(Participants,Planets,PlayersPids) ->
        
    Match = spawn(fun() -> newMatchInstance(Participants,Planets,PlayersPids) end),
    
    [ PlayerPid ! {in_match,Match,PlayersPids} || PlayerPid <- PlayersPids],
    spawn(fun() -> receive after 90000 -> Match ! timeover end end),

    timer:send_interval(21, Match, {tick}),
    
    receive
        has_winner -> 
            [PlayerPid ! {matchover,has_winner,PlayerPid,Participants} || PlayerPid <- PlayersPids];
        all_lost -> 
            [PlayerPid ! {matchover,all_lost,PlayerPid,Participants} || PlayerPid <- PlayersPids]
        
    end.

newMatchInstance(Participants,Planets,PlayersPids) ->

    receive 
        {tick} -> ok;
            % aqui aplicar calculo tendo em conta  gravidade

        {pressed,Key} -> ok;

        
        timeover ->
            [PlayerPid ! {matchover,timeover,PlayerPid,Participants} || PlayerPid <- PlayersPids]
    end.

%----------------------------HANDLES----------------------------

handle({"UP", Pid},PlayersInfo) ->
    {X,Y,Diameter,TargetX,TargetY,Angle,
     LineLength,LineEndX,LineEndY,TargetAngle,
     EasingAngle,R,G,B,Fuel,
     WaitingGame,InGame,GameOver}= maps:get(Pid,PlayersInfo),

    NewInfo = 0,
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),
    
    {ok,NextPlayers};

handle({"LEFT", Pid},PlayersInfo) ->
    {X,Y,Diameter,TargetX,TargetY,Angle,
     LineLength,LineEndX,LineEndY,TargetAngle,
     EasingAngle,R,G,B,Fuel,
     WaitingGame,InGame,GameOver}= maps:get(Pid,PlayersInfo),

    NewInfo = 0,
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),
    
    {ok,NextPlayers};

handle({"RIGHT", Pid},PlayersInfo) ->
    {X,Y,Diameter,TargetX,TargetY,Angle,
     LineLength,LineEndX,LineEndY,TargetAngle,
     EasingAngle,R,G,B,Fuel,
     WaitingGame,InGame,GameOver}= maps:get(Pid,PlayersInfo),

    NewInfo = 0,
    NextPlayers = maps:update(PlayersInfo,NewInfo,PlayersInfo),

    {ok,NextPlayers}.


%-----------------------FUNCOES AUXILIARES----------------------
randomNumRange(Small,Big) ->
    rand:uniform(Big - Small + 1) + Small - 1.

generate_planets(Int) -> 
    Sistema = #{0 => {0,960,540,35,255,255,0}}, % here comes the sun
    generate_planets(Int, Sistema).

generate_planets(0, Sistema) ->
    Sistema;
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
    generate_planets(Int-1,SistemaNovo).

newPlayerPos(Player,Map) ->
    X_0 = randomNumRange(300,1600),
    Y_0 = randomNumRange(200,850),
    if
        X_0 >= 960 -> X = 1750;
        X_0 < 960 -> X = 150
    end,

    if
        Y_0 >= 540 -> Y = 900;
        Y_0 < 540 -> Y = 100
    end,

    MapNew = Map#{Player => {X,Y,5,
                           X,Y,0,
                           15,X+math:cos(0) * 15,Y+math:sin(0) * 15,
                           0,0.2,
                           randomNumRange(90,255),
                           randomNumRange(90,255),
                           randomNumRange(90,255),
                           100,
                           false,true,false
			    }},
    MapNew.

through_players([H | T]) ->
    Map = newPlayerPos(H,#{}),
    through_players(T,Map).

through_players([ H | T ],Map) ->    
    MapNew = newPlayerPos(H,Map),
    through_players(T,MapNew);

through_players([],Map) -> 
    Map.

updatePlanetsPos(Planets, 0) ->
    Planets;
updatePlanetsPos(Planets,NumPlanet) ->
%TODO calculos dos planetas por tick
    {Dist, X0, Y0, Vx0, Vy0, Diameter, R, G, B} = maps:get(Planets, NumPlanet),
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


