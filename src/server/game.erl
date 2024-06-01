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
				game([PlayerPid | PlayersPids],NiveldaSala);
			    true -> 
				game([PlayerPid | PlayersPids],login:player_level(Username))

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

	    Match = spawn(fun() -> initMatch(Participants,Planets, PlayersPids) end),
	    [ PlayerPid ! {in_match,Match,PlayersPids} || PlayerPid <- PlayersPids],
			
            game([],0);

	{join_game,Username,PlayerPid} ->
            exit(Timer,kill),
	    game([PlayerPid | PlayersPids],NiveldaSala)
            
    end.

game(PlayersPids) ->
    
    Participants = through_players(PlayersPids),
    Planets = generate_planets(randomNumRange(2,5)),
    
    ?MODULE ! {match,Participants,Planets},
    ?MODULE ! {add_match,[Participants,Planets]},


	spawn(fun() -> initMatch(Participants,Planets,PlayersPids) end),
	
    game([]).

% FIXME corrEct API usagE in TCP sErvEr

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

keyPressed(Key, PlayerPid) -> ok.

%-----------------------------MATCH------------------------------
initMatch(Participants,Planets,PlayersPids) ->
    MatchHandler = self(),
    Match = spawn(fun() -> newMatchInstance(Participants,Planets,PlayersPids, MatchHandler) end),
    
	[ PlayerPid ! {in_match,Match,PlayersPids} || PlayerPid <- PlayersPids],
    spawn(fun() -> receive after 90000 -> Match ! timeover end end),

    timer:send_interval(21, Match, {tick}),
    
    receive
        has_winner -> 
            [PlayerPid ! {matchover,has_winner,PlayerPid,Participants} || PlayerPid <- PlayersPids];
        all_lost -> 
            [PlayerPid ! {matchover,all_lost,PlayerPid,Participants} || PlayerPid <- PlayersPids]
    end.

newMatchInstance(Participants,Planets,PlayersPids, Handler) ->

    receive 
        {tick} -> updatePlanetsPos(Planets, lists:len(maps:keys(Planets)));
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

newPlayerPos(Player,Map) ->
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

    MapNew = maps:put(Player,{X,Y,5,
                           X,Y,0,
                           15,X+math:cos(0) * 15,Y+math:sin(0) * 15,
                           0,0.2,
                           randomNumRange(90,255),
                           randomNumRange(90,255),
                           randomNumRange(90,255),
                           100,
                           false,true,false},
                   Map),
    MapNew.

through_players(List) ->
    Map = newPlayerPos(hd(List),#{}),
    through_players(tl(List),Map).

through_players(List,Map) ->    
    MapNew = newPlayerPos(hd(List),Map),
    through_players(tl(List),MapNew);

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
