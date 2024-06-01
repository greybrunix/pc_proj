-module(game).
-export([start/0,
	     join_game/1,
	     del_game/0,
	     stop_game/0,
         match/1
        ]).


start() ->
	spawn(fun() -> game([]) end).


game(PlayersPids,NiveldaSala) when length(PlayersPids) < 2 ->
	receive
		{join_game,Username,PlayerPid} ->
            case length(PlayersPids) > 0 of
                true -> 
                    NivelPlayer = login:player_level(Username),
                    case NivelPlayer+1 == NiveldaSala or NivelPlayer-1 == NiveldaSala of
                        true -> 
                            game([PlayerPid | PlayersPids],NiveldaSala)
                        false ->
                            game([PlayerPid],NivelPlayer)

                false->
			        game([PlayerPid | PlayersPids],login:player_level(Username))

            end,
	end;

game(PlayersPids,NiveldaSala) when ((length(PlayersPids) >= 2) and (length(PlayersPids) < 4)) ->
	Game = self(),
	Timer = spawn(fun() -> receive after 5000 -> Game ! timeout end end),
	receive
		timeout ->
            
            Participants = through_players(PlayersPids),
            Planets = generate_planets(randomNumRange(2,5)),

			Match = spawn(fun() -> initMatch(Participants,Planets) end),
			[ PlayerPid ! {in_match,Match,PlayersPids} || PlayerPid <- PlayersPids],
			
            game([],0);

		{join_game,Username,PlayerPid} ->
            exit(Timer,kill),
			game([PlayerPid | PlayersPids],NiveldaSala)
            
	end;

game(PlayersPids) ->
    
    Participants = through_players(PlayersPids),
    Planets = generate_planets(randomNumRange(2,5)),

	spawn(fun() -> initMatch(Participants,Planets,PlayersPids) end),
	
    game([]).

% FIXME corrEct API usagE in TCP sErvEr

join_game(PlayerPid,Username) ->
    ?MODULE ! {join_game,Username,PlayerPid},
    receive
    end.

keyPressed(Key, PlayerPid) -> ok.

    
del_game() ->
	ok.
stop_game() ->
	ok.

%-----------------------------MATCH------------------------------
initMatch(Participants,Planets,PlayersPids) ->
        
    Match = spawn(fun() -> newMatchInstance(Participants,Planets) end),

    spawn(fun() -> receive after 90000 -> Match ! timeover end end),

    timer:send_interval(21, Match, {tick}),
    
    receive
        has_winner -> 
            [PlayerPid ! {matchover,has_winner,PlayerPid,Participants} || PlayerPid <- PlayersPids]
        all_lost -> 
            PlayersPids = maps:keys(Participants), 
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

    {ok,NextPlayers};


%-----------------------FUNCOES AUXILIARES----------------------
randomNumRange(Small,Big) ->
    % É preciso fazer seed no incio
    rand:uniform(Big - Small + 1) + Small - 1.

generate_planets(Int, Sistema) -> 
    Sistema = #{0 => {0,960,540,35,255,255,0}}, % here comes the sun
    generate_planets(Int, Sistema);

generate_planets(Int,Sistema) ->
    case Int of
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
                                    randomNumRange(90,255)}, Sistema),
            generate_planets(Int-1,Sistema)
    end.

newPlayerPos(Player,Map) ->
    X = randomNumRange(300,1600),
    Y = randomNumRange(200,850),
    if
        X >= 960 -> X = 1750;
        X < 960 -> X = 150
    end,

    if
        Y >= 540 -> Y = 900;
        Y < 540 -> Y = 100
    end,

    Map = maps:put(Player,{X,Y,5,
                           X,Y,0,
                           15,X+math:cos(0) * 15,Y+math:sin(0) * 15,
                           0,0.2,
                           randomNumRange(90,255),
                           randomNumRange(90,255),
                           randomNumRange(90,255),
                           100,
                           false,true,false},
                   Map),
    Map.

through_players(List, Map) ->
    Map = maps:new(),
    Map = newPlayerPos(hd(List),Map),
    through_players(tl(List),Map);

through_players(List,Map) when length(List) > 0 ->    
    Map = newPlayerPos(hd(List),Map),
    through_players(tl(List),Map);

through_players([],Map) -> 
    Map.

updatePlanetsPos(Planets,NumPlanet) ->

    case NumPlanet>0 of

        false ->
            %TODO calculos dos planetas por tick

            updatePlanetsPos(Planets,NumPlanet-1);

        true -> Planets

    end.



