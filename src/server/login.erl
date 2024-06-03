-module(login).
-export([start/0,
	 create_account/2,
	 close_account/1,
	 login/2,
	 logout/1,
	 online/0,
	 add_win/1,
	 add_loss/1,
	 player_level/1,
	 all_offline/0,
	 leaderboard/0]).

start() ->
    Map = read_json_from_file(),
		Empty = maps:new(),
    register(?MODULE, spawn(fun() -> loop(Map) end)),
    case Map==Empty of
			true ->
				ok;
			_ ->
				all_offline()
		end.
	

rpc(Request) ->
	?MODULE ! {Request, self()},
	receive {Result, ?MODULE} -> Result end.

create_account(User, Passwd) -> rpc({create_account,User,Passwd}).
close_account(User) -> rpc({close_account,User}).
login(User,Passwd) -> rpc({login,User, Passwd}).
logout(User) -> rpc({logout, User}).
online() -> rpc(online).
player_level(User) -> rpc({player_level, User}).
all_offline() -> rpc(all_offline).
add_win(User) -> rpc({add_win, User}).
add_loss(User) -> rpc({add_loss, User}).
leaderboard() -> rpc(leaderboard).
    
loop(State) ->
	receive
		{Request, From} ->
			{Result, NextState} = handle(Request, State),
			From ! {Result, ?MODULE},
			loop(NextState)
	end.

handle({create_account, User, Passwd}, Map) ->
    UserBin = list_to_binary(User),
    PasswdBin = list_to_binary(Passwd),
    UserKey = <<UserBin/binary>>,
    case maps:is_key(UserKey, Map) of
        false ->
            Passwd_str = <<PasswdBin/binary>>,
            NewMap = Map#{UserKey => #{<<"Passwd">> => Passwd_str, <<"online">> => <<"true">>, <<"level">> => <<"0">>, <<"wins">> => <<"0">>, <<"losses">> => <<"0">>}},
            write_json_to_file(NewMap),
            {ok, NewMap};
        true ->
            {user_exists, Map}
    end;

handle({close_account, User}, Map) ->
    UserBin = list_to_binary(User),
    User_str = <<UserBin/binary>>,
    case maps:find(User_str, Map) of
        {ok, _} ->
            NewMap = maps:remove(User_str, Map),
            write_json_to_file(NewMap),
            {ok, NewMap};
        _ ->
            {invalid, Map}
    end;

handle({login, User, Passwd}, Map) ->
    UserBin = list_to_binary(User),
    PasswdBin = list_to_binary(Passwd),
    User_str = <<UserBin/binary>>,
    Passwd_str = <<PasswdBin/binary>>,
    case maps:find(User_str, Map) of
        {ok, UserData} ->
            case maps:get(<<"Passwd">>, UserData) == Passwd_str of
                true ->
                    case maps:get(<<"online">>, UserData) of
                        <<"false">> ->
                            UpdatedUserData = maps:put(<<"online">>, <<"true">>, UserData),
                            NewMap = maps:put(User_str, UpdatedUserData, Map),
                            write_json_to_file(NewMap),
                            {ok, NewMap};
                        <<"true">> ->
                            {already_online, Map}
                    end;
                _ ->
                    {invalid, Map}
            end;
        error ->
            {invalid, Map}
    end;

handle({logout, User}, Map) ->
    UserBin = list_to_binary(User),
    User_str = <<UserBin/binary>>,
    case maps:find(User_str, Map) of
        {ok, UserData} ->
            case maps:get(<<"online">>, UserData) of
                <<"true">> ->
                    UpdatedUserData = maps:put(<<"online">>, <<"false">>, UserData),
                    NewMap = maps:put(User_str, UpdatedUserData, Map),
                    write_json_to_file(NewMap),
                    {ok, NewMap};
                _ ->
                    {op, Map}
            end;
        error ->
            {op, Map}
    end;

handle(online, Map) ->
    OnlineUsers = [Username || {Username, UserData} <- maps:to_list(Map),
                   maps:get(<<"online">>, UserData) == <<"true">>],
    {OnlineUsers, Map};

handle({player_level, User}, Map) ->
    UserBin = list_to_binary(User),
    User_str = <<UserBin/binary>>,
    case maps:find(User_str, Map) of
        {ok, UserData} ->
            Level = maps:get(<<"level">>, UserData),
            {Level, Map};
        error ->
            {invalid, Map}
    end;

handle(all_offline, Map) ->
    NewMap = maps:fold(
        fun(Key, UserData, Acc) ->
            UpdatedUserData = maps:put(<<"online">>, <<"false">>, UserData),
            maps:put(Key, UpdatedUserData, Acc)
        end,
        Map,
        Map
    ),
    write_json_to_file(NewMap),
    {ok, NewMap};
handle({add_win, User}, Map) ->
    UserBin = list_to_binary(User),
    User_str = <<UserBin/binary>>,
    case maps:find(User_str, Map) of
        {ok, UserData} ->
	    OldWins = list_to_integer(binary_to_list(maps:get(<<"win">>, UserData))),
	    UpdatedWins = list_to_binary(io_lib:format("~p", [1 + OldWins])),
	    UpdatedUserData = maps:put(<<"win">>, UpdatedWins, UserData),
	    UpdatedUserDataL = maps:put(<<"losses">>, <<"0">>, UpdatedUserData),
	    NewMap = maps:put(User_str, UpdatedUserDataL, Map),
	    
	    CurrLevel = list_to_integer(binary_to_list(maps:get(<<"level">>, UpdatedUserDataL))),
	    
	    case OldWins + 1 == CurrLevel of
		true  -> 
		    UpdatedUserDataLvl = maps:put(<<"level">>, list_to_binary(io_lib:format("~p", [1 + CurrLevel])), UpdatedUserDataL),
		    NewMap1 = maps:put(User_str, UpdatedUserDataLvl, NewMap);
		false -> NewMap1 = NewMap
	    end,
	    write_json_to_file(NewMap1),
	    {ok, NewMap};
	_ ->
	    {op, Map}
    end;
handle({add_loss, User}, Map) ->
    UserBin = list_to_binary(User),
    User_str = <<UserBin/binary>>,
    case maps:find(User_str, Map) of
        {ok, UserData} ->
	    OldLosses = list_to_integer(binary_to_list(maps:get(<<"losses">>, UserData))),
	    UpdatedLosses = list_to_binary(io_lib:format("~p", [1 + OldLosses])),
	    UpdatedUserData = maps:put(<<"losses">>, UpdatedLosses, UserData),
	    UpdatedUserDataW = maps:put(<<"wins">>, <<"0">>, UpdatedUserData),
	    NewMap = maps:put(User_str, UpdatedUserDataW, Map),
	    CurrLevel = list_to_integer(binary_to_list(maps:get(<<"level">>, UpdatedUserDataW))),
	    
	    case OldLosses == ceil(CurrLevel/2) of
		true  -> 
		    UpdatedUserDataLvl = maps:put(<<"level">>, list_to_binary(io_lib:format("~p", [1 + CurrLevel])), UpdatedUserDataW),
		    NewMap1 = maps:put(User_str, UpdatedUserDataLvl, NewMap);
		false -> NewMap1 = NewMap
	    end,

	    write_json_to_file(NewMap1),
	    {ok, NewMap};
	_ ->
	    {op, Map}
    end;    
handle(leaderboard, Map) ->
    OnlineUsers = [#{Username => #{<<"level">> => maps:get(<<"level">>, UserData),
				   <<"victories_in_row">> =>maps:get(<<"wins">>, UserData),
				   <<"losses_in_row">> => maps:get(<<"losses">>, UserData)}}
		   || {Username, UserData} <- maps:to_list(Map)],
    OnlineJson = jsx:encode(OnlineUsers),
    io:format("~p~n", [binary_to_list(<<OnlineJson/binary, "\n">>)]),
    {OnlineUsers, Map}.
write_json_to_file(Map) ->
    Json = jsx:encode(Map),
    {ok, File} = file:open("accounts.json", [write]),
    ok = file:write(File, Json),
    file:close(File).

read_json_from_file() ->
    case file:read_file("accounts.json") of
        {ok, Binary} ->
            CleanedBinary = clean_json(Binary),
            case jsx:decode(CleanedBinary, [return_maps]) of
                Json when is_map(Json) -> Json;
                _ -> maps:new()  % Handle decoding errors
            end;
        {error, enoent} ->  % File does not exist
            EmptyJson = <<"{}">>,
            file:write_file("accounts.json", EmptyJson),
            maps:new();
        {error, _Reason} ->  % Some other error
            maps:new()
    end.

clean_json(Binary) ->
    JsonString = binary_to_list(Binary),
    CleanedString = string:replace(JsonString, "\\\"", "", all),
    list_to_binary(CleanedString).
