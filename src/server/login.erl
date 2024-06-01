-module(login).
-export([start/0,
	create_account/2,
	close_account/2,
	login/2,
	logout/1,
	online/0,
    player_level/1,
    all_offline/0]).

start() ->
    Map = read_json_from_file(),
    register(?MODULE, spawn(fun() -> loop(Map) end)).

rpc(Request) ->
	?MODULE ! {Request, self()},
	receive {Result, ?MODULE} -> Result end.

create_account(User, Passwd) -> rpc({create_account,User,Passwd}).
close_account(User, Passwd) -> rpc({close_account,User,Passwd}).
login(User,Passwd) -> rpc({login,User, Passwd}).
logout(User) -> rpc({logout, User}).
online() -> rpc(online).
player_level(User) -> rpc({player_level, User}).
all_offline() -> rpc(all_offline).

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
    case maps:is_key(<<UserBin/binary>>, Map) of
        false ->
            User_str = << "\"", UserBin/binary, "\"" >>,
            Passwd_str = << "\"", PasswdBin/binary, "\"" >>,
            NewMap = Map#{User_str => #{<<"Passwd">> => Passwd_str, <<"online">> => <<"true">>, <<"level">> => <<"0">>, <<"wins">> => <<"0">>, <<"losses">> => <<"0">>}},
            write_json_to_file(NewMap),
            {ok, NewMap};
        true ->
            {user_exists, Map}
    end;

handle({close_account, User, Passwd}, Map) ->
    UserBin = list_to_binary(User),
    PasswdBin = list_to_binary(Passwd),
    User_str = << "\"", UserBin/binary, "\"" >>,
    Passwd_str = << "\"", PasswdBin/binary, "\"" >>,
    case maps:find(User_str, Map) of
        {ok, UserData} ->
            case maps:get(<<"Passwd">>, UserData) of
                Passwd_str ->
                    NewMap = maps:remove(User_str, Map),
                    write_json_to_file(NewMap),
                    {ok, NewMap};
                _ ->
                    {invalid, Map}
            end;
        error ->
            {invalid, Map}
    end;

handle({login, User, Passwd}, Map) ->
    UserBin = list_to_binary(User),
    PasswdBin = list_to_binary(Passwd),
    User_str = << "\"", UserBin/binary, "\"" >>,
    Passwd_str = << "\"", PasswdBin/binary, "\"" >>,
    case maps:find(User_str, Map) of
        {ok, UserData} ->
            case maps:get(<<"Passwd">>, UserData) of
                Passwd_str ->
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
    User_str = << "\"", UserBin/binary, "\"" >>,
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
    User_str = << "\"", UserBin/binary, "\"" >>,
    case maps:find(User_str, Map) of
        {ok, UserData} ->
            Level = maps:get(<<"level">>, UserData),
            {Level, Map};
        error ->
            {invalid}
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
    {ok, NewMap}.

write_json_to_file(Map) ->
    Json = jsx:encode(Map),
    {ok, File} = file:open("accounts.json", [write]),
    ok = file:write(File, Json),
    file:close(File).

read_json_from_file() ->
    case file:read_file("accounts.json") of
        {ok, Binary} ->
            jsx:decode(Binary, [return_maps]);
        {error} ->
            maps:new()
    end.
