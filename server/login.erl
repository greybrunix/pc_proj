-module(login).
-export([start/0,
	create_account/2,
	close_account/2,
	login/2,
	logout/1,
	online/0]).


start() ->
	register(?MODULE, spawn(fun() -> loop(maps:new()) end)).

rpc(Request) ->
	?MODULE ! {Request, self()},
	receive {Result, ?MODULE} -> Result end.

create_account(User, Passwd) -> rpc({create_account,User,Passwd}).
close_account(User, Passwd) -> rpc({close_account,User,Passwd}).
login(User,Passwd) -> rpc({login,User, Passwd}).
logout(User) -> rpc({logout, User}).
online() -> rpc(online).

loop(State) ->
	receive
		{Request, From} ->
			{Result, NextState} = handle(Request, State),
			From ! {Result, ?MODULE},
			loop(NextState)
	end.

handle({create_account, User, Passwd}, Map) ->
	case maps:is_key(User, Map) of
		false ->
			{ok, Map#{User => {Passwd, true}}};
		true ->
			{user_exists, Map}
	end;
handle({close_account, User, Passwd}, Map) ->
	case maps:find(User, Map) of
		{ok, {P,_}} when P =:= Passwd ->
			{ok,maps:remove(User, Map)};
		_ ->
			{invalid, Map}
	end;
handle({login, User, Passwd}, Map) ->
	case maps:find(User, Map) of
		{ok, {P,false}} when P =:= Passwd ->
			{ok,maps:update(User, {Passwd, true})};
		{ok, {P, true}} when P =:= Passwd ->
			{ok, Map};
		_ ->
			{invalid, Map}
	end;
handle({logout, User}, Map) ->
	case maps:find(User, Map) of
		{ok, {Pass,true}} ->
			{ok,maps:update(User, {Pass, false}, Map)};
		_ ->
			{op,Map}
	end;
handle(online, Map) ->
	{[Username || {Username,{_,true}} <- maps:to_list(Map)], Map}.
