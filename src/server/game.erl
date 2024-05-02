-module(game).
-export([setup_game/0, del_game/0,
	wait_game/0, stop_wait/0]).

setup_game() ->
	ok.
del_game() ->
	ok.
wait_game() ->
	start_game().
start_game() ->
	ok.
stop_wait() ->
	stop_game().
stop_game() ->
	ok.

