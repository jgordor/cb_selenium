%%%-------------------------------------------------------------------
%%% @author Jose Luis Gordo Romero <jgordor@gmail.com>
%%% [http://www.freemindsystems.com]
%%% @copyright 2012 Free Mind Systems
%%% @doc Dashboard Controller.
%%%      Useful data
%%% @end
%%%-------------------------------------------------------------------
-module(cb_selenium_dashboard_controller, [Req]).

%% API
-export([index/2]).

%% API_BOSS
-default_action(index).

%%%===================================================================
%%% API
%%%===================================================================

index('GET', []) ->
    {ok, []}.