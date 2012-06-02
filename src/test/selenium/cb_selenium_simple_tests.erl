%%%----------------------------------------------------------------------------
%%% @author Jose Luis Gordo Romero <jgordor@gmail.com>
%%% [http://www.freemindsystems.com]
%%% @copyright 2012 Jose luis Gordo Romero
%%% @doc Selenium Simple page tests
%%% @end
%%%----------------------------------------------------------------------------
-module(cb_selenium_simple_tests).
-include_lib("eunit/include/eunit.hrl").
-export([dashboard/2]).

%%%============================================================================
%%% API
%%%============================================================================

suite_test_()->
    [{setup,
      fun() -> setup_session(B) end, 
      fun close_session/1,
      api_tests(B)} || B <- cb_sel:get_browsers()].

api_tests(Browser) ->
    Tests = [
	     dashboard
	    ],
    fun(X) ->
	    [{timeout, 120, 
	      {lists:flatten(io_lib:format("~s with ~s",[T,Browser])), fun() -> ?MODULE:T(Browser,X) end } 
	     } || T <- Tests]
    end.

%%--------------------------------------------------------------------
%% @doc dashboard() Dashboard
%%--------------------------------------------------------------------
dashboard(_B, S) -> 
    {ok, no_content} = webdriver_remote:get(S, "http://localhost:8001/selenium"),
    {ok, [Id]} = webdriver_remote:find_elements(S, xpath, "//h2"),
    ?assertEqual({ok, <<"Dashboard">>}, webdriver_remote:text(S, Id)),
	{ok, [Element]} = webdriver_remote:find_elements(S, id, "first_link"),
	{ok, no_content} = webdriver_remote:click(S, Element),
	{ok, [Id2]} = webdriver_remote:find_elements(S, xpath, "//h2"),
	?assertEqual({ok, <<"Lists">>}, webdriver_remote:text(S, Id2)),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Setup each test set
%%--------------------------------------------------------------------

setup()->
    ok.

setup_session(Browser) ->
    {ok, Session} = webdriver_remote:session(cb_sel:get_host(),
											 cb_sel:get_port(),
											 [{browserName, Browser}, 
											  {javascriptEnabled, true},
											  {version, <<"">>}, 
											  {platform, 'ANY'}]
											),
	Session.

close_session(Session) ->    
	{ok, no_content} = webdriver_remote:quit(Session).