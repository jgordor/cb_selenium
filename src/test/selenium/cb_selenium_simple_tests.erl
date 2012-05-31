%%%----------------------------------------------------------------------------
%%% @author Jose Luis Gordo Romero <jgordor@gmail.com>
%%% [http://www.freemindsystems.com]
%%% @copyright 2012 Jose luis Gordo Romero
%%% @doc Selenium Simple page tests
%%% @end
%%%----------------------------------------------------------------------------
-module(cb_selenium_simple_tests).
-include_lib("eunit/include/eunit.hrl").

%%%============================================================================
%%% API
%%%============================================================================

suite_test_()->
    Suite = 
    {foreach, local,
      fun setup/0,
      tests()
     },
    Suite.

tests() -> %{timeout, 60, fun start_session/0}
    [     
     {"Home Page",
      ?_test(home())},
     {"Home Page with Firefox",
      ?_test(home_firefox())}
    ].

%%--------------------------------------------------------------------
%% @doc home() Home Comercial Website
%%--------------------------------------------------------------------
home() -> 
    SSH = open_session(),
    {ok, no_content} = webdriver_remote:get(SSH, "http://localhost:8001/selenium"),
    {ok, Screen1} = webdriver_remote:screenshot(SSH),
    file:write_file("/home/jose.gordo/tmp/sel/screen3.png", base64:decode(Screen1)),
    {ok, [Id]} = webdriver_remote:find_elements(SSH, xpath, "//h2"),
    ?assertEqual({ok, <<"Dashboard">>}, webdriver_remote:text(SSH, Id)),
    ok.

%%--------------------------------------------------------------------
%% @doc home_firefox() Home Comercial Website
%%--------------------------------------------------------------------
home_firefox() -> 
    error_logger:info_msg("arrancamos...~n"),
    SSF = open_session(firefox),
    {error, []} = webdriver_remote:speed(SSF, 'SLOW'),
    {ok, no_content} = webdriver_remote:get(SSF, "http://localhost:8001/selenium"),
    {ok, Screen1} = webdriver_remote:screenshot(SSF),
    file:write_file("/home/jose.gordo/tmp/sel/screen1.png", base64:decode(Screen1)),
    {ok, [Id]} = webdriver_remote:find_elements(SSF, xpath, "//h2"),
    ?assertEqual({ok, <<"Dashboard">>}, webdriver_remote:text(SSF, Id)),
    {ok, no_content} = webdriver_remote:timeout(SSF, implicit_wait, 5000),
    timer:sleep(1000),
    {ok, [Element]} = webdriver_remote:find_elements(SSF, id, "first_link"),
    {ok, no_content} = webdriver_remote:click(SSF, Element),
    {ok, Screen2} = webdriver_remote:screenshot(SSF),
    file:write_file("/home/jose.gordo/tmp/sel/screen2.png", base64:decode(Screen2)),
    {ok, [Id2]} = webdriver_remote:find_elements(SSF, xpath, "//h2"),
    ?assertEqual({ok, <<"Lists">>}, webdriver_remote:text(SSF, Id2)),
    ok.

%% ===================================================================
%% Internal functions
%% ===================================================================

%%--------------------------------------------------------------------
%% @doc Setup each test set
%%--------------------------------------------------------------------
setup()->
    ok.

open_session() ->
    open_session(htmlunit).
open_session(htmlunit) ->
    {ok, Session} = webdriver_remote:session(cb_sel:get_host(), cb_sel:get_port(),[{browserName, htmlunit}, {version, <<"">>}, {platform, 'ANY'}]),
    Session;
open_session(firefox) ->
    {ok, Session} = webdriver_remote:session(cb_sel:get_host(), cb_sel:get_port(), [{browserName, firefox}]),
    Session.