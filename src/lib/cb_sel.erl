-module(cb_sel).
-include_lib("eunit/include/eunit.hrl").
-export([start_selenium_server/0,
         get_host/0,
		 get_port/0,
		 get_browsers/0,
         setup_session/1,
         close_session/1,
         assert_command_ok/2,
         assert_command/3
		]).
-define(MAX_SERVER_START_RETRIES, 20).

start_selenium_server() ->
    error_logger:tty(false),
    gen_event:delete_handler(error_logger,sasl_report_tty_h,swap),
    Path = boss_env:get_env(cb_selenium, path, "../cb_selenium"),
    Log = boss_env:get_env(cb_selenium, selenium_server_log_file, "/tmp/selenium_server.log"),
    Debug = case boss_env:get_env(cb_selenium, debug, false) of
                true -> " -debug";
                false -> ""
            end,
    StartCmd = "cd " ++ Path ++ "/priv/selenium; java -jar selenium-server-standalone-2.30.0.jar -browserSessionReuse -log " ++ Log ++ Debug ++ " > /dev/null 2>& 1 &",
    StartOutput = os:cmd(StartCmd),
    test_selenium_server(?MAX_SERVER_START_RETRIES).

test_selenium_server(0) -> 
    io:format("[FATAL] - Connection to selenium server failed.~n"),
    halt(1);
test_selenium_server(Retries) ->
    %% Try to connect
    case setup_session(htmlunit) of
        {error,econnrefused} ->
            io:format("."),
            timer:sleep(500),
            test_selenium_server(Retries - 1);
        Session ->
            io:format("[OK] - Connection to selenium server established.~n"),
            close_session(Session);
        _ ->
            io:format("[FATAL] - Unhandled error.~n"),
            halt(1)
    end.

get_host() ->
	boss_env:get_env(cb_selenium, host, "localhost").

get_port() ->
	boss_env:get_env(cb_selenium, port, 4444).

get_browsers() ->
	boss_env:get_env(cb_selenium, webdriver_browsers, [htmlunit, firefox]).
    
setup_session(Browser) ->
    case webdriver_remote:session(cb_sel:get_host(), cb_sel:get_port(),
                                  [{browserName, Browser},
                                   {javascriptEnabled, true},
                                   {version, <<"">>},
                                   {platform, 'ANY'}]) of
        {ok, Session} ->
            io:format("~nsetup_session_OK.~n"),
            %{ok,null} = webdriver_remote:execute(Session, "window.resizeTo(1920,1080);"),
            %% @TODO: make speed boss.config configurable
            %%{error, []} = webdriver_remote:speed(Session, 'SLOW'),
            Session;
        {error,econnrefused} -> 
            io:format("~nsetup_session_econnrefused.~n"),
            {error,econnrefused};
        _ -> 
            io:format("~nsetup_session_CAGADA.~n"),
            {error, unhandled_error}
    end.

close_session(Session) ->  
    {ok, no_content} = webdriver_remote:quit(Session).

assert_command_ok(Name, Result) ->
    case Result of
        {ok, _} -> 
            ok;
        _ ->
            case Result of
                {error, {13, Message}} = E ->
                    case proplists:get_value(<<"screen">>, Message) of
                        undefined ->
                            ok;
                        Screen ->
                            file:write_file("/tmp/sel_" ++ Name ++ ".png", base64:decode(Screen))
                    end,
                    exit(E);
                X -> ?assertEqual({ok, any}, X)
            end
    end.

assert_command(Name, Expected, Result) ->
    case Result of
        Expected -> 
            ok;
        {ok, _Other} -> 
            exit({bad_result, Expected, Result});
        {error, {13, Message}} = E ->
            case proplists:get_value(<<"screen">>, Message) of
                undefined ->
                    ok;
                Screen ->
                    file:write_file("/tmp/sel_" ++ Name ++ ".png", base64:decode(Screen))
            end,
            exit(E);
        X -> X
    end.