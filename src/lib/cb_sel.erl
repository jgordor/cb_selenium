-module(cb_sel).
-export([get_host/0,
		 get_port/0
		]).

get_host() ->
	boss_env:get_env(cb_selenium, host, "localhost").

get_port() ->
	boss_env:get_env(cb_selenium, port, 4444).


go() ->
	{ok, Session} = webdriver_remote:session(cb_sel:get_host(), cb_sel:get_port(),[{browserName, htmlunit}, {version, <<"">>}, {platform, 'ANY'}]),