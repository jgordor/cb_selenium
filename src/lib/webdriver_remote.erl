%%% Copyright(c) 2011 Nicolas Charpentier
%%% All rights reserved.
%%% See file $TOP_DIR/COPYING.

%% @author Nicolas Charpentier <open_source@charpi.net> [http://charpi.net]
%% @copyright 2011 Nicolas Charpentier
-module(webdriver_remote).

-type(abstract_session() :: {string(), integer(), any()}).
-type(command_result() :: {ok, term()}).

-type(capabilities() :: [{browserName, firefox|internet_explorer|htmlunit|iphone|chrome} |
			 {version, string()} |
			 {javascriptEnabled, true|false} |
			 {platform, 'WINDOWS'|'XP'|'VISTA'|'MAC'|'LINUX'|'UNIX'|'ANY'}]).

-export([session/3]).
-export([session/1]).
-export([quit/1]).
-export([get/2]).
-export([get_current_url/1]).
-export([timeout/3]).
-export([get_window_handle/1]).
-export([get_window_handles/1]).
-export([forward/1]).
-export([back/1]).
-export([refresh/1]).
-export([execute/2]).
-export([screenshot/1]).
-export([available_engines/1]).
-export([active_engine/1]).
-export([frame/2]).
-export([find_elements/3]).
-export([find_elements/4]).
-export([get_attribute/3]).
-export([click/2]).
-export([value/2]).
-export([value/3]).
-export([submit/2]).
-export([text/2]).
-export([title/1]).
-export([switch_to_window/2]).
-export([delete_window/1]).
-export([speed/1]).
-export([speed/2]).
-export([cookies/1]).
-export([add_cookie/4]).
-export([delete_cookie/2]).
-export([delete_cookies/1]).
-export([source/1]).
-export([active_element/1]).
-export([double_click/1]).
-export([button_up/1]).
-export([button_down/1]).
-export([mouse_click/1, mouse_click/2]).
-export([window_resize/3]).
-export([accept_alert/1]).
-export([dismiss_alert/1]).
-export([within_frame/3]).

-define(CONTENT_TYPE,"application/json;charset=UTF-8").

-spec session(string(), integer(), capabilities()) -> {ok, abstract_session()}.
session(Host, Port, Capabilities) ->
    application:start(inets),
    Result = post(path({Host,Port,undefined}), to_json([{desiredCapabilities, {struct, Capabilities}}])),
    case Result of
    	{ok, {struct, H}} ->
    	    Id = binary_to_list(proplists:get_value(<<"webdriver.remote.sessionid">>, H)),
    	    {ok, {Host, Port, Id}};
    	E ->
            E
    end.

session(Session) ->
    request(get, path(Session), []).

-spec quit(abstract_session()) -> command_result().
quit(Session) ->
    request(delete, path(Session), []).
    
-spec get(abstract_session(), string() | binary()) -> command_result().
get(Session, Destination) when is_list(Destination) ->
    get(Session, list_to_binary(Destination));
get(Session, Destination) when is_binary(Destination) ->
    post(path(Session, "url"), to_json([{url, Destination}])).

-spec get_current_url(abstract_session()) -> command_result().
get_current_url(Session) ->
    request(get, path(Session, "url"), []).

-spec(timeout(abstract_session(), async_script | implicit_wait, integer()) ->
	     command_result()).
timeout(Session, Timeout, Delay) when Timeout == async_script;
				      Timeout == implicit_wait,
				      is_integer(Delay) ->
    Body = to_json([{ms, Delay}]),
    post(path(Session,"timeouts/" ++ atom_to_list(Timeout)), Body).

-spec(get_window_handle(abstract_session()) -> command_result()).
get_window_handle(Session) ->
    request(get, path(Session, "window_handle"), []).
	     
-spec(get_window_handles(abstract_session()) -> command_result()).
get_window_handles(Session) ->
    request(get, path(Session, "window_handles"), []).

-spec(forward(abstract_session()) -> command_result()).
forward(Session) ->
    post(path(Session,"forward"), " ").

-spec(back(abstract_session()) -> command_result()).
back(Session) ->
    post(path(Session,"back"), " ").
    
-spec(refresh(abstract_session()) -> command_result()).
refresh(Session) ->
    post(path(Session, "refresh") , " ").

-spec(execute(abstract_session(), string()) -> command_result()).	     
execute(Session, Script) ->    
    post(path(Session, "execute"), 
 	 to_json([{<<"script">>, list_to_binary(Script)},
		  {<<"args">>, []}])).

-spec(screenshot(abstract_session()) -> command_result()).
screenshot(Session) ->
   request(get, path(Session, "screenshot"), []).

available_engines(Session) ->
    request(get, path(Session, "ime/available_engines"), []).

active_engine(Session) ->
    request(get, path(Session, "ime/active_engine"), []).

-spec(frame(abstract_session(), string()| number() | null) ->
	     command_result()).
frame(Session, Frame)  ->
    Body = case Frame of
	       L when is_list(L) ->
		   list_to_binary(L);
	       I when is_integer(I) ->
		   I;
	       null ->
		   null
	   end,
    post(path(Session, "frame"), 
	 to_json([{<<"id">>, Body}])).

-spec(find_elements(abstract_session(), atom()|binary(), string()) -> command_result()).
find_elements(Session, Using, Value) ->
    Res = post(path(Session, "elements"), 
	       to_json([{<<"using">>, Using},
			{<<"value">>, list_to_binary(Value)}])),
    case Res of
	{ok , []} ->
	    {error, {7, no_such_element}};
	{ok, List} ->
	    {ok, [webelement_id(E) || E <- List] };
	Other ->
	    io:format(user,"~p~n",[Other]),
	    Other
    end.

-spec(find_elements(abstract_session(), string(), atom()|binary(), string()) -> command_result()).
find_elements(Session, Id, Using, Value) ->
    Res = post(path(Session, "element/"++ Id ++ "/elements"), 
	       to_json([{<<"using">>, Using},
			{<<"value">>, list_to_binary(Value)}])),
    case Res of
	{ok , []} ->
	    {error, {7, no_such_element}};
	{ok, List} ->
	    {ok, [webelement_id(E) || E <- List] };
	Other ->
	    io:format(user,"~p~n",[Other]),
	    Other
    end.
    
-spec(get_attribute(abstract_session(), string()|binary(), string()) ->
	     command_result()).
get_attribute(Session, Id, Value) when is_binary(Id) ->
    get_attribute(Session, binary_to_list(Id), Value);
get_attribute(Session, Id, Value) ->
    request(get, path(Session, "element/" ++ Id ++ "/attribute/" ++ Value), []).


click(Session, Id) when is_binary(Id) ->
    click(Session, binary_to_list(Id));
click(Session, Id) ->
    post(path(Session, "element/" ++ Id ++ "/click"), " ").

submit(Session, Id) when is_binary(Id) ->
    submit(Session, binary_to_list(Id));
submit(Session, Id) ->
    post(path(Session, "element/" ++ Id ++ "/submit"), " ").

text(Session, Id) when is_binary(Id) ->
    text(Session, binary_to_list(Id));
text(Session, Id) ->
    request(get, path(Session, "element/" ++ Id ++ "/text"), []).

title(Session) ->
    request(get, path(Session, "title"), []).

-spec(value(abstract_session(), binary()|string()) ->
	      command_result()).
value(Session, Id) when is_binary(Id) ->
    value(Session, binary_to_list(Id));
value(Session, Id) ->
    request(get, path(Session, "element/" ++ Id ++ "/value"), []).

-spec(value(abstract_session(), binary()|string(), string()) ->
          command_result()).
value(Session, Id, Value) when is_binary(Id) ->
    value(Session, binary_to_list(Id), Value);
value(Session, Id, Value) ->
    NewValue = [unicode:characters_to_binary([E])  || E <- Value],
    post(path(Session, "element/" ++ Id ++ "/value"),
     to_json([{<<"value">>, NewValue}])).

-spec(switch_to_window(abstract_session(), string()) ->
	     command_result()).
switch_to_window(Session, Name) ->
    post(path(Session,"window"), to_json([{<<"name">>, list_to_binary(Name)}])).

-spec(delete_window(abstract_session()) ->
	     command_result()).
delete_window(Session) ->
    request(delete,path(Session,"window"),[]).

-spec(speed(abstract_session()) -> command_result()).
speed(Session) ->
    request(get, path(Session,"speed"), []).

-spec(speed(abstract_session(), 'SLOW' | 'MEDIUM' | 'FAST') -> command_result()).
speed(Session, Speed) ->
    post(path(Session,"speed"), to_json([{<<"speed">>,Speed}])).

-spec(cookies(abstract_session()) -> command_result()).
cookies(Session) ->
    request(get, path(Session, "cookie"), []).

-type(cookie_options() :: {path, binary()} |
			  {domain, binary()} |
			  {secure, true|false} |
			  {expiry, integer()}).
-spec(add_cookie(abstract_session(), binary(), binary(), [cookie_options()]) ->
	     command_result()).
add_cookie(Session, Name, Value, Options) ->
    Body = [{"name", Name},
	    {"value", Value} | Options],
    post(path(Session, "cookie"), to_json([{cookie,{struct, Body}}])).
		    

-spec(delete_cookie(abstract_session(), binary()) ->
	     command_result()).
delete_cookie(Session, Name) ->
    request(delete, path(Session, "cookie/" ++ binary_to_list(Name)), []).

delete_cookies(Session) ->
    request(delete, path(Session, "cookie"), []).

source(Session) ->
    request(get, path(Session, "source"), []).

active_element(Session) ->
   post(path(Session, "element/active"), " ").

double_click(Session) ->
    post(path(Session, "doubleclick"), " ").

button_up(Session) ->
    post(path(Session, "button_up"), " ").

button_down(Session) ->
    post(path(Session, "buttondown"), " ").

mouse_click(Session) ->
    mouse_click(Session, left).

-spec(mouse_click(abstract_session(), left| right| middle) ->
	     command_result()).
mouse_click(Session, Button) ->
    Value = case Button of
		left -> 0;
		middle -> 1;
		right -> 2
	    end,
    post(path(Session,"click"), to_json([{<<"button">>, Value}])).		  

% POST /session/:sessionId/window/:windowHandle/size
window_resize(Session, X, Y) ->
    {ok, Handle} = get_window_handle(Session),
    post(path(Session,"window/" ++ binary_to_list(Handle) ++ "/size"), 
         to_json([{<<"width">>, X}, {<<"height">>, Y}])
        ).

% POST /session/:sessionId/accept_alert
accept_alert(Session) ->
    post(path(Session,"accept_alert"), " ").

% POST /session/:sessionId/dismiss_alert
dismiss_alert(Session) ->
    post(path(Session,"dismiss_alert"), " ").

within_frame(Session,[],F) ->           
    Res = F(),
    frame(Session, null),
    Res;
within_frame(Session,[Frame|Rest], F) ->        
    {ok, no_content} = frame(Session, Frame) ,
    within_frame(Session,Rest,F).    


webelement_id({struct, [{<<"ELEMENT">>, Id}]}) ->
    Id.

post(Path,Body) ->
    request(post, Path, Body).

request(Method, Path, Body) ->
    %%    io:format(user,"~p ~p ~p~n",[Method, Url ++ Path, Body]),
    case httpc:request(Method, req(Path, Body), [], []) of
	{error,_} = E -> 
	    E;
	{ok, {{_,204,_}, _,[]}} ->
	    {ok, no_content};
	{ok, {{_,302,_}, H, _B}} ->
	    {ok,{302,H}};
	{ok, {{_,200,_}, H, B}} ->
	    decode_response(decode_body(H,B));
	{ok, {{_,_Code,_Reason}, H, B}} ->
	    decode_response(decode_body(H,B))
    end.

path({Host, Port, Id}) ->
    Base = "http://" ++ Host ++ ":" ++ integer_to_list(Port) ++"/wd/hub/session",
    case Id of
	undefined -> Base;
	_ -> Base ++ "/" ++ Id
    end.

path(Session, Tail) ->
    path(Session) ++ "/" ++ Tail.

req(Path,[]) -> 
    {Path, []};
req(Path,Body) ->
    {Path, [], ?CONTENT_TYPE, Body}.
	    
to_json(Proplist) ->
    Res = mochijson2:encode({struct, Proplist}),
    iolist_to_binary(Res).

from_json(List) ->
%%    io:format(user,"~p~n",[List]),
    mochijson2:decode(list_to_binary(List)).

decode_body(Headers, Body) ->
    case proplists:get_value("content-type",Headers) of
	undefined ->
	    Body;
	"application/json;charset=UTF-8" ->
	    {struct, Object} = from_json(string:strip(Body,both,0)),
	    Object;
	_ -> 
	    Body
    end.

decode_response(List) ->
    case proplists:get_value(<<"status">>, List) of
	0 ->
	    {ok, proplists:get_value(<<"value">>, List)};
	Other -> 
	    case lists:member(Other,status_codes()) of
		true ->
		    {struct, Message} = proplists:get_value(<<"value">>, List),
		    {error, {Other, Message}};
		_ ->
		    {error, List}
	    end
    end.


status_codes() ->
    [7,8,9,10,11,12,13,15,17,19,23,24,25,28].
