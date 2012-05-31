-module(cb_selenium_01_news).

-export([init/0, stop/1]).

% This script is first executed at server startup and should
% return a list of WatchIDs that should be cancelled in the stop
% function below (stop is executed if the script is ever reloaded).
init() ->
    error_logger:info_msg("Starting selenium server...~n"),
    Path = boss_env:get_env(cb_selenium, path, "../cb_selenium"),
    Log = boss_env:get_env(cb_selenium, selenium_server_log_file, "/tmp/selenium_server.log"),
    EnableDebug = boss_env:get_env(cb_selenium, debug, false),
    Debug = case EnableDebug of
                true -> " -debug";
                false -> ""
            end,
    StartCmd = "cd " ++ Path ++ "/priv/selenium; java -jar selenium-server-standalone-2.21.0.jar -browserSessionReuse -log " ++ Log ++ Debug ++ " > /dev/null 2>& 1 &",
    StartOutput = os:cmd(StartCmd),
    case string:tokens(StartOutput, "\n") of
        ["0"] ->
            error_logger:info_msg("Selenium started succefully...~n");
        _ ->
            error_logger:info_msg("Error starting Selenium server~n~p~n", [StartOutput])
    end,
    {ok, []}.

stop(ListOfWatchIDs) ->
    lists:map(fun boss_news:cancel_watch/1, ListOfWatchIDs).

%%%%%%%%%%% Ideas
%    boss_news:watch("user-42.*",
%        fun
%            (updated, {Donald, 'location', OldLocation, NewLocation}) ->
%                ;
%            (updated, {Donald, 'email_address', OldEmail, NewEmail})
%        end),
%
%    boss_news:watch("user-*.status",
%        fun(updated, {User, 'status', OldStatus, NewStatus}) ->
%                Followers = User:followers(),
%                lists:map(fun(Follower) ->
%                            Follower:notify_status_update(User, NewStatus)
%                    end, Followers)
%        end),
%
%    boss_news:watch("users",
%        fun
%            (created, NewUser) ->
%                boss_mail:send(?WEBSITE_EMAIL_ADDRESS,
%                    ?ADMINISTRATOR_EMAIL_ADDRESS,
%                    "New account!",
%                    "~p just created an account!~n",
%                    [NewUser:name()]);
%            (deleted, OldUser) ->
%                ok
%        end),
%    
%    boss_news:watch("forum_replies",
%        fun
%            (created, Reply) ->
%                OrignalPost = Reply:original_post(),
%                OriginalAuthor = OriginalPost:author(),
%                case OriginalAuthor:is_online() of
%                    true ->
%                        boss_mq:push(OriginalAuthor:comet_channel(), <<"Someone replied!">>);
%                    false ->
%                        case OriginalAuthor:likes_email() of
%                            true ->
%                                boss_mail:send("website@blahblahblah",
%                                    OriginalAuthor:email_address(),
%                                    "Someone replied!"
%                                    "~p has replied to your post on ~p~n",
%                                    [(Reply:author()):name(), OriginalPost:title()]);
%                            false ->
%                                ok
%                        end
%                end;
%            (_, _) -> ok
%        end),
%    
%    boss_news:watch("forum_categories",
%        fun
%            (created, NewCategory) ->
%                boss_mail:send(?WEBSITE_EMAIL_ADDRESS,
%                    ?ADMINISTRATOR_EMAIL_ADDRESS,
%                    "New category: "++NewCategory:name(),
%                    "~p has created a new forum category called \"~p\"~n",
%                    [(NewCategory:created_by()):name(), NewCategory:name()]);
%            (_, _) -> ok
%        end),
%
%    boss_news:watch("forum_category-*.is_deleted",
%        fun 
%            (updated, {ForumCategory, 'is_deleted', false, true}) ->
%                ;
%            (updated, {ForumCategory, 'is_deleted', true, false}) ->
%        end).

% Invoking the API directly:
%boss_news:deleted("person-42", OldAttrs),
%boss_news:updated("person-42", OldAttrs, NewAttrs),
%boss_news:created("person-42", NewAttrs)

% Invoking the API via HTTP (with the admin application installed):
% POST /admin/news_api/deleted/person-42
% old[status] = something

% POST /admin/news_api/updated/person-42
% old[status] = blah
% new[status] = barf

% POST /admin/news_api/created/person-42
% new[status] = something
