-module(ircbot_plugin_cassie).
-author("rafael@tweep.de").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-define(MINWORDCOUNT, 5).
-define(MAXWORDCOUNT, 10).

init(_Args) ->
    {ok, [_Args]}.


handle_event(Msg, State) ->
    %timer:sleep(2000+random:uniform(5000-2000)),
    case Msg of
        {in, Ref, [_Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, What]} ->
            Ref:privmsg(<<"#",Channel/binary>>, [answer_me(What)]),
            {ok, State};
        _ ->
            {ok, State}
    end.

answer_me(Msg) ->
    case Msg of
        <<"!foo">> ->
            "Bar thank you ";
        <<"!bar">> ->
            "You're welcome";
        <<"!who">> ->
            "I am Cassadee";
        <<"!how">> ->
            <<"That's how ",?MINWORDCOUNT>>;
        _ ->
            Msg
    end.

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
