-module(ircbot_plugin_cassie).
-author("rafael@tweep.de").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-define(MINWORDCOUNT, 5).
-define(MAXWORDCOUNT, 10).
-define(BREAKSTRING, " .,'").

init(_Args) ->
    ets:new(words,[duplicate_bag,named_table]),
    {ok, [_Args]}.


handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, What]} ->
            save_word_counts(binary_to_list(What)),
            %Ref:privmsg(<<"#",Channel/binary>>, [answer_me(What)]),
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
            ok
    end.

save_word_counts(Msg) ->
    Words = [ X || X <- string:tokens(Msg, ?BREAKSTRING), string:len(X) >= ?MINWORDCOUNT, string:len(X) =< ?MAXWORDCOUNT],
    lists:foreach(  fun(One) ->
                        Result = ets:lookup(words, One),
                        case Result of
                            [] -> ets:insert(words,{One,1});
                            _ -> [{One,LastCount}|_] = ets:lookup(words, One),
                                 ets:delete(words, One),
                                 ets:insert(words, {One, LastCount+1})
                        end
                    end, Words).


handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
