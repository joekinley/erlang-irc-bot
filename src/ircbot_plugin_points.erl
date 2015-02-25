-module(ircbot_plugin_points).
-author("rafael@tweep.de").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-define(SELF, "cassadeey").
-define(ADMINS, ["thegypsyknight","joekinley",?SELF]).
-define(DONATION,"1").

init(_Args) ->
    %ets:new(points,[set,named_table]),
    %ets:new(messages,[set,named_table]),
    %ets:new(nicks,[set,named_table]),
    %ets:new(quotes,[set,named_table]).
    %ets:new(donors,[set,named_table]).
    ets:new(misc_dynamic,[set,named_table]), % miscellaneous table for dynamic stuff (that might be lost on restart)
    ets:new(votes,[set, named_table]),
    ets:file2tab("points.tab"),
    ets:file2tab("messages.tab"),
    ets:file2tab("nicks.tab"),
    ets:file2tab("quotes.tab"),
    ets:file2tab("donors.tab"),
    {ok, [_Args]}.


handle_event(Msg, State) ->
    case Msg of
        {in, Ref, [_Sender, _Name, <<"PRIVMSG">>, <<"#",Channel/binary>>, What]} ->
            Answer = handle_command(string:to_lower(binary_to_list(_Sender)), string:to_lower(binary_to_list(What))),
            case Answer of
              ok -> ok;
              _ -> Ref:privmsg(<<"#",Channel/binary>>, [Answer])
            end,
            {ok, State};
        {in, Ref, [_Sender, _Name, <<"JOIN">>, <<"#",Channel/binary>>]} ->
          Message = fetch_message(string:to_lower(binary_to_list(_Sender))),
          case Message of
            ok -> ok;
            _ -> Ref:privmsg(<<"#",Channel/binary>>, ["Messages for ",_Sender,": ", Message])
          end,
          {ok, State};
        _ ->
            {ok, State}
    end.

handle_command(_Sender, Msg) ->
  ets:insert(nicks,{_Sender}),
  ets:tab2file(nicks,"nicks.tab"),
  [Cmd|Parts] = string:tokens(Msg, " "),
  case Cmd of
    "!help" -> show_help();
    "!points" -> show_points(_Sender);
    "!showpoints" -> show_points(_Sender, Parts);
    "!addpoints" -> add_points(_Sender, Parts);
    "!removepoints" -> remove_points(_Sender, Parts);
    "!transferpoints" -> transfer_points(_Sender, Parts);
    "!highscore" -> show_highscore();
    "!botsnack" -> botsnack(_Sender);
    "!leavemessage" -> leave_message(_Sender, Parts);
    "!nick" -> find_nick(Parts);
    "!quoteby" -> quote_by(Parts);
    "!addquote" -> add_quote(Parts);
    "!startpoll" -> start_poll(_Sender, Parts);
    "!endpoll" -> end_poll(_Sender);
    "!showpoll" -> show_poll();
    "!vote" -> vote(_Sender, Parts);
    "!charity" -> charity(_Sender, Parts);
    "!donate" -> donate(_Sender, Parts);
    _ -> ok
  end.

show_points(_Sender) ->
  Entry = ets:lookup(points, _Sender),
  case Entry of
    [] -> _Sender++" has no points yet :(";
    [{_Sender, Points}|_] -> ets:tab2file(points, "points.tab"), string:join([_Sender,"has",integer_to_list(floor(Points)),"points"]," ");
    _ -> ok
  end.

show_points(_Sender, []) -> ok;
show_points(_Sender, _Parts) ->
  IsAdmin = lists:member(_Sender, ?ADMINS),
  [_Nick|_] = _Parts,
  case IsAdmin of
    true -> show_points(_Nick);
    _ -> ok
  end.

add_points(_Sender, Parts) when length(Parts) < 2 -> ok;
add_points(_Sender, Parts) ->
  IsAdmin = lists:member(_Sender, ?ADMINS),
  [_Nick|[_Points|_]] = Parts,
  case IsAdmin of
    true ->
      Current = ets:lookup(points,_Nick),
      case Current of
        []                         -> ets:insert(points,{_Nick, string_to_num(_Points)});
        [{_Nick, CurrentPoints}|_] -> ets:delete(points,_Nick),
                                      ets:insert(points,{_Nick, CurrentPoints+string_to_num(_Points)});
        _                          -> ok
      end,
      show_points(_Nick);
    _ -> ok
  end.

remove_points(_Sender, Parts) when length(Parts) < 2 -> ok;
remove_points(Sender, Parts) ->
  IsAdmin = lists:member(Sender, ?ADMINS),
  [_Nick|[_Points|_]] = Parts,
  case IsAdmin of
    true ->
      Current = ets:lookup(points,_Nick),
      case Current of
        []                         -> ets:insert(points,{_Nick, -string_to_num(_Points)});
        [{_Nick, CurrentPoints}|_] -> ets:delete(points,_Nick),
                                      ets:insert(points,{_Nick, floor(CurrentPoints)-string_to_num(_Points)});
        _                          -> ok
      end,
      show_points(_Nick);
    _ -> ok
  end.

transfer_points(_Sender, Parts) when length(Parts) < 2 -> ok;
transfer_points(_Sender, Parts) ->
  [_Receiver|[_Points|_]] = Parts,
  IPoints = string_to_num(_Points),
  case ets:lookup(points, _Sender) of
    [{_Sender, NPoints}|_] when NPoints >= IPoints, IPoints > 0 -> remove_points(?SELF,[_Sender,_Points]),
                                                                   add_points(?SELF,[_Receiver,_Points]);
    _ -> ok
  end.

botsnack(_Sender) -> ["<3 ", _Sender].

show_highscore() ->
  People = lists:sublist(lists:reverse(lists:keysort(2,ets:tab2list(points))),10),
  "Top 10: "++format_people(People).

format_person({Name, Points}) ->
  lists:flatten(io_lib:format("~s ~b", [Name, floor(Points)])).

format_people(People) ->
  string:join(lists:map(fun format_person/1, People), ", ").

leave_message(_, Parts) when length(Parts) < 2 -> ok;
leave_message(Sender, Parts) ->
  [Receiver|Message] = Parts,
  case ets:lookup(messages,Receiver) of
    [{Receiver,Messages}|_] -> NewMessages = Messages++["; ",Sender, ": ", string:join(Message," ")],
                               ets:delete(messages,Receiver),
                               ets:insert(messages,{Receiver,NewMessages});
    _                       -> ets:insert(messages,{Receiver,[Sender, ": ", string:join(Message," ")]})
  end,
  ets:tab2file(messages,"messages.tab"),
  ["Left message for ",Receiver].

fetch_message(Sender) ->
  case ets:lookup(messages, Sender) of
    [{Sender,Messages}|_] -> ets:delete(messages, Sender),
                             ets:tab2file(messages,"messages.tab"),
                             Messages;
    _                     -> ok
  end.

find_nick(Parts) when length(Parts) < 1 -> ok;
find_nick(Parts) ->
  [Part|_] = Parts,
  Found = lists:filter(fun(Elem) -> {N} = Elem, string:str(N,Part) > 0 end, ets:tab2list(nicks)),
  case Found of
    [] -> ok;
    _  -> string:join(to_stringlist(Found),", ")
  end.

to_stringlist([]) -> [];
to_stringlist(List) -> [{Head}|Tail] = List, [Head] ++ to_stringlist(Tail).

quote_by(Parts) when length(Parts) < 1 -> ok;
quote_by(Parts) ->
  [Person|_] = Parts,
  case ets:lookup(quotes, Person) of
    [{Sender,Quotes}] -> lists:nth(random:uniform(length(Quotes)),Quotes)++[" - ", Sender];
    _                 -> ok
  end.

add_quote(Parts) when length(Parts) < 2 -> ok;
add_quote(Parts) ->
  [Person|Quote] = Parts,
  case ets:lookup(quotes, Person) of
    []                -> ets:insert(quotes,{Person,[string:join(Quote, " ")]});
    [{Person,Quotes}] -> ets:delete(quotes, Person),
                         ets:insert(quotes,{Person, [string:join(Quote, " ")]++Quotes})
  end,
  ets:tab2file(quotes,"quotes.tab"),
  "Quote saved".

start_poll(_Sender, Parts) when length(Parts) < 3 -> ok;
start_poll(Sender, Parts) ->
  case lists:member(Sender, ?ADMINS) of
    true ->
      case ets:lookup(misc_dynamic, poll) of
        [] -> [Question|[T]] = string:tokens(string:join(Parts, " "),"|"),
              Answers = string:tokens(T, ","),
              ets:insert(misc_dynamic, {poll, {Sender, [Question], Answers}}),
              show_poll();
        _  -> "There is still an unfinished poll"
      end;
    _ -> ok
  end.

show_poll() ->
  case ets:lookup(misc_dynamic, poll) of
    []                                     -> ok;
    [{poll, {_Sender, Question, Answers}}] -> "Current poll: "++Question++" - Please vote with !vote <number> for "++format_answers(Answers);
    _                                      -> "Please help me!"
  end.

end_poll(Sender) ->
  case lists:member(Sender, ?ADMINS) of
    true ->
      case ets:lookup(misc_dynamic, poll) of
        [{poll, {_Sender, Question, Answers}}] -> Question++" -> "++get_highest_answer(Answers, ets:tab2list(votes));
        _                                     -> ok
      end;
    _ -> ok
  end.

get_highest_answer(Answers, Votes) ->
  ets:delete(misc_dynamic,poll),
  Zipped = lists:zip(Answers,lists:seq(1,length(Answers))),
  Final = lists:map(fun({Q,No}) -> {Q, length(lists:filter(fun({_,NoI}) -> case NoI of No -> true; _ -> false end end, Votes))} end, Zipped),
  [{Winner, TotalVotes}, {Second, SecondVotes}] = lists:sublist(lists:reverse(lists:keysort(2,Final)),2),
  Winner++" won by total of "++integer_to_list(TotalVotes)++" Votes, with "++Second++" coming second at "++integer_to_list(SecondVotes)++" votes".

vote(_Sender, Parts) when length(Parts) < 1 -> ok;
vote(Sender, [No|_]) ->
  INo = string_to_num(No),
  case ets:lookup(misc_dynamic, poll) of
    []  -> ok;
    [{poll, {_,_,Answers}}] ->
      if length(Answers) < INo, INo > 0 -> ok;
         true                  -> ets:insert(votes, {Sender, INo})
       end
  end,
  ok.

charity(_Sender, Parts) when length(Parts) < 1 -> ok;
charity(Sender, [Receiver|_]) ->
  case lists:member(Sender, ?ADMINS) of
    true -> transfer_points(Sender,[Receiver,?DONATION]),
            Receiver++" got a donation in the name of the Robin Hood Charity Initiative";
    _    -> ok
  end.

donate(_Sender, Parts) when length(Parts) < 1 -> ok;
donate(Sender,[Amount|_]) ->
  case transfer_points(Sender, ["cassadeey",Amount]) of
    ok -> ok;
    _  -> case ets:lookup(donors,Sender) of
            [{Sender,TotalAmount}] -> ets:insert(donors,{Sender,TotalAmount+string_to_num(Amount)}),
                                      "The Robin Hood Charity Initiative thanks "++Sender++" for donating for a noble cause";
            _                      -> ok
          end
  end.

format_answers(Answers)      -> format_answers(Answers, 1).
format_answers([], _)        -> "";
format_answers(Answers, Num) when Num == 1 -> [Current|Rest] = Answers, io_lib:format("~b) ~s", [Num, Current])++format_answers(Rest, Num+1);
format_answers(Answers, Num)               -> [Current|Rest] = Answers, io_lib:format("; ~b) ~s", [Num, Current])++format_answers(Rest, Num+1).

string_to_num(S) ->
  try
    case string:to_float(S) of
      {error,no_float} -> list_to_integer(S);
      {F,_Rest} -> floor(F)
    end
  catch error:badarg ->
    0
  end.

floor(X) when X < 0 ->
    T = trunc(X),
    case X - T == 0 of
        true -> T;
        false -> T - 1
    end;
floor(X) ->
    trunc(X).

show_help() ->
  ["!points - shows points, ",
   "!transferpoints <nick> <points> - transfers your points to <nick>, ",
   "!highscore - shows TOP 10, ",
   "!leavemessage <nick> <message> - leave a message for <nick>, ",
   "!nick <part> - finds all people with <part> in their name, ",
   "!quoteby <nick> - shows a random quote by <person>, ",
   "!addquote <nick> <quote> - adds the quote, ",
   "!showpoll - shows current poll if there is any, ",
   "!botsnack - yummy"].

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
