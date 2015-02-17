-module(ircbot_plugin_points).
-author("rafael@tweep.de").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-define(SELF, "cassadeey").
-define(ADMINS, ["thegypsyknight","joekinley",?SELF]).

%BUG: if points are 10.2 (point values) the bot crashes

init(_Args) ->
    %ets:new(points,[set,named_table]),
    %ets:new(messages,[set,named_table]),
    %ets:new(nicks,[set,named_table]),
    %ets:new(quotes,[set,named_table]).
    ets:file2tab("points.tab"),
    ets:file2tab("messages.tab"),
    ets:file2tab("nicks.tab"),
    ets:file2tab("quotes.tab"),
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
remove_points(_Sender, Parts) ->
  IsAdmin = lists:member(_Sender, ?ADMINS),
  [_Nick|[_Points|_]] = Parts,
  case IsAdmin of
    true ->
      Current = ets:lookup(points,_Nick),
      case Current of
        []                         -> ets:insert(points,{_Nick, -string_to_num(_Points)});
        [{_Nick, CurrentPoints}|_] -> ets:delete(points,_Nick),
                                      ets:insert(points,{_Nick, CurrentPoints-string_to_num(_Points)});
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

format_person({Name, Age}) ->
  lists:flatten(io_lib:format("~s ~b", [Name, Age])).

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
    []                -> ets:insert(quotes,{Person,[Quote]});
    [{Person,Quotes}] -> ets:delete(quotes, Person),
                         ets:insert(quotes,{Person, [Quote]++Quotes})
  end.

string_to_num(S) ->
  case string:to_float(S) of
    {error,no_float} -> list_to_integer(S);
    {F,_Rest} -> floor(F)
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
   "!botsnack - yummy"].

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
