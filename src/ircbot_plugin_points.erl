-module(ircbot_plugin_points).
-author("rafael@tweep.de").

-behaviour(gen_event).
-export([init/1, handle_event/2, terminate/2, handle_call/2, handle_info/2, code_change/3]).

-define(SELF, "cassadeey").
-define(ADMINS, ["thegypsyknight","joekinley",?SELF]).

init(_Args) ->
    %ets:new(points,[set,named_table]),
    ets:file2tab("points.tab"),
    ets:new(messages,[set,named_table]),
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
        %showmy101!showmy101@showmy101.tmi.twitch.tv JOIN #thegypsyknight
        {in, Ref, [_Sender, _Name, <<"JOIN">>, <<"#",Channel/binary>>]} ->
          Message = fetch_message(string:to_lower(binary_to_list(_Sender))),
          case Message of
            ok -> ok;
            _ -> Ref:privmsg(<<"#",Channel/binary>>, [Message])
          end,
          {ok, State};
        _ ->
            {ok, State}
    end.

handle_command(_Sender, Msg) ->
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
    _ -> ok
  end.

show_points(_Sender) ->
  Entry = ets:lookup(points, _Sender),
  case Entry of
    [] -> _Sender++" has no points yet :(";
    [{_Sender, Points}|_] -> ets:tab2file(points, "points.tab"), string:join([_Sender,"has",integer_to_list(Points),"points"]," ");
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
        []                         -> ets:insert(points,{_Nick, list_to_integer(_Points)});
        [{_Nick, CurrentPoints}|_] -> ets:delete(points,_Nick),
                                      ets:insert(points,{_Nick, CurrentPoints+list_to_integer(_Points)});
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
        []                         -> ets:insert(points,{_Nick, -list_to_integer(_Points)});
        [{_Nick, CurrentPoints}|_] -> ets:delete(points,_Nick),
                                      ets:insert(points,{_Nick, CurrentPoints-list_to_integer(_Points)});
        _                          -> ok
      end,
      show_points(_Nick);
    _ -> ok
  end.

transfer_points(_Sender, Parts) when length(Parts) < 2 -> ok;
transfer_points(_Sender, Parts) ->
  [_Receiver|[_Points|_]] = Parts,
  IPoints = list_to_integer(_Points),
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
  [Receiver|[Message|_]] = Parts,
  case ets:lookup(messages,Receiver) of
    [{Receiver,Messages}|_] -> NewMessages = Messages++[Sender, ": ", string:join(Message," ")],
                               ets:delete(messages,Receiver),
                               ets:insert(messages,{Receiver,NewMessages});
    _                       -> ets:insert(messages,{Receiver,[Message]})
  end,
  ["Left message for ",Receiver].

fetch_message(Sender) ->
  case ets:lookup(messages, Sender) of
    [{Sender,Messages}|_] -> ets:delete(messages, Sender),
                             Messages;
    _                     -> ok
  end.

show_help() ->
  ["!points - shows points, ",
   "!transferpoints <nick> <points> - transfers your points to <nick>, ",
   "!highscore - shows TOP 10, ",
   "!leavemessage <nick> <message> - leave a message for <nick>, ",
   "!botsnack - yummy"].

handle_call(_Request, State) -> {ok, ok, State}.
handle_info(_Info, State) -> {ok, State}.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_Args, _State) -> ok.
