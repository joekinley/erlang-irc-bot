-module(ircbot_module_twitch).
-author("rafael@tweep.de").

-define(GETMODS, "https://api.twitch.tv/kraken/channels/thegypsyknight/editors")

start(Token) ->
  receive
    {get_mods} -> ok
  end.