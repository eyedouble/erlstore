
-module(interface_websocket_middleware).

-behaviour(cowboy_middleware).

-include("dev.hrl").

-export([
    execute/2
]).

execute(Req, Opts) ->
    ?PRINT(Req),
    ?PRINT(Opts),
	{ok, Req, Opts}.


   