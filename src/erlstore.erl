
-module(erlstore).

-behaviour(application).

-include_lib("stdlib/include/qlc.hrl").

-include("dev.hrl").

-export([
    start/2
    ,stop/1
    ,boot/1
    ,bootHttp/1
    ,bootWebsocket/1
]).

start(_Type, _Args) ->   
    Response = erlstore_sup:start_link(),    
    % boot ( "data/db" ),
    % bootHttp ( 8090 ),
    % bootWebsocket ( 8091 ),
    Response.

stop(State) -> 
    ?PRINT( State ),
    ok.

boot ( Directory ) ->
    erlstore_persistence:start ( Directory ).

bootHttp ( Port ) ->
    {ok, HttpServerPid} = supervisor:start_child( erlstore_sup, 
        {interface_http_server, {interface_http_server, start, [Port]}, 
            permanent, brutal_kill, worker, [interface_http_server]} 
    ).

bootWebsocket ( Port ) ->
    {ok, WebsocketServerPid} = supervisor:start_child( erlstore_sup, 
        {interface_websocket_server, {interface_websocket_server, start, [Port]}, 
            permanent, brutal_kill, worker, [interface_websocket_server]} 
    ).
