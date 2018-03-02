
-module(interface_websocket_server).

-include("dev.hrl").

- export([
    start/1
    ,stop/0
]).

start ( Port ) ->
    
    Handlers = [             
        {"/", interface_websocket_handler, [] }   
    ],
    Dispatch = cowboy_router:compile([
        {'_', Handlers }            
    ]),
    {ok, _} = cowboy:start_clear(socket_listener,
        [{port, Port}],
        #{
            env => #{dispatch => Dispatch},
            middlewares => [
                cowboy_router, 
                interface_websocket_middleware, 
                cowboy_handler
            ]
        }
    ).

stop ( ) ->
    ok.