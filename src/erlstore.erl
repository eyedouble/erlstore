
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

    % ----- STANDALONE ------ % 
    % boot ( "data/db" ),
    % bootHttp ( 8090 ),
    % bootWebsocket ( 8091 ),
    % ------------------------%


    % ----- DEV ------ % 
    % {2000, X} = erlstore_persistence:createDomain ( #{ 
    %     <<"name">> => <<"test">>,
    %     <<"groups">> => [ <<"superadmin">> ]
    % } ),
    % Id = maps:get(<<"id">>, X),
    % Salt = erlstore_utils:uuid ( ),
    % Hash = erlstore_interface_auth:password ( hash, <<"superadmin">>, Salt ),
    % Y = erlstore_persistence:createUser ( #{
    %     <<"id">> => <<"superadmin">>,
    %     <<"salt">> => Salt,        
    %     <<"password">> => Hash,
    %     <<"domain">> => <<Id/binary, ":0">>
    % } ),

    % ?PRINT(Y),
    % ------------------------%
    Response.

stop(State) -> 
    ?PRINT( State ),
    ok.

boot ( Directory ) ->
    erlstore_persistence:start ( Directory ).

bootHttp ( Port ) ->
    {ok, HttpServerPid} = supervisor:start_child( erlstore_sup, 
        {erlstore_interface_http_server, {erlstore_interface_http_server, start, [Port]}, 
            permanent, brutal_kill, worker, [erlstore_interface_http_server]} 
    ).

bootWebsocket ( Port ) ->
    {ok, WebsocketServerPid} = supervisor:start_child( erlstore_sup, 
        {erlstore_interface_websocket_server, {erlstore_interface_websocket_server, start, [Port]}, 
            permanent, brutal_kill, worker, [erlstore_interface_websocket_server]} 
    ).
