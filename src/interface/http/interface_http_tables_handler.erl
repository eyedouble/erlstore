
-module(interface_http_tables_handler).

-behaviour(cowboy_handler).

-include ( "dev.hrl" ).

-define(common_handler, interface_http_common_handler).

-export([
    init/2
]).

init ( Request=#{method := <<"GET">>, bindings := #{action := <<"list">> } }, State ) ->    
    ?common_handler:response ( Request, State, erlstore_persistence:listTables ( ) );

init ( Request=#{method := <<"POST">>, has_body := true }, State ) ->  
    {ok, Body, _Req } = cowboy_req:read_body( Request ),
    #{ <<"name">> := Name } = jsone:decode ( Body ),
    ?common_handler:response ( Request, State, erlstore_persistence:createTable ( Name ) );

init ( Request=#{method := <<"DELETE">>, bindings := #{table := Table} }, State ) ->         
    ?common_handler:response ( Request, State, erlstore_persistence:deleteTable ( Table ) );

init ( Request=#{method := <<"OPTIONS">>}, State ) ->
    ?common_handler:init ( Request, State );

init ( Request, State ) ->  
    ?common_handler:error ( Request, State ).
