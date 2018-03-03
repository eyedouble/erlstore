
-module(erlstore_interface_http_auth_handler).

-behaviour(cowboy_handler).

-include ( "dev.hrl" ).

-define(common_handler, erlstore_interface_http_common_handler).

-export([
    init/2
]).

init ( Request=#{method := <<"POST">>, bindings := #{action := <<"login">>}, has_body := true }, State ) ->  
    {ok, Body, _Req } = cowboy_req:read_body( Request ),
 
    Credentials = case jsone:decode ( Body ) of
        #{ <<"username">> := Username, <<"password">> := Password } ->
            case erlstore_persistence:get ( users, Username ) of
                {4000, _} -> 
                    {5000, null};
                {2000, User=#{<<"id">> := Id, <<"password">> := Hash, <<"salt">> := Salt } } ->
                    case erlstore_interface_auth:credentials ( check, {Username, Id}, {Password, Salt, Hash} ) of
                        true ->
                            {2000, User};
                        false ->
                            {5000, null}
                    end;
                _Else -> 
                    {5000, null}
            end;
        Else ->
            {5000, Else}
    end,
    case Credentials of
        {2000, VerifiedUser} ->
            Token = erlstore_interface_auth:token ( generate, VerifiedUser, <<"secretkey">>, 900 ),
            ?common_handler:response ( cowboy_req:set_resp_header(<<"Authorization">>, Token, Request ), State, {2000, Token} ); 
        _NoAccess ->
            ?common_handler:response ( Request, State, {5000, null} )
        end;


init ( Request=#{method := <<"OPTIONS">>}, State ) ->
    ?common_handler:init ( Request, State );

init ( Request, State ) ->  
    ?common_handler:error ( Request, State ).


