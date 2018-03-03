
-module(erlstore_interface_http_domains_handler).

-behaviour(cowboy_handler).

-include ( "dev.hrl" ).

-define(common_handler, erlstore_interface_http_common_handler).

-export([
    init/2
]).

%
% Unauthenticated
%
init ( Request=#{method := <<"OPTIONS">>}, State ) ->    
    ?common_handler:options ( Request, State );

init ( Request=#{ headers := #{ <<"authorization">> := Token } }, State ) ->
    case erlstore_interface_auth:token ( decode, Token, <<"secretkey">> ) of 
        {error, expired} ->            
            erlstore_interface_http_json_response:respond ( Request, State, {5000, Token}, 401);
        {ok, #{ <<"data">> := User, <<"exp">> := ExpiryTime } } ->            
            case ExpiryTime - erlstore_utils:unixtime() of 
                TimeLeft when TimeLeft < 300 ->
                    init ( 
                            cowboy_req:set_resp_header(<<"Authorization">>,erlstore_interface_auth:token ( generate, User, <<"secretkey">>, 900 ), Request ),
                            State, User 
                        );
                _True ->
                    init ( Request, State, User )
            end  
    end;

init ( Request, State ) ->  
        ?common_handler:error ( Request, State ).

%
% Authenticated
%
init ( Request=#{method := <<"GET">>, bindings := #{id := Id} }, State, User ) -> 
    ?common_handler:response ( Request, State, erlstore_persistence:get ( domains, Id, User ) );

init ( Request=#{method := <<"GET">> }, State, User ) ->  
    ?common_handler:response ( Request, State, erlstore_persistence:getAll ( domains, User ) );

init ( Request=#{method := <<"POST">>, has_body := true }, State, _User ) ->  
    {ok, Body, _Req } = cowboy_req:read_body( Request ),
    Json = jsone:decode ( Body ),   
    ?common_handler:response ( Request, State, erlstore_persistence:createDomain ( Json ) );

init ( Request=#{method := <<"PUT">>, has_body := true }, State, _User ) ->  
    {ok, Body, _Req } = cowboy_req:read_body( Request ),
    Json = jsone:decode ( Body ),   
    ?common_handler:response ( Request, State, erlstore_persistence:updateDomain ( Json ) );

init ( Request=#{method := <<"DELETE">>, bindings := #{id := Id} }, State, _User ) ->         
    ?common_handler:response ( Request, State, erlstore_persistence:deleteDomain ( Id ) );

init ( Request, State, _User ) ->  
    ?common_handler:error ( Request, State ).