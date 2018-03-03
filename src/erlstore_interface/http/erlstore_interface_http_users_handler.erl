
-module(erlstore_interface_http_users_handler).

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
    ?common_handler:response ( Request, State, erlstore_persistence:get ( users, Id, User ) );

init ( Request=#{method := <<"GET">> }, State, User ) ->  
    ?common_handler:response ( Request, State, erlstore_persistence:getAll ( users, User ) );

init ( Request=#{method := <<"POST">>, has_body := true }, State, _User ) ->  
    {ok, Body, _Req } = cowboy_req:read_body( Request ),
    Result = handleUserWrite ( Body ),
    ?common_handler:response ( Request, State, Result );

init ( Request=#{method := <<"DELETE">>, bindings := #{id := Id} }, State, _User ) ->         
    ?common_handler:response ( Request, State, erlstore_persistence:deleteUser ( Id ) );

init ( Request, State, _User ) ->  
    ?common_handler:error ( Request, State ).

handleUserWrite ( Body ) ->
    case jsone:decode ( Body ) of 
        User=#{ <<"password">> := Password } ->
            Salt = erlstore_utils:uuid ( ),
            Hash = erlstore_interface_auth:password ( hash, Password, Salt ),
            UserWithHash = User#{ <<"password">> := Hash, <<"salt">> => Salt }, 
            erlstore_persistence:createUser ( UserWithHash );
        _Else ->
            {5000, null}
    end.