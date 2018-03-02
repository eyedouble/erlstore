-module(interface_http_common_handler).

-behaviour(cowboy_handler).

-include ( "dev.hrl" ).

-export([
    init/2
    ,response/3
    ,options/2
    ,error/2
]).

%
% Unauthenticated
%
init ( Request=#{method := <<"OPTIONS">>}, State ) ->    
    options ( Request, State );

init ( Request=#{ headers := #{ <<"authorization">> := Token } }, State ) ->
    case interface_auth:token ( decode, Token, <<"secretkey">> ) of 
        {error, expired} ->            
            interface_http_json_response:respond ( Request, State, {5000, Token}, 401);
        {ok, #{ <<"data">> := User, <<"exp">> := ExpiryTime } } ->            
            case ExpiryTime - utils:unixtime() of 
                TimeLeft when TimeLeft < 300 ->
                    init ( 
                            cowboy_req:set_resp_header(<<"Authorization">>,interface_auth:token ( generate, User, <<"secretkey">>, 900 ), Request ),
                            State, User 
                        );
                _True ->
                    init ( Request, State, User )
            end  
    end;

init ( Request, State ) ->  
    ?MODULE:error ( Request, State ).

%
% Authenticated
%
init ( Request=#{method := <<"GET">>, bindings := #{table := Table, id := Id} }, State, User ) ->     
    response ( Request, State, erlstore_persistence:get ( binary_to_atom(Table, utf8), Id, User ) );

init ( Request=#{method := <<"GET">>, bindings := #{ table := Table }, qs := Qs }, State, User ) when Qs =/= <<"">> ->
    QueryParams = cowboy_req:parse_qs(Request),
    FilterParams = ?MODULE:parseFilterParams(QueryParams),
    response ( Request, State, erlstore_persistence:filter ( binary_to_atom ( Table, utf8 ), FilterParams, User ) );

init ( Request=#{method := <<"GET">>, bindings := #{ table := Table } }, State, User ) ->  
    response ( Request, State, erlstore_persistence:getAll ( binary_to_atom(Table, utf8), User ) );

init ( Request=#{method := <<"POST">>, bindings := #{ table := Table }, has_body := true }, State, User ) ->  
    {ok, Body, _Req } = cowboy_req:read_body( Request ),
    Json = jsone:decode ( Body ),   
    response ( Request, State, erlstore_persistence:create ( binary_to_atom(Table, utf8), Json, User ) );

% init ( Request=#{method := <<"PUT">>, bindings := #{ table := Table }, has_body := true }, State ) -> 
init ( Request=#{method := <<"PUT">>, bindings := #{ table := Table }, has_body := _Hasbody }, State, User ) ->  
    {ok, Body, _Req } = cowboy_req:read_body( Request ),
    Json = jsone:decode ( Body ),   
    response ( Request, State, erlstore_persistence:update ( binary_to_atom(Table, utf8), Json, User ) );

init ( Request=#{method := <<"DELETE">>, bindings := #{table := Table, id := Id} }, State, User ) ->         
    response ( Request, State, erlstore_persistence:delete ( binary_to_atom ( Table, utf8 ), Id, User ) );

init ( Request, State, _User ) ->  
    ?MODULE:error ( Request, State ).


options ( Request=#{method := <<"OPTIONS">>}, State ) ->
     response ( Request, State, {2000, ok} ).

response ( Request, State, Data ) ->    
    interface_http_json_response:respond ( Request, State, Data ).

error ( Request, State ) ->
    response ( Request, State, {3000, <<"Invalid operation">>} ).


%
% Filter Params
%
parseFilterParams ( QueryParams ) ->
    lists:map ( fun ( { Param, Expression } ) -> 
        [Operator, Value] = parseFilterExpression ( Expression ),
        {Param, Operator, Value}           
    end, QueryParams ).  

parseFilterExpression ( true ) ->
    [<<"isset">>, <<"">>];
parseFilterExpression ( <<"_">> ) ->
    [<<"isnotset">>, <<"">>];
parseFilterExpression ( Expression ) ->
    [Operator, Value] = string:split ( Expression, <<":">> ),   
    ConvertedValue = case re:run( Value, "^[+-]?([0-9]*[.])?[0-9]+$") of    
        {match,[_IsInteger]} ->            
            list_to_integer(binary_to_list(Value));  
        {match,[_Is, _Float]} ->      
            list_to_float(binary_to_list(Value));
        _Else when Value =:= <<"true">> ->
            true;
        _Else when Value =:= <<"false">> ->
            false;
        _Else ->
            Value
    end,
    [Operator, ConvertedValue].
