
-module(interface_http_json_response).

-include("dev.hrl").

-export ( [
    respond/3
    ,respond/4
    ,convert/1
] ).

respond ( Request, State, Data ) ->
   respond ( Request, State, Data, 200 ).

respond ( Request, State, Data, Code ) ->
    Response = cowboy_req:reply( Code,
        #{
            <<"content-type">> => <<"application/json">>
            ,<<"Access-Control-Allow-Origin">> => <<"*">>
            ,<<"Access-Control-Allow-Headers">> => <<"Origin, X-Requested-With, Content-Type, Accept, Authorization">>            
            ,<<"Access-Control-Expose-Headers">> => <<"Origin, X-Requested-With, Content-Type, Accept, Authorization">>
            ,<<"Access-Control-Allow-Methods">> => <<"GET, POST, PUT, DELETE, OPTIONS">>
        },
        convert ( Data ),
    Request ),
    {ok, Response, State}.

convert ( { Code, Data } ) ->
    jsone:encode ( [ status( Code ), {data, Data} ] ).

status ( Code ) when Code < 3000 ->
    {ok, true};
status ( Code ) ->
    {error, Code}.


