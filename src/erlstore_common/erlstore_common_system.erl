
-module (erlstore_common_system).

-export([
    generate/2
    ,update/2
]).

generate ( Data, User ) when is_map ( Data ) andalso is_map ( User ) -> 
    case maps:is_key ( <<"_system">>, Data ) of 
        true ->
            update ( Data, User );
        false ->
            new ( Data, User )
    end.


new ( Data, User ) when is_map ( User ) ->
    Data#{ 
        <<"_system">> => #{
            <<"access">> => maps:get ( <<"domain">>, User ),
            <<"created">> => erlstore_utils:unixtime(),
            <<"updated">> => erlstore_utils:unixtime(),
            <<"owner">> => maps:get ( <<"id">>, User ),
            <<"last_editor">> => maps:get ( <<"id">>, User )
        } 
    }.

update ( Data=#{ <<"_system">> := CurrentSystem }, User ) when is_map ( User ) ->
    Data#{ <<"_system">> => 
        CurrentSystem#{ 
            <<"updated">> := erlstore_utils:unixtime(),
            <<"last_editor">> := maps:get ( <<"id">>, User )
        }
    }.