
-module(erlstore_interface_auth_tests).

-include ( "dev.hrl" ).
-include_lib("eunit/include/eunit.hrl").

-define(key, <<"loepie">>).
-define(expiration_time, 200).
-record( mock, {
    username = <<"Username@example.com">>,
    password = <<"loepie">>,
    salt = <<"salt">>,
    hash = <<"0e480b9e29dd649b80b8ccbf2fcc80a7eb59854f">>
}).

password_hash_test ( ) ->    
    Key = erlstore_interface_auth:password ( hash, 
                    <<"password">>,
                    <<"salt">>
                    ),
    ?assert ( Key == <<"c5e478d59288c841aa530db6845c4c8d962893a0">> ).


token_generate_test ( ) ->
    Claims = [
        {user_id, 42},
        {username, <<"vanwel@eyedouble.nl">>}
    ],
    Res = erlstore_interface_auth:token (generate, Claims, ?key, ?expiration_time ),
    ?assert ( is_binary ( Res ) ).

token_verify_expired_test ( ) -> 
    ExpiredToken = <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJleHAiOjE1MTIxMjQ5NjAsInVzZXJfaWQiOjQyLCJ1c2VybmFtZSI6InZhbndlbEBleWVkb3VibGUubmwifQ.VE6qj2NcwjxIdUWcPabZSJ79p7qWTMp9IqQ9EPNNrz8">>,
    Res = erlstore_interface_auth:token (verify, ExpiredToken, ?key ),
    ?assert ( Res == error ).

token_verify_valid_test ( ) -> 
    ValidToken = <<"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjo0MiwidXNlcm5hbWUiOiJ2YW53ZWxAZXllZG91YmxlLm5sIn0.WyG7x3Hb9ncP1wM-2F35asTSZ1hhyJjc-5Ro7Xgf4L4">>,
    Res = erlstore_interface_auth:token (verify, ValidToken, ?key ),
    ?assert ( Res == ok ).

credentials_check_test ( ) -> 
    Data = #mock{},
    Res = erlstore_interface_auth:credentials ( check,
        { Data#mock.username, Data#mock.username },
        { Data#mock.password, Data#mock.salt, Data#mock.hash }
    ),        
    ?assert ( Res =:= true ).

credentials_check_fail_test ( ) -> 
    Data = #mock{},           
    Res = erlstore_interface_auth:credentials ( check,
        { Data#mock.username, <<"roepie">> },
        { Data#mock.password, Data#mock.salt, Data#mock.hash }
    ),
    ?assert ( Res =:= false ).