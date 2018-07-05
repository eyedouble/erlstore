
-module(erlstore_common_changefeed_tests).

-include_lib("eunit/include/eunit.hrl").

-include("../../dev.hrl").

changefeed_notification_test ( ) ->
    R = erlstore_common_changefeed:notification ( create, null, null ),
    ?PRINT ( R ),
    ?assert ( R =:= {2100,{create,null,null}} ).

