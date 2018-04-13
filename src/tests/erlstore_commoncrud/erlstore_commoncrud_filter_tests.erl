
-module(erlstore_commoncrud_filter_tests).

-include_lib("eunit/include/eunit.hrl").


mock ( ) ->
  #{<<"_system">> =>
    #{  <<"access">> => <<"d01:0">>,
        <<"times">> => #{<<"created">>=> 9,<<"updated">> => 10}
    },
    <<"id">> => <<"vanwel@eyedouble.nl">>,
    <<"list">> => [ "12", 5, 99 ]
  }.


filter_onelevel_test ( ) ->
    Result = erlstore_commoncrud_filter:filter ( 
      mock ()
      ,<<"_system">>
      ,<<"isset">>
      ,<<"">> ),
    ?assert ( Result =:= true ).

filter_threelevel_test ( ) ->
    Result = erlstore_commoncrud_filter:filter ( 
      mock ()
      ,<<"_system.times.created">>
      ,<<"isset">>
      ,<<"">> ),
    ?assert ( Result =:= true ).

filter_isnotset_test ( ) ->
    Result = erlstore_commoncrud_filter:filter ( 
      mock ()
      ,<<"_system.times.created.kak">>
      ,<<"isnotset">>
      ,<<"">> ),
    ?assert ( Result =:= true ).

filter_isnotset_negative_test ( ) ->
    Result = erlstore_commoncrud_filter:filter ( 
      mock ()
      ,<<"_system.times.created">>
      ,<<"isnotset">>
      ,<<"">> ),
    ?assert ( Result =:= false ).
  
filter_equals_test ( ) ->
    Result = erlstore_commoncrud_filter:filter ( 
      mock ()
      ,<<"_system.times.created">>
      ,<<"=">>
      ,9 ),
    ?assert ( Result =:= true ).

filter_flatlistlength_test ( ) ->
  Result = erlstore_commoncrud_filter:filter ( 
    mock ()
    ,<<"list">>
    ,<<"fllen">>
    ,3 ),
  ?assert ( Result =:= true ).

filter_flatlistmember_test ( ) ->
  Result = erlstore_commoncrud_filter:filter ( 
    mock ()
    ,<<"list">>
    ,<<"flmem">>
    ,"12" ),
  ?assert ( Result =:= true ).