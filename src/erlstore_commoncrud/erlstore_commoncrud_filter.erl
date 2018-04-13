
-module(erlstore_commoncrud_filter).

-include("dev.hrl").

-export([filter/4]).

filter ( Data, Keys, Operator, Value ) when is_binary ( Keys ) ->
    filter ( Data, string:split(Keys, ".", all), Operator, Value );

filter ( Data, Keys, Operator, Value ) when is_list ( Keys ) andalso length ( Keys ) > 1 ->    
    case maps:is_key ( lists:nth(1, Keys ), Data ) of 
        true ->            
            filter ( maps:get ( lists:nth(1, Keys ), Data ), lists:nthtail (1, Keys), Operator, Value  );            
        false ->
            false
    end;

filter ( Data, Keys, Operator, Value ) when is_list ( Keys ) andalso length ( Keys ) =:= 1 ->    
    case Operator of
        <<"isset">> ->
            maps:is_key ( lists:nth(1, Keys ), Data );
        <<"isnotset">> when is_map ( Data ) =:= false ->
            true;
        <<"isnotset">> when is_map ( Data ) ->
            maps:is_key ( lists:nth(1, Keys ), Data ) =:= false;
        <<"=">> ->            
            maps:is_key ( lists:nth(1, Keys ), Data ) andalso
            maps:get ( lists:nth(1, Keys ), Data ) =:= Value;
        <<"/">> ->            
            maps:is_key ( lists:nth(1, Keys ), Data ) andalso
            maps:get ( lists:nth(1, Keys ), Data ) =/= Value;
        <<"<">> ->            
            maps:is_key ( lists:nth(1, Keys ), Data ) andalso
            maps:get ( lists:nth(1, Keys ), Data ) < Value;
        <<"=<">> ->            
                maps:is_key ( lists:nth(1, Keys ), Data ) andalso
                maps:get ( lists:nth(1, Keys ), Data ) =< Value;
        <<">">> ->            
                maps:is_key ( lists:nth(1, Keys ), Data ) andalso
                maps:get ( lists:nth(1, Keys ), Data ) > Value;
        <<">=">> ->            
                maps:is_key ( lists:nth(1, Keys ), Data ) andalso
                maps:get ( lists:nth(1, Keys ), Data ) >= Value;
        <<"fllen">> ->
            maps:is_key ( lists:nth(1, Keys ), Data ) andalso 
            is_list( maps:get ( lists:nth(1, Keys ), Data ) ) andalso
            length ( maps:get ( lists:nth(1, Keys ), Data ) ) =:= Value;
        <<"flmem">> ->
                maps:is_key ( lists:nth(1, Keys ), Data ) andalso 
                is_list( maps:get ( lists:nth(1, Keys ), Data ) ) andalso
                lists:member ( Value, maps:get ( lists:nth(1, Keys ), Data ) );
        <<"@">> ->
            hasAccess ( maps:get ( lists:nth(1, Keys ), Data ), Value )
    end.

  

%
% PRIVATE
%
hasAccess ( _DataDomain, <<"superadmin:0">> ) ->
    true;

hasAccess ( DataDomainString, Accessor ) ->    
    {DataDomain, DataGroup} = erlstore_utils:convertDomainString ( DataDomainString ),
    {AccessorDomain, AccessorGroup} = erlstore_utils:convertDomainString ( Accessor ),   
    AccessorDomain =:= DataDomain andalso AccessorGroup =< DataGroup.
