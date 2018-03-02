-module(utils).

-include ( "dev.hrl" ).

-export([
    uuid/0
    ,unixtime/0
    ,convertDomainString/1]).

uuid ( ) -> 
    quickrand:seed ( ),
    list_to_binary ( uuid:uuid_to_string ( uuid:get_v4_urandom ( ) ) ).

unixtime ( ) -> 
    {Mega, Seconds, Ms} = os:timestamp(),    
    ( Mega * 1000000 + Seconds ).

convertDomainString ( DomainString ) ->
    case string:split(DomainString, ":") of
        [Domain, Group] ->
            {Domain, binary_to_integer ( Group )};
        _Err ->
            error   
    end.
