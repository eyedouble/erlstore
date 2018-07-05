
-module(erlstore_common_changefeed).

-include("../dev.hrl").

-export([
    notify/4
    ,filter/2
]).

-define ( status_code(Code), maps:get(Code, #{create => 2100, update => 2101, delete=> 2102}) ).

notify ( Receipent, Type, Table, Data ) ->
    Receipent ! notification ( Type, Table, Data ).

filter ( Filters, Data ) when is_list ( Data )  ->
    case Filters of 
        [] -> Data;
        Populated ->  [X||X<-Data, lists:foldl(fun( { Property, Operator, Value }, Sum) ->
            case { Sum, erlstore_common_filter:filter ( X, Property, Operator, Value ) } of
                {false, _ } -> false;
                {true, CheckResponse} -> CheckResponse                    
            end                    
        end, true, Populated )]            
    end;

filter ( Filters, Data ) ->
    filter ( Filters, [Data] ).

%
%   PRIV
%
notification ( Type, Table, Data ) ->
   {?status_code(Type), {Type, Table, Data } }.