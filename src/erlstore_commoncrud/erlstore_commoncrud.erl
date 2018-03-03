
-module(erlstore_commoncrud).

-include("dev.hrl").

-export([
    getAll/1
    ,getAll/2
    ,create/2
    ,create/3
]).

getAll ( Table ) ->
    getAll ( Table, <<"superadmin">> ).

getAll ( Table, UserDomain ) ->
    ok.

create ( Table, Data ) ->
    create ( Table, Data, <<"superadmin">> ).

create ( Table, Data, UserDomain ) ->
    ok.

