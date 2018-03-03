
-module(erlstore_mnesia_adaptor).

-behaviour(erlstore_persistence_adaptor).

-include_lib("stdlib/include/qlc.hrl").

-include("dev.hrl").

-export([
    start/1
    ,getAll/2
    ,get/3
    ,write/3
    ,delete/3
    ,filter/3
    ,listTables/0
    ,isTable/1
    ,infoTable/1
    ,createTable/1
    ,deleteTable/1
    ,subscribe/1
    ,dumpExport/1
    ,dumpImport/1
    ,dumpGetOriginalNode/1
    ,dumpChangeNode/4
]).

%
% Instance
%
start ( Path ) ->
    application:set_env ( mnesia, dir, "data/db" ),
    mnesia:create_schema ( [node()] ),
    mnesia:start ( ),    
    mnesia:wait_for_tables ( listTables (), 2000 ),
    initTables ( ).

%
% CommonCRUD
%
getAll ( Table, User ) ->
    F = fun() ->
        qlc:eval ( qlc:q(
            [                    
                Data                    
                ||                    
                {_Type, _DId, Data} <- mnesia:table(Table),
                erlstore_commoncrud_filter:filter ( Data, <<"_system.access">>, <<"@">>, maps:get( <<"domain">>, User ))   

            ]
        ) )
    end,
    {atomic, Data} = mnesia:transaction(F),  
    {2000, Data}.  

get ( Table, Id, User ) -> 
    F = fun() ->
        qlc:eval ( qlc:q(
            [                    
                Data                    
                ||                    
                {_Type, DId, Data} <- mnesia:table(Table),     
                DId =:= Id,            
                erlstore_commoncrud_filter:filter ( Data, <<"_system.access">>, <<"@">>, maps:get( <<"domain">>, User ))   

            ]
        ) )
    end,
    case mnesia:transaction(F) of
        {atomic, [Data]} ->
            {2000, Data};
        _True ->
            {4000, null}
    end.

write ( Table, Id, Data ) ->    
    case mnesia:transaction( fun() ->
        mnesia:write(Table, {Table, Id, Data }, write )   
    end ) of
        {atomic, Result } ->
            {2000, Data};
        {aborted,{no_exists, ErrTable}} ->           
            {4001, ErrTable};
        {aborted,{bad_type,schema,_,write}} ->
            {4010, Table};
        _True ->            
            {4000, null}
    end.

delete ( Table, Id, User ) ->
    F = fun() ->    
        [{_Table, _Id, Data }] = mnesia:read ( {Table, Id} ),
        case erlstore_commoncrud_filter:filter ( Data, <<"_system.access">>, <<"@">>, maps:get( <<"domain">>, User )) of
            true -> 
                mnesia:delete ( Table, Id, write )            
        end       
    end,
    case mnesia:transaction ( F ) of
        {atomic, _Status} ->
            {2000, Id};
        {aborted,{{badmatch,_},_}} ->
            {4001, Id};
        _True ->
            {4000, null}
    end. 
    

filter ( Table, Filters, User ) ->
    F = fun() ->
        qlc:eval ( qlc:q(
            [                    
                Data                    
                ||                    
                {_Type, _DId, Data} <- mnesia:table(Table),
                erlstore_commoncrud_filter:filter ( Data, <<"_system.access">>, <<"@">>, maps:get( <<"domain">>, User )),
                lists:foldl(fun( { Property, Operator, Value }, Sum) ->                    
                    case { Sum, erlstore_commoncrud_filter:filter ( Data, Property, Operator, Value ) } of
                        {false, _ } ->
                            false;
                        {true, CheckResponse} ->
                            CheckResponse                    
                    end                    
                end, true, Filters)
            ]
        ) )
    end,
    {atomic, Data} = mnesia:transaction(F),  
    {2000, Data}.

%
% Tables
%
listTables ( ) -> 
    [Table||Table <- mnesia:system_info(tables), Table =/= schema].

isTable ( Name ) ->
    lists:member ( Name, listTables ( ) ).

infoTable ( Name ) ->
    case isTable ( Name ) of        
        true -> mnesia:table_info ( Name, all );
        false -> undefined
    end.        

createTable ( Name ) -> 
   case isTable ( Name ) of 
        true -> 
            4002;
        false ->
            mnesia:create_table( Name, [
                {disc_copies, [node()]},         
                {attributes, [ id, data ] }        
            ])
    end.

deleteTable ( Name ) ->  
    mnesia:schema ( Name ),        
    mnesia:delete_table ( Name ).


%
% Domains
%

createDomain ( ) ->
    ok.
deleteDomain ( ) ->
    ok.

%
% Users
%
createUser ( ) ->
    ok.
deleteUser ( ) ->
    ok.

%
% Changefeed
%
subscribe ( Table ) ->
    mnesia:subscribe ( {table, Table, detailed} ).    


%
% Data transfer
%
% "data/db/dump.bup"
dumpExport ( FileName ) ->
    mnesia:backup( FileName ).

dumpImport ( FileName ) ->
    mnesia:restore( FileName, [{default_op, recreate_tables}]).

dumpGetOriginalNode ( FileName ) ->
    View = fun(Item, Acc) ->
        case Item of 
            {schema, schema, Props } ->
                {[Item], lists:keyfind(disc_copies, 1, Props)};
            _Else ->
                {[Item], Acc}
        end
    end,
    {ok,{disc_copies,[OriginalNode]}} = mnesia:traverse_backup( FileName, read_only, View, 0),
    OriginalNode.

dumpChangeNode ( FileName, NewFileName, OriginalNode, NewNode ) ->
    change_node_name(mnesia_backup, OriginalNode, NewNode, FileName, NewFileName ).

% PRIVATE
initTables ( ) -> 
    createTable ( domains ),
    createTable ( users ).

change_node_name(Mod, From, To, Source, Target) ->
    Switch =
        fun(Node) when Node == From -> To;
           (Node) when Node == To -> throw({error, already_exists});
           (Node) -> Node
        end,
    Convert =
        fun({schema, db_nodes, Nodes}, Acc) ->                
                {[{schema, db_nodes, lists:map(Switch,Nodes)}], Acc};
           ({schema, version, Version}, Acc) ->               
                {[{schema, version, Version}], Acc};
           ({schema, cookie, Cookie}, Acc) ->                
                {[{schema, cookie, Cookie}], Acc};
           ({schema, Tab, CreateList}, Acc) ->                
                Keys = [ram_copies, disc_copies, disc_only_copies],
                OptSwitch =
                    fun({Key, Val}) ->                            
                            case lists:member(Key, Keys) of
                                true -> {Key, lists:map(Switch, Val)};
                                false-> {Key, Val}
                            end
                    end,
                {[{schema, Tab, lists:map(OptSwitch, CreateList)}], Acc};
           (Other, Acc) ->               
                {[Other], Acc}
        end,
    mnesia:traverse_backup(Source, Mod, Target, Mod, Convert, switched).
   
