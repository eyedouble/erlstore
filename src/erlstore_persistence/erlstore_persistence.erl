
-module(erlstore_persistence).

-include("dev.hrl").

-define(adaptor, erlstore_mnesia_adaptor).
-define(superuser, #{ <<"id">> => <<"superadmin">>, <<"domain">> => <<"superadmin:0">> } ).

-export([
    start/1
    ,getAll/1
    ,getAll/2
    ,get/2
    ,get/3    
    ,create/3    
    ,update/2
    ,update/3
    ,delete/2
    ,delete/3
    ,filter/2
    ,filter/3
    ,listTables/0
    ,isTable/1
    ,infoTable/1
    ,createTable/1
    ,deleteTable/1
    ,createDomain/1
    ,deleteDomain/1
    ,createUser/1
    ,deleteUser/1
    ,subscribe/2
    ,subscribe/3
    ,dump/2
]).

% Instance
start ( Directory ) ->    
    ?adaptor:start ( Directory ).

% CommonCrud

getAll ( Table ) ->
    getAll ( Table, ?superuser ).

% REPLACE WHENS WITH PATTERN MATCHING AS BELOW?
getAll ( Table, User=#{ <<"domain">> := _ } ) when is_map ( User ) ->
    ?adaptor:getAll ( Table, User ).

get ( Table, Id ) ->
    get ( Table, Id, ?superuser ).

get ( Table, Id, User ) when is_map ( User ) ->
    ?adaptor:get ( Table, Id, User ).

create ( Table, Data ) ->
    create ( Table, Data, ?superuser ).

create ( Table, Data, User ) when is_map ( Data ) ->   
    Id = erlstore_utils:uuid ( ),   
    Object = case maps:is_key ( <<"_system">>, Data ) of
        true ->            
            maps:remove ( <<"_system">>, Data );
        false ->
            Data
    end,
    ObjectWithId = Object#{ <<"id">> => Id },
    ?MODULE:update ( Table, ObjectWithId, User ).

update ( Table, Data ) ->
    update ( Table, Data, ?superuser ).

update ( Table, Data=#{ <<"id">> := Id }, User ) when is_map ( Data ) -> 
    Object = erlstore_commoncrud_system:generate ( Data, User ),    
    ?adaptor:write ( Table, Id, Object ).

delete ( Table, Id ) ->
    delete ( Table, Id, ?superuser ).

delete ( Table, Id, User ) when is_map ( User ) ->
    ?adaptor:delete ( Table, Id, User ).

filter ( Table, Filters ) ->
    filter ( Table, Filters, ?superuser ).

filter ( Table, Filters, User ) when is_map ( User ) ->
    ?adaptor:filter ( Table, Filters, User ).


% Tables
listTables ( ) ->
    {2000, [Table||Table <- ?adaptor:listTables ( ), Table =/= users, Table =/= domains] }.

isTable ( Name ) ->
    ?adaptor:isTable ( Name ).

infoTable ( Name ) -> 
    ?adaptor:infoTable ( Name ).

createTable ( Name ) ->   
    case isTable ( Name ) of 
        true -> 
            {4002, Name};
        false ->
            case ?adaptor:createTable ( Name ) of
                {atomic, _Status} ->
                    {2000, Name};
                _Err ->
                    {4000, Name}
            end
    end.  

deleteTable ( Name ) ->
    ?adaptor:deleteTable ( Name ).

%
% Domains
%
createDomain ( Domain=#{ <<"groups">> := _Groups, <<"_system">> := _System } ) -> 
     createDomain ( maps:remove ( <<"_system">>, Domain ) );

createDomain ( Domain=#{ <<"groups">> := Groups } ) when is_list ( Groups ) ->    
    Id = erlstore_utils:uuid ( ),    
    updateDomain ( Domain#{ <<"id">> => Id } );

createDomain ( Domain ) ->
    {4020, Domain}.

updateDomain ( Domain=#{ <<"id">> := Id, <<"groups">> := Groups } ) when is_list ( Groups ) ->    
    DomainWithSystem = erlstore_commoncrud_system:generate ( 
        Domain, 
        #{ <<"id">> => Id, <<"domain">> => <<Id/binary, ":0">> } 
    ),
    Object = DomainWithSystem#{ <<"id">> => Id },
    ?adaptor:write ( domains, Id, Object );

updateDomain ( Domain ) ->
    {4020, Domain}.

deleteDomain ( Domain ) ->
    % ONLY ALLOWED ONCE ALL USERS THAT ARE PART OF DOMAIN ARE GONE!!
    % ONLY ALLOWED ONCE ALL DOCUMENTS THAT ARE WITHIN THE DOMAIN ARE GONE!!
    {4000, <<"NOT IMPLETMENTED YET">>}.

%
% Users
%
createUser ( User=#{ <<"id">> := _Id, <<"domain">> := _Domain, <<"_system">> := _System } ) ->
    createUser ( maps:remove ( <<"_system">>, User ) );

createUser ( User=#{ <<"id">> := Id, <<"domain">> := Domain } ) ->
    case erlstore_utils:convertDomainString ( Domain ) of
        {DomainId, Group} ->
            case ?MODULE:get ( domains, DomainId ) of
                {2000, _Domain} ->    
                    case ?MODULE:get ( users, Id ) of
                        {4000, _NoMatch} ->
                            case ?adaptor:write ( users, Id, erlstore_commoncrud_system:generate ( User, User ) ) of
                                {2000, CreatedUser } ->
                                    {2000, CreatedUser};
                                _True ->
                                    {4000, User}
                            end;
                        Err ->                         
                            {4002, User}                            
                    end;
                _Err ->
                    {4031, User}
            end;
        _Err -> 
            {4031, User}
    end;

createUser ( User ) ->
    {4030, User}.

deleteUser ( Id ) ->
    {4000, <<"not implemented yet">>}.

%
% Changefeed
%
subscribe ( Pid, Table ) ->
    spawn ( ?MODULE, subscribe, [subscribe, Pid, Table ] ).

subscribe ( subscribe, Pid, Table ) ->
    ?adaptor:subscribe ( Table ),
    subscribe ( listen, Pid, Table );

%
% STILL NEED TO IMPLEMENT FILTERING ON THE CHANGE FEED
%
subscribe ( listen, Pid, Table ) ->
    receive 
        {mnesia_table_event,{write,Table,{Table,_Id,Data},[],_Transaction}} ->            
            Pid ! {2000, {create, Data } };           
        {mnesia_table_event,{write,Table,{Table,_Id,Data},_OldDocument,_Transaction}} ->            
            Pid ! {2000, {update, Data } };  
        {mnesia_table_event,{delete,Table,{Table,Data},_OldData,_Transaction}} ->
            Pid ! {2000, {delete, Data } };
        _All ->
            null
    end,
    subscribe ( listen, Pid, Table ).

%
% Data dumping
%

dump ( import, FileName ) ->  
    OriginalNode = ?adaptor:dumpGetOriginalNode ( FileName ++ ".erlstoredump" ),   
    ?PRINT ( OriginalNode ),
    NewFileName = FileName ++ "-nc." ++ atom_to_list( node() ) ++ ".erlstoredump",
    ?PRINT ( NewFileName ),
    case OriginalNode =:= node () of        
        true -> 
            E = ?adaptor:dumpImport ( FileName ++ ".erlstoredump" ),
            ?PRINT ( E ),
            E;
        false ->            
            Q = ?adaptor:dumpChangeNode ( FileName ++ ".erlstoredump", NewFileName, OriginalNode, node() ),
            ?PRINT ( Q ),
            W = ?adaptor:dumpImport ( NewFileName ),
            ?PRINT ( W ),
            W
    end;

dump ( export, FileName ) ->
    ?adaptor:dumpExport ( FileName ++ "." ++ atom_to_list( node() ) ++ ".erlstoredump" ).
