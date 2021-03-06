
-module(erlstore_persistence).

-include("../dev.hrl").

-define(adaptor, erlstore_mnesia_adaptor).
-define(superuser, #{ <<"id">> => <<"superadmin">>, <<"domain">> => <<"superadmin:0">> } ).

-export([
    start/1
    ,getAll/1
    ,getAll/2
    ,get/2
    ,get/3    
    ,create/2
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
    ,updateDomain/1
    ,deleteDomain/1
    ,createUser/1
    ,deleteUser/1
    ,subscribe/2
    ,subscribe/3
    ,subscribe/4
    ,dump_export/1
    ,dump_import/1
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
    Object = erlstore_common_system:generate ( Data, User ),    
    ?adaptor:write ( Table, Id, Object ).

delete ( Table, Id ) ->
    delete ( Table, Id, ?superuser ).

delete ( Table, Id, User ) when is_map ( User ) ->
    ?adaptor:delete ( Table, Id, User ).

filter ( Table, Filters ) ->
    filter ( Table, Filters, ?superuser ).

filter ( Table, Filters, User ) when is_map ( User ) ->
    ?adaptor:filter ( Table, Filters, User ).


%
% Tables
%
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
    DomainWithSystem = erlstore_common_system:generate ( 
        Domain, 
        #{ <<"id">> => Id, <<"domain">> => <<Id/binary, ":0">> } 
    ),
    Object = DomainWithSystem#{ <<"id">> => Id },
    ?adaptor:write ( domains, Id, Object );

updateDomain ( Domain ) ->
    {4020, Domain}.

deleteDomain ( _Domain ) ->
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
        {DomainId, _Group} ->
            case ?MODULE:get ( domains, DomainId ) of
                {2000, _Domain} ->    
                    case ?MODULE:get ( users, Id ) of
                        {4000, _NoMatch} ->
                            case ?adaptor:write ( users, Id, erlstore_common_system:generate ( User, User ) ) of
                                {2000, CreatedUser } ->
                                    {2000, CreatedUser};
                                _True ->
                                    {4000, User}
                            end;
                        _Err ->                         
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

deleteUser ( _Id ) ->
    {4000, <<"not implemented yet">>}.

%
% Changefeed
%
subscribe ( Pid, Table ) ->
    subscribe ( Pid, Table, [], ?superuser ).

subscribe ( Pid, Table, User ) when is_map ( User ) ->
    subscribe ( Pid, Table, [], User );

subscribe ( Pid, Table, Filters ) when is_list ( Filters ) ->
    subscribe ( Pid, Table, Filters, ?superuser ).   

subscribe ( Pid, Table, Filters, User ) ->
    case isTable ( Table ) of 
        false -> {4001, Table};
        true -> ?adaptor:subscribe ( Pid, Table, Filters, User )
    end.

%
%   Data dumping
%
dump_import ( FileName ) ->        
    case filelib:is_file ( FileName ) of
        true -> 
            Split = string:split ( FileName, ".", trailing ),
            {FileName2, Ext} = {lists:nth (1, Split ), "." ++ lists:last( Split )},
            OriginalNode = ?adaptor:dumpGetOriginalNode ( FileName ),            
            ImportFileName = case OriginalNode =:= node () of
                true -> FileName;                   
                false ->           
                    NodeChangeFileName = FileName2 ++ "_nc." ++ atom_to_list( node() ) ++ Ext, 
                    ?adaptor:dumpChangeNode ( FileName, NodeChangeFileName, OriginalNode, node() ),
                    NodeChangeFileName
            end,
            case ?adaptor:dumpImport ( ImportFileName ) of         
                {aborted, Reason} -> {error, Reason};     
                {atomic, _Tables} ->  
                    ImportedFileName = string:join ( lists:join ( "_imported" ++ Ext, string:split ( ImportFileName, Ext, all ) ), "" ), 
                    file:rename ( ImportFileName, ImportedFileName )
            end;
        false -> {error, "File does not exist"}
    end.

dump_export ( FileName ) ->  
    case ?adaptor:dumpExport ( FileName ) of
       ok -> {ok, FileName};
       Error -> Error
    end.
