
-module(erlstore_interface_http_file_handler).

-behaviour(cowboy_handler).

-include ( "dev.hrl" ).

-define(common_handler, erlstore_interface_http_common_handler).

-export([
    init/2
    ,data_payload/2
]).

%
% Unauthenticated
%
init ( Request=#{method := <<"OPTIONS">>}, State ) ->    
    ?common_handler:options ( Request, State );

init ( Request=#{ headers := #{ <<"authorization">> := Token } }, State ) ->
    case erlstore_interface_auth:token ( decode, Token, <<"secretkey">> ) of 
        {error, expired} ->            
            erlstore_interface_http_json_response:respond ( Request, State, {5000, Token}, 401);
        {ok, #{ <<"data">> := User, <<"exp">> := ExpiryTime } } ->            
            case ExpiryTime - erlstore_utils:unixtime() of 
                TimeLeft when TimeLeft < 300 ->
                    init ( 
                            cowboy_req:set_resp_header(<<"Authorization">>,erlstore_interface_auth:token ( generate, User, <<"secretkey">>, 900 ), Request ),
                            State, User 
                        );
                _True ->
                    init ( Request, State, User )
            end  
    end;

init ( Request, State ) ->  
        ?common_handler:error ( Request, State ).

%
% Authenticated
%
init ( Request=#{method := <<"GET">> }, State, User ) ->  
    ?common_handler:response ( Request, State, {2000, ok} );

init ( Request=#{method := <<"POST">>, bindings := #{id := <<"upload">>}, has_body := true }, State, User ) ->   
    X = multipart ( Request, erlstore_interface_http_common_handler ),
    % Y = cow_multipart:form_data(Headers),
    ?PRINT ( X ),
    ?common_handler:response ( Request, State, {2000, ok} );

% init ( Request=#{method := <<"POST">>, has_body := true }, State, User ) ->   
%     X = multipart ( Request, erlstore_interface_http_common_handler ),
%     % Y = cow_multipart:form_data(Headers),
%     ?PRINT ( X ),
%     ?common_handler:response ( Request, State, {2000, ok} );

init ( Request, State, _User ) ->  
    ?common_handler:error ( Request, State ).


data_payload(FieldName, Body) ->
    ?PRINT ( FieldName ),
    ?PRINT ( Body ).

file_payload(FileSize) ->
    ?PRINT ( FileSize ).




%
% MULTPART HANDLING
%

-define(MAX_FILE_SIZE_LIMIT, (16 * 1048580) ). % 16Mb
% -define(MAX_FILES,unlimited).
-define(TMP_PATH,"tmp").


multipart ( Request, Callback ) ->
    multipart( Request, Callback, ?MAX_FILE_SIZE_LIMIT).
multipart ( Request, Callback, MaxFileSizeLimit ) when is_atom( Callback ) and is_integer ( MaxFileSizeLimit ) ->
    
    case cowboy_req:read_part ( Request ) of   
        {ok, Headers, Request2} -> 
            Request3 = parseMultipart ( Request2, Headers, Callback );
        {done, Request2} ->
            ?PRINT ( "done" ),
            {ok, Request2};
        All -> 
            ?PRINT(All)
    end.

parseMultipart ( Request, Headers, Callback ) -> 
    Response = case cow_multipart:form_data ( Headers ) of
        {data, FieldName} ->
            {ok, Body, Request2} = cowboy_req:read_part_body ( Request ),
            data_payload(FieldName, Body),
            Request2;
        {file, FieldName, FileName, MimeType} -> 
            ?PRINT(FieldName),    
            ?PRINT(FileName),    
            ?PRINT(MimeType),
            runFile ( Request );
        All ->
            ?PRINT ( All )
    end,
    multipart ( Response, Callback ).

runFile ( Request ) -> 
    % TempFilename = temp_filename(),
    {ok, IoDevice} = file:open ( "filename.png", [raw, write] ),
    Rsf = stream_file ( Request, IoDevice, 0, ?MAX_FILE_SIZE_LIMIT ),
    ok = file:close(IoDevice),
    case Rsf of
        {ok, FileSize, Request2} ->
            ok = file_payload(FileSize),
            Request2;
        {limit, Reason, Request2} ->
            % Upload limit detected           
            % ok = file:delete ( TempFilename ),
            Request2
    end.

%
%   OLD
%
zmultipart(Req, C) ->
    zmultipart(Req, C, ?MAX_FILE_SIZE_LIMIT).
zmultipart(Req, C, MaxFileSizeLimit) when is_atom(C) and is_integer(MaxFileSizeLimit) ->

    case cowboy_req:read_part(Req) of
        {ok, Headers, Req2} ->
            Req5 = case cow_multipart:form_data(Headers) of
                {data, FieldName} ->
                    {ok, Body, Req3} = cowboy_req:part_body(Req2),
                    ok = C:data_payload(FieldName, Body),
                    Req3;
                {file, FieldName, Filename, _CType, _CTransferEncoding} ->
                    TempFilename = temp_filename(),
                    {ok, IoDevice} = file:open(TempFilename, [raw, write]),
                    Rsf = stream_file(Req2, IoDevice, 0, MaxFileSizeLimit),
                    ok = file:close(IoDevice),
                    case Rsf of
                            {ok, FileSize, Req4} ->
                                ok = C:file_payload(FieldName, Filename, TempFilename, FileSize),
                                Req4;
                            {limit, Reason, Req4} ->
                                error_logger:warning_msg("Upload limit detected! Type: ~p, FieldName: ~p, Filename: ~p,~nReq: ~p~n",
                                    [Reason, FieldName, Filename, Req4]),
                                ok = file:delete(TempFilename),
                                Req4
                    end
            end,
            multipart(Req5, C, MaxFileSizeLimit);
        {done, Req2} ->
            {ok, Req2}
    end.

stream_file(Req, IoDevice, FileSize, MaxFileSizeLimit) ->
    {Control, Data, Req2} = cowboy_req:read_part_body(Req),
    NewFileSize = byte_size(Data) + FileSize,
    case NewFileSize > MaxFileSizeLimit of
        true -> {limit, file_size, Req2};
        false ->
            ok = file:write(IoDevice, Data),
            case Control of
                ok -> {ok, NewFileSize, Req2};
                more -> stream_file(Req2, IoDevice, NewFileSize, MaxFileSizeLimit)
            end
    end.

temp_filename() ->
    list_to_binary(filename:join([?TMP_PATH, atom_to_list(?MODULE) ++ integer_to_list(erlang:phash2(make_ref()))])).
