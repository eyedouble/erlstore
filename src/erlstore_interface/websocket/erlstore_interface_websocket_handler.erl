
-module(erlstore_interface_websocket_handler).

-behaviour(cowboy_handler).

-include("dev.hrl").

-export([
    init/2
    ,websocket_init/1
    ,websocket_handle/2
    ,websocket_info/2
]).

init(Req, Opts) ->
	?PRINT(self()),
	?PRINT(Opts),
	{cowboy_websocket, Req, Opts}.

% Init connection
websocket_init(State) ->
	io:fwrite( " CONN INIT \n" ),
	?PRINT ( State ),
	erlang:start_timer(1000, self(), <<"Hello!">>),
	{ok, State}.


% Handle incoming
websocket_handle({text, Data }, State) ->	
	handle ( jsone:decode ( Data ), State );

websocket_handle(_Data, State) ->
	?PRINT( kak ),
    {ok, State}.

% PING
websocket_info({timeout, _Ref, Msg}, State) ->
	?PRINT("PING"),	
	erlang:start_timer(25000, self(), <<"Heartbeat">>),
	{reply, {ping, ""}, State};


websocket_info( {2000, {Status, Data } }, State) ->
	respond ( #{ status => Status, data => Data }, State );

websocket_info(Info, State) ->
	?PRINT(Info),
	{ok, State}.
	% {_,_,Msg}=Info,
	% {reply, {text, Msg}, State}.


handle ( Data=#{ <<"action">> := <<"subscribe">>, <<"table">> := Table }, State ) ->
	?PRINT ( Table ),
	Subscription = persistence:subscribe ( self(), binary_to_atom ( Table, utf8 ) ),
	respond ( #{ status => 2000, data => is_pid ( Subscription ) }, State ).


respond ( Data, State ) ->
	{reply, {text, jsone:encode ( Data ) }, State}.

