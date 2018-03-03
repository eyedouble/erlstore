
-module(erlstore_interface_auth).

-include("dev.hrl").

-export([
    password/3
    ,token/4
    ,token/3
    ,credentials/3
]).

-spec password(hash, binary(), binary()) -> binary().
password ( hash, Password, Salt ) ->
    {ok, Hash} = generate_pbkdf2 ( Password, Salt, 4096, 20 ),
    Hash.

-spec token(generate, map(), binary(), integer()) -> binary().
token ( generate, Claims, Key, ExpirationTime ) ->   
    {ok, Token} = jwt:encode ( <<"HS256">>, #{ data => Claims }, ExpirationTime, Key ),
    Token.

-spec token(verify, binary(), binary()) -> boolean(); 
           (decode, binary(), binary()) -> tuple().
token ( verify, Token, Key ) ->
    {Status, _Data} = token ( decode, Token, Key ),
    Status;

token ( decode, Token, Key ) ->
    {Status, Data} = jwt:decode ( Token, Key ).


credentials ( check, {Username, StoredUsername}, {Password, Salt, Hash} ) ->
    Username =:= StoredUsername andalso password ( hash, Password, Salt ) =:= Hash.


%
% Private
%
generate_pbkdf2 ( String, Salt, Iterations, DerivedLength ) ->
    {ok, IntKey} = pbkdf2:pbkdf2(sha256, String, Salt, Iterations, DerivedLength),
    Key = binary_to_hexstring ( IntKey ),
    { ok, Key }.

binary_to_hexstring ( <<X:160/big-unsigned-integer>> ) ->      
        Intermediary = io_lib:format("~40.16.0b", [X]),
        list_to_binary ( Intermediary ).