
-module(erlstore_interface_http_server).

-include("../../dev.hrl").

- export([
    start/1
    ,stop/0
]).

start ( Port ) ->   

    case ranch_sup:start_link ( ) of
        {ok, RanchPid} -> RanchPid;
        {error,{already_started, RanchPid}} -> RanchPid
    end, 
    link ( RanchPid ),

    case cowboy_sup:start_link ( ) of
        {ok, CowboyPid} -> CowboyPid;
        {error,{already_started, CowboyPid}} -> CowboyPid
    end, 
    link ( RanchPid ),  
    
    Handlers = [             
        {"/domains/[:id]", erlstore_interface_http_domains_handler, [] }
        ,{"/files/[:id]", erlstore_interface_http_file_handler, [] }
        ,{"/users/[:id]", erlstore_interface_http_users_handler, [] }
        ,{"/tables/[:action]", erlstore_interface_http_tables_handler, [] }
        ,{"/auth/[:action]", erlstore_interface_http_auth_handler, [] }
        ,{"/admin", cowboy_static, {priv_file, erlstore, "www-admin/index.html", [{mimetypes, {<<"text">>, <<"html">>, []}}]} }
        ,{"/admin/[...]", cowboy_static, {priv_dir, erlstore, "www-admin", [{mimetypes, cow_mimetypes, all}] } }        
        ,{"/:table/:id", erlstore_interface_http_common_handler, [] }      
        ,{"/:table", erlstore_interface_http_common_handler, [] }
    ],
    Dispatch = cowboy_router:compile([
        {'_', Handlers }            
    ]),
    {ok, _} = cowboy:start_clear(http_listener,
        [{port, Port}],
        #{
            env => #{dispatch => Dispatch}
        }
    ).

stop ( ) ->
    ok.