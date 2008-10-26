%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the kademlia application and supervisor
-module(kad).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").
-behaviour(application).
-behaviour(supervisor).

-export([start/2, stop/1]).
-export([init/1]).

start(_Type, _Args) ->  
    case application:get_env(ip) of
	undefined ->
	    ?LOG("must specify the ip arg~n"),
	    {error, badarg};
	{ok, Addr} ->
	    Port = get_env(port, ?KAD_PORT),
	    Virtual = get_env(virtual, 1),
	    supervisor:start_link({local, kad_sup}, ?MODULE, {Addr, Port, Virtual})
    end.

stop(_Args) ->
    ok.

%% supervisor callbacks
init({Addr, Port, Virtual}) ->
    ParsedIp =
    if is_list(Addr) ->
	    {ok, IpTuple} = inet_parse:address(Addr),
	    IpTuple;
       is_tuple(Addr) ->
	    Addr;
       true ->
	    {0,0,0,0}
    end,
    ?LOG("start supervisor init:~p~n", [{ParsedIp, Port, Virtual}]),
    Stragegy = {one_for_one, 10, 10},
    Node = {kad_node, {kad_node, start_link, [ParsedIp, Port, Virtual]},
	    transient, 1000, worker, [kad_node]},
    Net = {kad_net, {kad_net, start_link, [[binary, {ip, ParsedIp}, {port, Port}]]},
	   transient, 1000, worker, [kad_net]},
    Rpc = {kad_rpc_mgr, {kad_rpc_mgr, start_link, []},
	   transient, 1000, worker, [kad_rpc_mgr]},
    Routing = {kad_routing, {kad_routing, start_link, []},
	       transient, 1000, worker, [kad_routing]},
    Store = {kad_store, {kad_store, start_link, []}, 
	     transient, 1000, worker, [kad_store]},
    {ok, {Stragegy, 
	  [Node, Net, Rpc, Routing, Store]
	 }}.
%%
%% internal API
%%

%% get the env
get_env(Par, Def) ->
    case application:get_env(Par) of
	{ok, Val} ->
	    Val;
	undefined ->
	    Def
    end.


%% get the ip
get_ip() ->
    {ok, Name} = inet:gethostname(),
    {ok, Addr} = inet:getaddr(Name, inet),
    Addr.
