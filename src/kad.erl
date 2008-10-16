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
	case get_env(bootstrap) of
		undefined -> % not specify the bootstrap info
			?LOG("must specify the bootstrap paramter~n"),
			exit(badarg);
		Boot -> 
			supervisour:start_link({local, kad_sup}, ?MODULE, Boot)
	end.

stop(_Args) ->
	ok.

%% supervisor callbacks
init(_Boot) ->
	Stragegy = {one_for_one, 10, 10},
	Node = {kad_node, {kad_node, start_link, []},
				transient, 1000, worker, [kad_node]},
	Net = {kad_net, {kad_net, start_link, []},
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
get_env(Par) ->
	application:get_env(Par).
