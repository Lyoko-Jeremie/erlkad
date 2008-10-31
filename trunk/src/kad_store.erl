%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the store module
-module(kad_store).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").
-behaviour(gen_server).

-export([start_link/0]).
-export([lookup/1, store/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).


-define(SERVER, ?MODULE).

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec lookup(Key :: key()) -> {'value', Value} | 'none'.
lookup(Key) ->
    gen_server:call(?SERVER, {lookup, Key}).
	
-spec store(Key :: key(), Data :: data()) -> 'ok'.
store(Key, Data) ->
    gen_server:cast(?SERVER, {store, Key, Data}).

%%
%% gen_server callback
%% 
init(_Args) ->
    ets:new(kad_data, [set, protected, named_table, {keypos, 1}]),
    {ok, []}.

handle_call({lookup, Key}, _From, State) ->
    Reply = 
    case est:lookup(kad_data, Key) of
	[Value] ->
	    {value, Value};
	[] ->
	    none
    end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({store, Key, Data}, State) ->
    ets:insert(kad_data, {Key, Data}),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

