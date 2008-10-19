%%% @author litao cheng litaocheng@gmail.com
%%% @copyright 2008 toquick.com.
%%% @doc the kad node stand for self
-module(kad_node).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").
-behaviour(gen_server).

-export([start_link/3]).
-export([id/0, address/0, contact/0, virtual/0]).
-export([distance/1, distance/2]).
-export([new_node/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-record(state, {
	  node,
	  virtual % virtual node id
	 }).
	  
-define(SERVER, ?MODULE).

start_link(Addr, Port, Virtual) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, {Addr, Port, Virtual}, []).

%% @spec id() -> identify()
%% @doc the self node id
id() ->
    gen_server:call(?SERVER, {get, id}).


%% @spec address() -> {ip_address(), integer()}
%% @doc return the ip and port tuple
address() ->
    gen_server:call(?SERVER, {get, address}).

%% @spec contact() -> identify()
%% @doc the self node contact info
contact() ->
    gen_server:call(?SERVER, {get, contact}).

%% @spec virtual() -> integer()
%% @doc return the self virtual id
virtual() ->
    gen_server:call(?SERVER, {get, virtual}).

%% @spec distance(identify()) -> identify()
%% @doc return the distance between X and self( based XOR)
distance(X) ->
    distance(id(), X).

%% @spec distance(identfiy(), identify()) -> identify()
%% @doc return the distance between X and Y
distance(X, Y) ->
    kad_util:distance(X, Y).

%% @spec new_node(identify(), ip_address(), integer()) -> kad_contact()
%% @doc return new node
new_node(Id, Addr, Port) ->
    #kad_contact{id = Id, ip = Addr, port = Port}.

%% gen_server callbacks
init({Addr, Port, Virtual}) ->
    Id = gen_nodeid(Addr, Port, Virtual),
    Node = #kad_contact{id = Id, ip = Addr, port = Port},	
    {ok, #state{node = Node, virtual = Virtual}}.

handle_call({get, id}, _From, State) ->
    Node = State#state.node,
    {reply, Node#kad_contact.id, State};
handle_call({get, address}, _From, State) ->
    #kad_contact{ip = Ip, port = Port} = State#state.node,
    {reply, {Ip, Port}, State};    
handle_call({get, virtual}, _From, State) ->
    {reply, State#state.virtual, State};
handle_call({get, contact}, _From, State) ->
    {reply, State, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.


%%
%% internal API
%%

gen_nodeid({D1, D2, D3, D4}, Port, Virtual) ->
    %{A, B, C} = now(),
    kad_util:id(<<D1, D2, D3, D4, Port:16, Virtual>>).
