%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the kademlia rpc manager
-module(kad_rpc_mgr).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").
-include("kad_protocol.hrl").

-behaviour(gen_server).
-export([start_link/0]).
-export([msgid/0]).
-export([msgdata/0, msgdata/1]).
-export([add/3, exist/1]).
-export([dispatch/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-record(msgdata, {
	  start_stamp, % msg kick off time
	  pid         % the msg send process pid	  
	  }).
	  

-record(item, {
	  ref, % the msg reference
	  id,  % the msg id
	  data % the msg data
	 }).


-record(state, {
	  msgid % the msg id to generate the next id
	  }).

	  
-define(RPCTABLE, rpc_table).
-define(SERVER, ?MODULE).

-type msgid() :: id().
-type msgdata() :: #msgdata{}.

%% @doc start the kad udp socket
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc return the next msg id to use
-spec msgid() -> msgid().
msgid() ->
    gen_server:call(?SERVER, msgid).

%% @doc create msgdata 
-spec msgdata() -> msgdata().
msgdata() -> msgdata(self()).
msgdata(Caller) ->
    #msgdata{pid = Caller, start_stamp = now()}.

%% @doc add a msg invoke to rpc manager
-spec add(KRef :: ref(), MsgId :: msgid(), MsgData :: msgdata()) -> 'ok'.
add(KRef, MsgId, MsgData) when is_reference(KRef) andalso is_binary(MsgId) ->
    gen_server:cast(?SERVER, {add, KRef, MsgId, MsgData}).

%% @doc return true if the msg exist in manager, otherwise return false
-spec exist(MsgId :: msgid()) -> bool().
exist(MsgId) ->
    gen_server:call(?SERVER, {exist, MsgId}).


%% @doc dispatch the msg
-spec dispatch(MsgId :: msgid(), Src :: id(), Cmd :: cmd(), Msg :: binary()) -> 'ok'.
dispatch(MsgId, Src, Cmd, Msg) ->
    gen_server:cast(?SERVER, {dispatch, MsgId, Src, Cmd, Msg}).

%%
%% gen_server callback
%% 
init(_Args) ->
    ets:new(?RPCTABLE, [set, private, named_table, {keypos, #item.id}]),
    State = #state{msgid = kad_util:randid()},
    {ok, State}.

handle_call(msgid, _From, State) ->
    Id = kad_util:idinc(State#state.msgid),
    {reply, Id, State#state{msgid = Id}};    
handle_call({exist, MsgId}, _From, State) ->
    Reply =
    case ets:lookup(?RPCTABLE, MsgId) of
	[] ->
	    false;
	[_|_] ->
	    true
    end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({add, KRef, MsgId, MsgData = #msgdata{}}, State) ->
    ets:insert(?RPCTABLE, #item{ref = KRef, id = MsgId, data = MsgData}),
    {noreply, State};
handle_cast({dispatch, MsgId, _Src, Cmd, Msg}, State) ->
    case ets:lookup(?RPCTABLE, MsgId) of
	[#item{ref = KRef, data = #msgdata{pid = Pid}}] ->
	    do_notify_msg(Pid, KRef, Cmd, Msg),
	    % remove this entry
	    do_remove_entry(MsgId);
	[] ->
	    ?LOG("this msg:~p is not exist in rpc manager~n", [MsgId])	    
    end,
    {noreply, State};    
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

do_notify_msg(Pid, KRef, Cmd, Msg) when is_pid(Pid) ->
    case is_process_alive(Pid) of
	true ->
	    Pid ! {KRef, Cmd, Msg};
	false ->
	    {error, noproc}
    end.


do_remove_entry(MsgId) ->
    ets:delete(?RPCTABLE, MsgId).
