%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the kademlia rpc manager
-module(kad_rpc_mgr).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").

-behaviour(gen_server).
-export([start_link/0]).
-export([msgid/0]).
-export([msgdata/1, msgdata/2]).
-export([add/2, exist/1]).
-export([dispatch/4]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-record(msgdata, {
	  start_stamp, % msg kick off time
	  pid,         % the msg send process pid
	  discard = false  % if discard the result
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

%% @type msgdata() = #msgdata{}
%% @type cmd() = integer()


%% @doc start the kad udp socket
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @spec msgid() -> identify()
%% @doc return the next msg id to use
msgid() ->
    gen_server:call(?SERVER, msgid).

%% @spec msgdata(pid(), bool()) -> msgdata()
%% @doc create msgdata 
msgdata(Sender, Discard) ->
    #msgdata{pid = Sender, discard = Discard, start_stamp = now()}.

msgdata(Sender) ->
    msgdata(Sender, false).

%% @spec add(reference(), identify(), msgdata()) -> Ret
%% @doc add a msg invoke to rpc manager
add(KRef, MsgId, MsgData) is_reference(KRef) andalso is_binary(MsgId) ->
    gen_server:cast(?SERVER, {add, KRef, MsgId, MsgData}).


%% @spec exist(identify()) -> bool()
%% @doc return true if the msg exist in manager, otherwise return false
exist(MsgId) ->
    gen_server:call(?SERVER, {exist, MsgId}).


%% @spec dispatch(identify(), identify(), cmd(), binary()) -> ok
%% @doc dispatch the msg
dispatch(MsgId, Src, Cmd, Msg) ->
    gen_server:cast(?SERVER, {dispatch, MsgId, Src, Cmd, Msg}).

%%
%% gen_server callback
%% 
init(_Args) ->
    est:new(?RPCTABLE, [set, private, named_table, {keypos, #item.id}]),
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
	[#item{ref = KRef, data = #msgdata{pid = Pid, discard = Discard}}] ->
	    do_notify_msg(Pid, KRef, Cmd, Msg, Discard);	
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

do_notify_msg(_Pid, _KRef, _Cmd, _Msg, true) ->
    ?LOG("discard the msg result\n"),
    ok;
do_notify_msg(Pid, KRef, Cmd, Msg, false) when is_pid(Pid) ->
    Pid ! {KRef, Cmd, Msg}.
