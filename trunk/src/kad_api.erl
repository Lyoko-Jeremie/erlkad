%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the kademlia api2
-module(kad_api).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").
-include("kad_protocol.hrl").

-export([bootstrap/1]).
-export([ping_first/3, ping_first/4]).
-export([ping/4, ping/5]).
-export([find_node/4, find_node/4]).
-export([find_value/3, find_value/4]).
-export([store/5, delete/2]).
-export([wait_rsp/3, wait_rsp_iter/5]).

%% @doc bootstrap the kad
bootstrap({Addr, Port} = G) ->
    case kad_api:ping(<<>>, Addr, Port, true) of
	{ok, Id} ->
	    % find self
	    kad_api:find_node(kad_node:id(), false, true);	    
	{error, Reason} ->
	    % pint the gateway error
	    ?LOG("bootstrap ping the gateway error:~p\n", [Reason]),
	    {error, Reason}
    end;		
			
bootstrap([_|_] = _G) ->
    ?NOT_IMPL.

%% @spec ping_first(ip_address(), integer(), bool()) -> {value, Id} | {error, Reason} | {ok, KRef}
%% @doc first time ping the node, now we don't know the node's id
%%      the receipt return the self id. if the Sync is false, the {ok, KRef} will return,
%%      the send process will receive the msg {KRef, Cmd, Msg}
ping_first(Addr, Port, Sync) ->
    ping_first(Addr, Port, Sync, infinity).

ping_first(Addr, Port, Sync, Timeout) ->
    ?LOG("first ping the node:~p:~p\n", [Addr, Port]),
    IsSelf = is_self({Addr, Port}),
    KRef = make_ref(),
    case IsSelf of
	true ->
	    Id = kad_node:id(),
	    if Sync ->
		    {value, Id};
	       true ->
		    self() ! {KRef, ?PING_FIRST_RSP, Id},
		    {ok, KRef}
	    end; 
	false  ->
	    MsgId = kad_rpc_mgr:msgid(),
	    Msg = kad_protocol:gen_msg(?PING_FIRST, dummy, MsgId, dummy),
	    case send_msg(Addr, Port, KRef, MsgId, Msg) of
		ok ->
		    if Sync ->
			    case wait_rsp(?PING_FIRST_RSP, KRef, Timeout) of
				{error, Reason} ->
				    {error, Reason};
				Rsp ->
				    {value, Rsp}
			    end;
		       true  ->
			    {ok, KRef}
		    end;
		Other ->
		    Other	       
	    end
    end.

%% @spec ping(identify(), ip_address(), integer(), bool()) -> {value, Rsp} | {error, Reason} | {ok, KRef}
%% @doc ping the Node, check if it's online
ping(Node, Addr, Port, Sync) ->
    ping(Node, Addr, Port, Sync, infinity).
ping(Node, Addr, Port, Sync, Timeout) when is_binary(Node)  ->
    ?LOG("ping the node:~p ~p:~p~n", [Node, Addr, Port]),
    IsSelf = is_self(Node),
    KRef = make_ref(),
    case IsSelf of
	true ->
	    Id = kad_node:id(),
	    if Sync ->
		    {value, Id};
	       true ->
		    self() ! {KRef, ?PING_RSP, Id},
		    {ok, KRef}
	    end;
	false -> 
	    MsgId = kad_rpc_mgr:msgid(),
	    Msg = kad_protocol:gen_msg(?PING, Node, MsgId, dummy),
	    case send_msg(Addr, Port, KRef, MsgId, Msg) of
		ok ->
		    if Sync -> % sync wait the response
			    case wait_rsp(?PING_RSP, KRef, Timeout) of
				{error, Reason} ->
				    {error, Reason};
				Rsp ->
				    {value, Rsp}
			    end;
		       true ->
			    {ok, KRef}
		    end;
		Other ->
		    Other
	    end
    end.


%% find the nodes close to the Id
%% return {ok, KRef} if the Sync is false, and then the Caller will receive the msg
%%               {KRef, Cmd, Data}
%% return {value, Value} if Sync is true
%% return {error, Reason} if error
find_node(Id) when is_binary(Id) ->
    find_node(Id, infinity). 
find_node(Id, Timeout) ->
    find_node(Id, Timeout, true, false).
find_node(Id, Timeout, Sync, Discard) ->
    ?LOG("find_node start:~p~n", [Id]),
    %get alpha closest nodes
    {N, Closest} = kad_routing:closest(Id, ?A),
    ?LOG("get ~p Closest Node~n", [N]),
    KRef = make_ref(),
    Caller = self(),
    case Sync of
       true ->
           {Success, Failed} = send_find_to_nodes(?FIND_NODE, KRef, Id, Closest, ),
           % process the contacts which send request error
            process_req_error(Failed),
	    TimerId = kad_util:start_timer(Timeout, self(), ok),
            wait_rsp_iter(KRef, Id, kad_searchlist:new(Id), Discard, TimerId);	
      false -> % is async
          Receiver = 
	  proc_lib:spawn(fun() ->
	         Mref = erlang:monitor(process, Caller),
		 receive 
		     {Caller, Tag} ->  % start the find
			{Success, Failed} = send_find_to_nodes(?FIND_NODE, KRef, Id, Closest, ),
			 % process the contacts which send request error
			 process_req_error(Failed),      
			 TimerId = kad_util:start_timer(Timeout, self(), ok),
			 Ret = wait_rsp_iter(KRef, Id, kad_searchlist:new(Id), Discard, TimerId),
			 case Discard of
			     true ->
			        exit(normal);
			     false ->
			        % send the result to the caller
			        Caller ! {KRef, Cmd, Ret}
			end;
		    {'DOWN',Mref,_,_,_} -> % Caller died before sending us the go-ahead.
			  %% Give up silently.
		         exit(normal)
		end
	    end),
	 Receiver ! {Caller, KRef},
	 {ok, KRef}
    end.

%% find the value corresponding the key
find_value(Key, 1, Sync) ->
    find_value(Key, 1, Sync, infinity).
find_value(Key, 1, Sync, Timeout) when is_binary(Key) ->
    ?LOG("find value start:~p\n", [Key]),
    case kad_store:lookup(Key) of
	{value, Value} -> % is store in lcoal, just return the value
	    ?LOG("the value is in local, just return\n"),
	    {ok, Value};
	none -> % is not in local
	    % send request
	    {N, Closest} = kad_routing:closest(Key, ?A),
	    KRef = make_ref(),
	    Ret = send_find_to_nodes(?FIND_VALUE, KRef, Key, Closest, false),
	    Ret,
	    % wait the response
	    wait_rsp(?FIND_VALUE_RSP)
    end.

%% @spec store(ip_address(), integer(), identify(), key(), binary()) -> ok | {error, Reason}
%% @doc store the key-value pair in kad network
store(Addr, Port, Node, {Key, Data}, Sync) when is_binary(Key) andalso is_binary(Data) ->
    ?LOG("store the key-value:~p to:~p\n", [{Key, Data}, Node]),
    Msg = kad_protocol:gen_cmd(?STORE, Node, {Key, Data}),
    case kad_net:send(Addr, Port, Msg) of
		ok ->
			case Sync of
				true -> 
					wait_rsp(?STORE_RSP);
				false ->
					ok
			end;
		{error, Reason} ->
			{error, Reason}
	end.

delete(_Key, _Sync) ->
    ok.

%%
%% internal API
%%

%% if the Node is ourself
is_self(Node) ->
    Node =:= kad_node:id();
is_self({_Ip, _Port} = Addr) ->
    Addr =:= kad_node:address().

%% send msg
send_msg(Addr, Port, KRef, MsgId, Msg) ->
    send_msg(Addr, Port, KRef, MsgId, Msg, self()).
send_msg(Addr, Port, KRef, MsgId, Msg, Caller) ->
    case kad_net:send(Addr, Port, Msg) of
	ok -> % add the msg to the rpc manager
	    kad_rpc_mgr:add(KRef, MsgId, kad_rpc_mgr:msgdata(Caller)),
	    ok;
	Error ->
	    Error
    end.

%% send cmd to nodes
send_find_to_nodes(Cmd, KRef, Node, Nodes, Caller) ->
    % send reqeust
    Ret = 
    lists:map(fun(#kad_contact{id = Dest, ip = Addr, port = Port}) -> 
		         % gen the msg
		         MsgId = kad_rpc_mgr:msgid(),
			 Msg = kad_protocol:gen_msg(Cmd, Dest, Msgid, Node),
		         send_msg(Addr, Port, KRef, MsgId, Msg, Caller)			 
	      end, 
	      Nodes),
    FSuccess = fun(ok) -> true;
		  ({error, _}) -> false
	       end,				
    lists:partition(FSuccess, Ret).

%% process the contacts which send request error
process_req_error(_Ret) ->
    ok.

%% wait the rsp
wait_rsp(Cmd, KRef, Timeout) ->
    receive 
	{KRef, Cmd, Msg} ->
	    Msg
    after Timeout ->
	    {error, timeout}
    end.

%% wait the find_node response 
wait_rsp_iter(KRef, Target, SearchList, Discard, TimerId) ->
    wait_rsp_iter1(Kref, Target, SearchList, Discard, TimerId).

wait_rsp_iter1(KRef, Target, SearchList, Discard, TimerId)
    receive 
	{KRef, ?FIND_NODE_RSP, Msg} ->
	    case find_iter_stop(Msg, SearchList) of
		true ->	% select the k un-queried nodes send find_node msg
		    KNodes = kad_searchlist:closest(?K, true, SearchList),
		    {Lives, Failed} = send_find_to_nodes(KNodes, ?FIND_NODE, Target),
		    NewNodes = wait_k_rsp(?FIND_NODE_RSP, length(KNodes)),
		    SL2 = add_searchlist(NewNodes, SearchList),
		    Ret = kad_searchlist:closest(?K, false, SL2),
		    {value, Ret};
		false -> % continue request to get closest nodes
		    SL2 = add_searchlist(Msg, SearchList),
		    ANodes = kad_searchlist:closest(?A, true, SL2),
		    send_find_to_nodes(?FIND_NODE, KRef, Target, ANodes, Discard),		    
		    wait_rsp_iter(KRef, Target, SL2, Discard, Timeout)
	    end;
       {timeout, TimerId, _} ->
            {error, timeout}
    end.


find_iter_stop(Nodes, SearchList) ->
	FCloser = fun(#kad_contact{id = Id}) ->
					kad_searchlist:is_closer(Id, SearchList)
				end,
	not lists:any(FCloser, Nodes).


wait_k_rsp(Cmd, N, SearchList) ->	
    wait_k_rsp(Cmd, N, []).

wait_k_rsp(Cmd, 0, Acc) ->
    Acc;
wait_k_rsp(Cmd, N, Acc) ->
    receive 
	{_Parent, Cmd, Data} ->
	    wait_k_rsp(Cmd, N - 1, [Data | Acc])
    after 100000 ->
	    {error, timeout}
    end.

%% add the nodes to search list
add_searchlist(Nodes, Search) ->
FAdd = fun(#kad_contact{} = Node, List) ->
	       kad_searchlist:add(Node, List)
       end,
    lists:foldl(FAdd, Search, Nodes).
