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
-export([find_node/3, find_node/4]).
-export([find_value/3, find_value/4]).
-export([store/5, delete/2]).
-export([wait_rsp/1, wait_rsp_iter/2]).

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

%% @spec ping_first(ip_address(), integer(), bool()) -> {ok, Id} | {error, Reason} | {ok, KRef}
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
		    {ok, Id};
	       true ->
		    self() ! {KRef, ?PING_FIRST_RSP, Id},
		    {ok, KRef}
	    end; 
	false  ->
	    Msg = kad_protocol:gen_cmd(?PING_FIRST, <<>>, []),
	    case send_msg(Addr, Port, KRef, Msg) of
		ok ->
		    if Sync ->
			    wait_rsp(?PING_FIRST_RSP, Timeout);			    
		       true  ->
			    {ok, KRef}
		    end;
		{error, Reason} ->
		    {error, {ping_error, Reason}}
	    end
    end.

%% @spec ping(identify()) -> ok | {error, Reason}
%% @doc ping the Node, check if it's online
ping(Node, Addr, Port, Sync) when is_binary(Node)  ->
    ?LOG("ping the node:~p ~p:~p~n", [Node, Addr, Port]),
    IsSelf = is_self(Node),
    case IsSelf of
	true ->
	    ok;
	false -> 
	    % generate the msg
	    Msg = kad_protocol:gen_cmd(?PING, Node, []),
	    case kad_net:send(Addr, Port, Msg) of
		ok ->
		    if Sync -> % sync wait the response
			    wait_rsp(?PING_RSP);
		       true ->
			    ok
		    end;
		{error, Reason} ->
		    {error, {ping_error, Reason}}
	    end
    end.

find_node(Node, Sync, Discard) when is_binary(Node) ->
    ?LOG("find_node start:~p~n", [Node]),
    %get a closest nodes
    {N, Closest} = kad_routing:closest(Node, ?A),
    ?LOG("get ~p Closest Node~n", [N]),
    {Success, Failed} = send_find_to_nodes(Closest, ?FIND_NODE, Node),
    % process the contacts which send request error
    process_req_error(Failed),
    if Sync ->
	    % wait the response
	    wait_rsp_iter(Node, kad_searchlist:new(Node));
       true ->
	    ok
    end.

%% @spec find_value(key(), integer()) -> {ok, Value} | {error, Reason}
%% @doc find the value corresponding the key
find_value(Key, 1, Sync) when is_binary(Key) ->
    ?LOG("find value start:~p\n", [Key]),
    case kad_store:lookup(Key) of
	{value, Value} -> % is store in lcoal, just return the value
	    ?LOG("the value is in local, just return\n"),
	    {ok, Value};
	none -> % is not in local
	    % send request
	    Ret = send_to_closest_nodes(Key, ?FIND_VALUE),
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


%% send cmd  to nodes
send_find_to_nodes(Nodes, Cmd, Key) ->
    % send reqeust
    Ret = 
    lists:map(fun(#kad_contact{id = Id, ip = Addr, port = Port}) -> 
		         % gen the msg
			 Msg = kad_protocol:gen_cmd(Cmd, Id, Key),
			 kad_net:send(Addr, Port, Msg)
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
wait_rsp(?PING_FIRST_RSP) ->
    receive 
	{KRef, 
wait_rsp(?PING_RSP) ->
    receive
	{_Parent, ?PING_RSP, Msg} ->
	    case Msg of
			none ->
				ok;
			Key ->
				{ok, Key}
		end
    after ?PTIMEOUT ->
	    {error, timeout}
    end;
wait_rsp(?FIND_VALUE_RSP) ->
    receive 
	{_Parent, ?FIND_VALUE_RSP, Data} ->
	    if is_binary(Data) ->
		    {ok, Data};
	       is_list(Data) ->
		    resolve_find_node(Data)
	    end
    after ?PTIMEOUT ->
	    {error, timeout}
    end;
wait_rsp(?STORE_RSP) ->
    receive 
	{_Parent, ?STORE_RSP, Data} ->
	    {ok, Data}
    after ?PTIMEOUT ->
	    {error, timeout}
    end.

%% wait the find_node response 
wait_rsp_iter(Target, SearchList) ->
    receive 
	{Parent, ?FIND_NODE_RSP, Msg} ->
	    case find_iter_stop(Msg, SearchList) of
			true ->	% select the k un-queried nodes send find_node msg
				KNodes = kad_searchlist:closest(?K, true, SearchList),
				{Lives, Failed} = send_find_to_nodes(KNodes, ?FIND_NODE, Target),
				NewNodes = wait_k_rsp(?FIND_NODE_RSP, length(KNodes)),
				SL2 = add_searchlist(NewNodes, SearchList),
				kad_searchlist:closest(?K, false, SL2);
			false -> % continue request to get closest nodes
				SL2 = add_searchlist(Msg, SearchList),
				ANodes = kad_searchlist:closest(?A, true, SL2),
				send_find_to_nodes(ANodes, ?FIND_NODE, Target),
				wait_rsp_iter(Target, SL2)
	   end
    %afetr 10000 ->
	%	{error, timeout}
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
    end;

%% add the nodes to search list
add_searchlist(Nodes, Search) ->
FAdd = fun(#kad_contact{} = Node, List) ->
	       kad_searchlist:add(Node, List)
       end,
    lists:foldl(FAdd, Search, Nodes).


%% send msg
send_msg(Addr, Port, KRef, Msg) ->
    case kad_net:send(Addr, Port, Msg) of
	ok -> % add the msg to the rpc manager
	    kad_rpc_mgr(
	    ok;
	Error ->
	    Error
    end.
