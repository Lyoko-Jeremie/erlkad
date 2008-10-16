%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc coordinator the msg
-module(kad_coordinator).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").
-include("kad_protocol.hrl").

-export([dispatch/3]).

%% @spec dispatch(ip_address(), integer(), binary()) -> ok | {error, Reason}
%% @doc dispatch the received msg from socket, *MUST NOT* block
dispatch(Addr, Port, Packet) when is_binary(Packet) ->
    Self = kad_node:id(),
    case kad_protocol:parse(Packet) of
        {Cmd, Dest, Src, Id, Data} when Dest =:= Self ->
	    case kad_protocol:optype(Cmd) of
		?OP_REQ ->
		    %% it's request
		    may_update_bucket(Cmd, Id, Addr, Port),
		    %% reply the cmd(non block)
		    reply(Addr, Port, Src, Id, Cmd, Data);
		?OP_RSP ->
		    %% it's response
		    % rpc manager dispatch the msg
		    case kad_rpc_mgr:dispatch(Id, Src, Cmd, Data) of
			ok ->
			    ?LOG("msg success dispatch~n"),
			    ok;
			{error, noproc} ->
			    ?LOG("msg is not mine~n"),
			    {error, noproc}
		    end
	    end;
	    

	ignore -> 
	    ?LOG("msg parse error ignore:~p~n", [Packet]),
	    ok;
	_ -> 
	    ?LOG("msg invalid~n"),
	    ok
    end.   

%% 
%% internal API
%% 

%% if this msg can update the bucket
may_update_bucket(Cmd, Id, Addr, Port) when Cmd /= ?PING ->
    kad_routing:update(kad_node:new_node(Id, Addr, Port));
may_update_bucket(_Cmd, _Id, _Addr, _Port) ->
    ok.


%% reply the request, spawn new process
reply(Addr, Port, Dest, Id, Cmd, Msg) ->
    prob_lib:spawn(fun() ->	 
		       case do_reply(Dest, Id, Cmd, Msg) of
			   {ok, Rsp} ->
			       kad_net:send(Addr, Port, Rsp);
			   _ ->
			       ?LOG("the request msg is ingore[~p:~p]\n", [?MODULE, ?LINE]),
			       exit(normal)
		       end
		       
		   end).

%% do the reply
do_reply(Dest, Id, ?PING, _Msg) ->
    kad_protocol:gen_rsp(?PING_RSP, Dest, Id, dummy);
do_reply(Dest, Id, ?FIND_NODE, Node) ->
    {N, Closest} = kad_rotining:closest(Node, ?K),
    ?LOG("response ~p closest nodes to ~p:~p\n", [N, Dest, Closest]),
    kad_protocol:gen_rsp(?FIND_NODE_RSP, Dest, Id, Closest);
do_reply(Dest, Id, ?FIND_VALUE, Key) ->
    % if the key exist in local store
    case kad_store:lookup(Key) of
	{value, Value} ->
    	    ?LOG("response ~p FIND_VALUE msg. [key:~p data:~p]\n", [Dest, Key, Value]),
	    kad_protocol:gen_rsp(?FIND_VALUE_RSP, Value);
	none ->
	    do_reply(Dest, Id, ?FIND_NODE, Key)
    end;
do_reply(Dest, Id, ?STORE, {Key, Data}) ->
    case kad_store:store(Key, Data) of
	ok ->
	    kad_protocol:gen_rsp(?STORE_RSP, Dest, Id, ?E_SUCCESS);
	{error, _Reason} ->
	    kad_protocol:gen_rsp(?STORE_RSP, Dest, Id, ?E_FAILED)
    end;
do_reply(Dest, Id, ?DELETE, _Key) ->
   kad_protocol:gen_rsp(?DELETE, Dest, Id, ?E_NOTIMPL).
