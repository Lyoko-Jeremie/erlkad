%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc bootstrap
-module(kad_bootstrap).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").

-export([start/1]).

start(Args) ->
	proc_lib:spawn_link(fun() -> bootstrap(Args) end).

%%
%% internal API
%%
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
			
bootstrap([_|_] = G) ->
	?NOT_IMPL.
	%lists:map(fun({Addr, Port}) -> kad_api:ping(<<>>, Addr, Port, false) end, G),
	%case wait_rsp(?PING_RSP) of	
	%	{ok, Id} ->
			% find self
	%		kad_api:find_node(kad_node:id());
