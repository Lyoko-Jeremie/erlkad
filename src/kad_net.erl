%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the kademlia udp manager
-module(kad_net).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").
-include("kad_protocol.hrl").

-behaviour(gen_server).
-export([start_link/0, start_link/1, stop/1]).
-export([socket/0]).
-export([send/3]).
-export([kad_net_loop/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-record(state, {
            socket :: socket(),     % the udp socket
	    pid :: pid()        % the udp process pid
	    }).

-define(SERVER, ?MODULE).

%% @doc start the kad udp socket
start_link() ->
    start_link([]).

%% @doc start the kad udp socket
%%   Opts = [Option]
%%   Option = {port, Port} 
%% @see inet:setopts/2
-spec start_link(Opts :: list()) -> any().
start_link(Opts) when is_list(Opts) ->
    ?LOG("start kad_net :~p~n", [Opts]),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @spec stop(term()) -> ok
%% @doc stop the kad udp server
stop(Reason) ->
    gen_server:cast(?SERVER, {stop, Reason}).

%% @doc return the socket
-spec socket() -> socket().
socket() ->
    gen_server:call(?SERVER, socket).

%% @doc send the msg
-spec send(Addr :: ip_address(), Port :: ip_port(), Msg :: msg()) -> 'ok' | {'error', any()}.
send(Addr, Port, Msg) ->
    ?LOG("send msg:[~p:~p] ~p\n", [Addr, Port, Msg]),
    Socket = socket(),
    gen_udp:send(Socket, Addr, Port, Msg).

%%
%% gen_server callback
%% 
init(Opts) ->
    process_flag(trap_exit, true),
    {Port, UdpOpts} = parse_opt(Opts),
    case gen_udp:open(Port, UdpOpts) of
        {ok, Socket} ->
	    ?LOG("kad open udp port:~p socket:~p~n", [Port, Socket]),
            Pid = proc_lib:spawn_link(?MODULE, kad_net_loop, [Socket]),
            State = #state{socket = Socket, pid = Pid},
	    {ok, State};
	{error, Reason} ->
	    {stop, Reason}
    end.

handle_call(socket, _From, State) ->
    {reply, State#state.socket, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({stop, Reason}, State) ->
    ?LOG("pre stop the kad udp socket:~p~n", [Reason]),
    State#state.pid ! {stop, Reason, self()},
    case ?WAIT_MSG(stop, [dummy], 1000) of
        {error, _} ->
	    ?LOG("stop udp process error~n"),
	    {stop, ok, State};
	success ->
	    {stop, ok, State}
     end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%% @doc the kad loop
-spec kad_net_loop( Socket :: socket() ) -> no_return().
kad_net_loop(Socket) ->
    case need_stop() of
	{true, Reason} ->
	    ?LOG("udp process stop reason:~p\n",[Reason]),
	    exit(normal);
	false ->
	    case gen_udp:recv(Socket, ?MAX_MSG_LEN) of
		{ok, {Addr, Port, Packet}} ->
		    ?LOG("recv packet:[~p:~p] ~p\n", [Addr, Port, Packet]),
		    kad_coordinator:dispatch(Addr, Port, Packet),
		    kad_net_loop(Socket);
		{error, Reason} ->
		    ?LOG("udp recv error:~p~n", [Reason]),
		    exit({udp, Reason})
	    end
    end.

%%
%% internal API
%%

%% parse the options
parse_opt(Opts) ->
    Port = proplists:get_value(port, Opts, ?KAD_PORT),
    UdpOpts = proplists:delete(port, Opts),
    %% specify the {active, false} option
    UdpOpts2 = 
    case proplists:get_value(active, UdpOpts) of
        true ->
	    proplists:expand([{active, [{active, false}]}], UdpOpts);
        false ->
	    UdpOpts;    
	undefined ->
	    [{active, false} | UdpOpts]
    end,
    {Port, UdpOpts2}.


need_stop() ->
    receive 
	{stop, Ref, Reason, Parent} ->
	    Parent ! {stop, Ref},
	    {true, Reason};
	_ ->
	    false
    after 0 ->
	    false
    end.
	
