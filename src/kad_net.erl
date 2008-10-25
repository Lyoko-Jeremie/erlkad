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
            socket,     % the udp socket
	    pid         % the udp process pid
	    }).

-define(SERVER, ?MODULE).

%% @doc start the kad udp socket
start_link() ->
    start_link([]).

%% @doc start the kad udp socket
%%   Opts = [Option]
%%   Option = {port, Port} 
%% @see inet:setopts/2
start_link(Opts) when is_list(Opts) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Opts, []).

%% @spec stop(term()) -> ok
%% @doc stop the kad udp server
stop(Reason) ->
    gen_server:cast(?SERVER, {stop, Reason}).

%% @spec socket() -> socket()
%% @doc return the socket
socket() ->
    gen_server:call(?SERVER, socket).

%% @spec send(Addr, Port, Msg) -> ok | {error, Reason}
%% @doc send the msg
send(Addr, Port, Msg) ->
    Socket = socket(),
    gen_udp:send(Socket, Addr, Port, Msg).

%%
%% gen_server callback
%% 
init(Opts) ->
    process_flag(trap_exit, true),
    {Port, UdpOpts} = parse_opt(Opts),
    ?LOG("kad open udp port:~p~n", [Port]),
    case gen_udp:open(Port, UdpOpts) of
        {ok, Socket} ->
            Pid = proc_lib:spawn_link(?MODULE, kad_net_loop, Socket),
            State = #state{socket = Socket, pid = Pid},
	    {ok, State};
	{error, Reason} ->
	    {stop, Reason}
    end.

handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({stop, Reason}, State) ->
    ?LOG("pre stop the kad udp socket:~p~n", [Reason]),
    State#state.pid ! {stop, Reason, self()},
    case ?WAIT_MSG(stop, [], 1000) of
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

%% @spec kad_net_loop(list()) -> void
%% @doc the kad loop
kad_net_loop(Socket) ->
    case gen_udp:recv(Socket, ?MAX_MSG_LEN) of
        {ok, {Addr, Port, Packet}} ->	    
	    kad_coordinator:dispatch(Addr, Port, Packet),
	    kad_net_loop(Socket);
	{error, Reason} ->
	    ?LOG("udp recv error:~p~n", [Reason]),
	    exit({udp, Reason})
    end.

%%
%% internal API
%%

%% parse the options
parse_opt(Opts) ->
    Port = proplist:get_value(port, Opts, ?KAD_PORT),
    UdpOpts = proplist:delete(port, Opts),
    %% specify the {active, false} option
    UdpOpts2 = 
    case proplists:get_value(active, UdpOpts) of
        true ->
	    proplists:expand([{active, {active, false}}], UdpOpts);
        false ->
	    UdpOpts;    
	undifined ->
	    [{active, false} | UdpOpts]
    end,
    {Port, UdpOpts2}.
