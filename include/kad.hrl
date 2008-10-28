%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the kademlia header file

-ifdef(debug).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% version
-define(Version, "0.1").
%% kad network version
-define(KADVer, "1.1").

%% the default udp kad port
-define(KAD_PORT, 5102).

%% the max msg length
-define(MAX_MSG_LEN, 1400).

%% the node id length
-define(NODE_ID_LEN, 160).

%% the node id bytes
-define(NODE_ID_BYTES, 20).

%% the k constant
-define(K, 20).

%% the k constant specify the items  in k-bucket
-define(KK, 30).

%% the alpha constant
-define(A, 3).

%% the random node count, when refresh the bucket
-define(REFRESH_NODE_COUNT, 1).

%% the node probe  timeout
-define(PTIMEOUT, 15000).

%% the log macro
-define(LOG, io:format).

%% not implement
-define(NOT_IMPL, throw(not_imple)).

%%%%%%%%%%%%%%%%
%time info
%%%%%%%%%%%%%%%%
-define(BUCKET_REFRESH, timer:minutes(10)).

%%%%%%%%%%%%%%%%

%% kad node id
-record(kad_contact, {
        id,     % the kad node indentify
	ip,			% the node ip (ip_address)
	port,       % the port (integer)
	rtt = 0,	% the rtt
	nat = false	% nat info
	}).



%% loop infinity wait one msg
%% example: ?WAIT_MSG(hello, [foo, bar], 1000),
%% wait the hello msg and skip the foo, bar msg with timeout 1000 ms
-define(WAIT_MSG(Msg, Skip, Timeout), 
            fun() ->
	        F = fun(F) ->
		        receive
			    Msg ->
			        success;
			    Packet ->
			        case lists:member(Packet, Skip) of
				    true ->
				        F(F);
				    false ->
				        {error, msg_unknown}
                                end
			after Timeout ->
			    {error, timeout}
			end
		    end,
                F(F)
                end()
		).
