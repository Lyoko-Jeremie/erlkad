%%% @author litao cheng litaocheng@gmail.com
%%% @copyright 2008 toquick.com.
%%% @doc the kademlia protocol parse module
-module(kad_protocol).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").
-include("kad_protocol.hrl").

-export([parse/1]).
-export([gen_msg/4]).
-export([optype/1]).

%% @type cmd() = integer()
%% @type identify() = binary()
%% @type data() = term()

%% @spec parse(binary()) -> {cmd(), identify(), identify(), identify(), data()}
%% @doc parse the kad msg
parse(<<Cmd, D:160/binary, S:160/binary, ID:160/binary, Len:16, Payload:Len/binary, _Rest/binary>>) ->
    case parse_data(Cmd, Payload) of
	ignore ->
	    ignore;
	Data ->
	    {Cmd, D, S, ID, Data}
    end;
parse(_) ->
    ?LOG("invalid msg~n"),
    ignore.

%% @spec gen_msg(cmd(), identfiy(), identify(), term()) -> binary()
%% @doc gen the rsp msg
gen_msg(Cmd, D, Id, Args) ->
    Header = <<Cmd, D/binary, (kad_node:id())/binary, Id/binary>>,
    gen_msg(Cmd, Header, Args).

%% @spec gen_msg(cmd(), identify(), list()) -> binary()
%% @doc gen the msg
gen_msg(?PING, Hd, _Dummy) ->
    <<?PING, Hd/binary, 0:16>>;
gen_msg(?PING_FIRST, Hd, _Dummy) ->
    <<?PING_FIRST, Hd/binary, 0:16>>;
gen_msg(?STORE, Hd, {Key, Data}) ->
    Len = byte_size(Data) + ?NODE_ID_BYTES,
    <<Hd/binary, Len:16, Key/binary, Data/binary>>;
gen_msg(?FIND_NODE, Hd, Node) ->
    <<Hd/binary, ?NODE_ID_BYTES:16, Node/binary>>;
gen_msg(?FIND_VALUE, Hd, Key) ->
    <<?FIND_VALUE, Hd/binary, ?NODE_ID_BYTES:16, Key/binary>>;
gen_msg(?DELETE, Hd, Key) ->
    <<Hd/binary, ?NODE_ID_BYTES:16, Key/binary>>;


gen_msg(?PING_RSP, Hd, _Dummy) ->
    <<Hd/binary, 0:16>>;
gen_msg(?PING_FIRST_RSP, Hd, Self) ->
    <<Hd/binary, ?NODE_ID_BYTES:16, Self/binary>>;
gen_msg(?PING_FIRST_ACK, Hd, _Dummy) ->
    <<Hd/binary, 0:16>>;
gen_msg(?STORE_RSP, Hd, Value) ->
    <<?STORE_RSP, Hd/binary, Value>>;
gen_msg(?FIND_NODE_RSP, Hd, Nodes) when is_list(Nodes) ->
    Data = gen_nodes(Nodes),
    Len = byte_size(Data),
    <<Hd/binary, Len:16, Data/binary>>;
gen_msg(?FIND_VALUE_RSP, Hd, Data) ->
    Len = byte_size(Data),
    <<Hd/binary, Len:16, Data/binary>>;
gen_msg(?DELETE_RSP, Hd, _Dummy) ->
    <<Hd/binary, 0:16>>.


%% @spec optype(cmd()) -> ?OP_REQ | ?OP_RSP
%% @doc return ?OP_REQ if the cmd is a request, 
%%      return ?OP_RSP if the cmd is a response.
optype(?PING_FIRST) ->
    ?OP_REQ;
optype(?PING) ->
    ?OP_REQ;
optype(?STORE) ->
    ?OP_REQ;
optype(?FIND_NODE) ->
    ?OP_REQ;
optype(?FIND_VALUE) ->
    ?OP_REQ;
optype(?DELETE) ->
    ?OP_REQ;
optype(?PING_FIRST_RSP) ->
    ?OP_RSP;    
optype(?PING_RSP) ->
    ?OP_RSP;
optype(?STORE_RSP) ->
    ?OP_RSP;
optype(?FIND_NODE_RSP) ->
    ?OP_RSP;
optype(?FIND_VALUE_RSP) ->
    ?OP_RSP;
optype(?DELETE_RSP) ->
    ?OP_RSP.
optype(?PING_FIRST_ACK) ->
    ?OP_RSP.


%%
%% internal API
%% 

%% parse the msg data
parse_data(?PING, <<>>) ->
    none;
parse_data(?PING, <<Key:160/bytes>>) ->
    Key;
parse_data(?STORE, <<Key:160/bytes, Data/bytes>>) ->
    {Key, Data};
parse_data(?FIND_NODE, <<Key:160/bytes>>) ->
    Key;
parse_data(?FIND_VALUE, <<Key:160/bytes>>) ->
    Key;
parse_data(?DELETE, <<Key:160/bytes>>) ->
    Key;

parse_data(?PING_RSP, <<>>) ->
    none;
parse_data(?STORE_RSP, <<>>) ->
    none;
parse_data(?FIND_NODE_RSP, Nodes) ->
    parse_nodes(Nodes);
parse_data(?FIND_VALUE_RSP, Data) ->
    Data;
parse_data(?DELETE_RSP, <<>>) ->
    none;
parse_data(_Cmd, _Payload) ->
    ignore.

parse_nodes(Nodes) ->
    parse_nodes(Nodes, []).

parse_nodes(<<D1, D2, D3, D4, Port:2, Node:160/binary, Rest/binary>>, Acc) ->
    %case inet_parse:address(binary_to_list(Ip)) of
    Entry = #kad_contact{id = Node, ip = {D1, D2, D3, D4}, port = Port},
    parse_nodes(Rest, [Entry | Acc]);
parse_nodes(<<>>, Acc) ->
    Acc.


gen_nodes(Nodes) when is_list(Nodes) ->
    gen_nodes(Nodes, []).

gen_nodes([{{D1, D2, D3, D4}, Port, Id} | Rest], Acc) ->
    Entry = <<D1, D2, D3, D4, Port:2, Id/binary>>,
    gen_nodes(Rest, [Entry | Acc]);
gen_nodes([], Acc) ->
    list_to_binary(Acc).
