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

%% @doc parse the kad msg
-spec parse(binary()) -> 'ignore' | {cmd(), id(), id(), id(), any()}.
parse(<<Cmd, D:160/bits, S:160/bits, ID:160/bits, Len:16, Payload:Len/bytes, _Rest/bytes>>) ->
    case parse_data(Cmd, Payload) of
	ignore ->
	    ?LOG("parse data error~n"),
	    ignore;
	Data ->
	    {Cmd, D, S, ID, Data}
    end;
parse(_) ->
    ?LOG("invalid msg format~n"),
    ignore.

%% @doc gen the msg
-spec gen_msg(Cmd :: cmd(), D :: id(), Id :: id(), Args :: any()) -> binary().
gen_msg(Cmd, D, Id, Args) ->
    Header = <<Cmd, D/binary, (kad_node:id())/binary, Id/binary>>,
    Msg = gen_msg(Cmd, Header, Args),
    %?LOG("gen msg:~p~n", [Msg]),
    Msg.

gen_msg(?PING, Hd, _Dummy) ->
    <<Hd/binary, 0:16>>;
gen_msg(?PING_FIRST, Hd, _Dummy) ->
    <<Hd/binary, 0:16>>;
gen_msg(?STORE, Hd, {Key, Data}) ->
    Len = byte_size(Data) + ?NODE_ID_BYTES,
    <<Hd/binary, Len:16, Key/binary, Data/binary>>;
gen_msg(?FIND_NODE, Hd, Node) ->
    <<Hd/binary, ?NODE_ID_BYTES:16, Node/binary>>;
gen_msg(?FIND_VALUE, Hd, Key) ->
    <<Hd/binary, ?NODE_ID_BYTES:16, Key/binary>>;
gen_msg(?DELETE, Hd, Key) ->
    <<Hd/binary, ?NODE_ID_BYTES:16, Key/binary>>;


gen_msg(?PING_RSP, Hd, _Dummy) ->
    <<Hd/binary, 0:16>>;
gen_msg(?PING_FIRST_RSP, Hd, Self) ->
    <<Hd/binary, ?NODE_ID_BYTES:16, Self/binary>>;
gen_msg(?PING_FIRST_ACK, Hd, _Dummy) ->
    <<Hd/binary, 0:16>>;
gen_msg(?STORE_RSP, Hd, Value) ->
    <<Hd/binary, 1:16, Value>>;
gen_msg(?FIND_NODE_RSP, Hd, Nodes) when is_list(Nodes) ->
    Data = gen_nodes(Nodes),
    Len = byte_size(Data),
    <<Hd/binary, Len:16, Data/binary>>;
gen_msg(?FIND_VALUE_RSP, Hd, Data) ->
    Len = byte_size(Data),
    <<Hd/binary, Len:16, Data/binary>>;
gen_msg(?DELETE_RSP, Hd, _Dummy) ->
    <<Hd/binary, 0:16>>.


%% @doc return ?OP_REQ if the cmd is a request, 
%%      return ?OP_RSP if the cmd is a response.
-spec optype(cmd()) -> optype().
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
    ?OP_RSP;
optype(?PING_FIRST_ACK) ->
    ?OP_RSP.


%%
%% internal API
%% 

%% parse the msg data
parse_data(?PING, <<>>) ->
    none;
parse_data(?PING_FIRST, <<>>) ->
    none;
parse_data(?STORE, <<Key:160/bits, Data/bytes>>) ->
    {Key, Data};
parse_data(?FIND_NODE, <<Key:160/bits>>) ->
    Key;
parse_data(?DELETE, <<Key:160/bits>>) ->
    Key;

parse_data(?PING_RSP, <<>>) ->
    none;
parse_data(?PING_FIRST_RSP, <<Id:160/bits>>) ->
    Id;
parse_data(?PING_FIRST_ACK, <<>>) ->
    none;
parse_data(?STORE_RSP, <<Value>>) ->
    Value;
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

parse_nodes(<<D1, D2, D3, D4, Port:16, Node:160/bits, Rest/bytes>>, Acc) ->
    %case inet_parse:address(binary_to_list(Ip)) of
    Entry = #kad_contact{id = Node, ip = {D1, D2, D3, D4}, port = Port},
    parse_nodes(Rest, [Entry | Acc]);
parse_nodes(<<>>, Acc) ->
    Acc.


gen_nodes(Nodes) when is_list(Nodes) ->
    gen_nodes(Nodes, []).

gen_nodes([#kad_contact{ip = {D1, D2, D3, D4}, port = Port, id = Id} | Rest], Acc) ->
    Entry = <<D1, D2, D3, D4, Port:16, Id/bytes>>,
    gen_nodes(Rest, [Entry | Acc]);
gen_nodes([], Acc) ->
    list_to_binary(Acc).


-ifdef(debug).
%% unit test
gen_msg2(Cmd, D, S, Id, Data) ->
    Header = <<Cmd, D/binary, S/binary, Id/binary>>,
    gen_msg(Cmd, Header, Data).    
    
proto_test_() ->
    D = <<0:160>>,
    S = <<16#fffffffff:160>>,
    Id = <<16#ddddddddddd:160>>,	
    Key = <<16#134132132304da83313de333234324:160>>,
    Data = <<12, 34, 23, 34, 32, 1, 34, 32, 81, 112>>,
    NodeList1 = [#kad_contact{ ip = {127,0,0,1}, port = 2100, id = Id}, #kad_contact{ip = {192, 168, 1, 1}, port = 2112, id = Id}],
    NodeList2 = [],
    %Msg_SR = gen_msg2(?FIND_VALUE_RSP, D, S, Id, Data),
    %io:format("msg :~p~n parse:~p~n", [Msg_SR, parse(Msg_SR)]),
    [
     ?_assert(parse(gen_msg2(?PING, D, S, Id, dummy)) =:= {?PING, D, S, Id, none}),
     ?_assert(parse(gen_msg2(?PING_FIRST, D, S, Id, dummy)) =:= {?PING_FIRST, D, S, Id, none}),
     ?_assert(parse(gen_msg2(?STORE, D, S, Id, {Key, Data})) =:= {?STORE, D, S, Id, {Key, Data}}),
     ?_assert(parse(gen_msg2(?FIND_NODE, D, S, Id, Key)) =:= {?FIND_NODE, D, S, Id, Key}),
     ?_assert(parse(gen_msg2(?FIND_VALUE, D, S, Id, Key)) =:= {?FIND_VALUE, D, S, Id, Key}),
     ?_assert(parse(gen_msg2(?DELETE, D, S, Id, Key)) =:= {?DELETE, D, S, Id, Key}),

     ?_assert(parse(gen_msg2(?PING_RSP, D, S, Id, dummy)) =:= {?PING_RSP, D, S, Id, none}),
     ?_assert(parse(gen_msg2(?PING_FIRST_RSP, D, S, Id, Key)) =:= {?PING_FIRST_RSP, D, S, Id, Key}),
     ?_assert(parse(gen_msg2(?STORE_RSP, D, S, Id, 1)) =:= {?STORE_RSP, D, S, Id, 1}),
     ?_assert(parse(gen_msg2(?FIND_NODE_RSP, D, S, Id, NodeList1)) =:= {?FIND_NODE_RSP, D, S, Id, NodeList1}),
     ?_assert(parse(gen_msg2(?FIND_NODE_RSP, D, S, Id, NodeList2)) =:= {?FIND_NODE_RSP, D, S, Id, NodeList2}),
     ?_assert(parse(gen_msg2(?FIND_VALUE_RSP, D, S, Id, Data)) =:= {?FIND_VALUE_RSP, D, S, Id, Data}),
     ?_assert(parse(gen_msg2(?PING_FIRST_ACK, D, S, Id, dummy)) =:= {?PING_FIRST_ACK, D, S, Id, none})
    ].

-endif.
