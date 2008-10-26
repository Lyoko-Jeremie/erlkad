%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the util functions
-module(kad_util).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").

-export([randid/0, id/1, idinc/1]).
-export([byte_padding/1]).
-export([log2/1]).
-export([now_ms/0]).
-export([id_to_integer/1, integer_to_id/2, distance/2]).
-export([takewhile/3]).
-export([start_timer/3, cancel_timer/1]).

%% @spec randid() -> binary()
%% @doc gen the random 160-bytes identify
randid() ->
    {A1, A2, A3} = erlang:now(),
    crypto:sha(<<A1:4, A2:4, A3:4>>).

%% @spec id(binary()) -> binary()
%% @doc gen the identify 
id(Data) when is_binary(Data) ->
    crypto:sha(Data).

%% @spec idinc(identify()) -> identify()
%% @doc return the indentify increase one
idinc(Id) when is_binary(Id) ->
    N = id_to_integer(Id),
    N2 = N + 1,
    integer_to_id(N2, ?NODE_ID_LEN div 8).

%% the padding bytes for msg
byte_padding(Len) ->
   binary_append(<<>>, Len,  0).

%% @spec log2(integer()) -> integer()
%% @doc return the 
log2(X) when is_float(X) ->
    log2(trunc(X));
log2(X) when is_integer(X) ->
    log2(X, 0).

log2(0, Acc) -> Acc;
log2(X, Acc) ->
    log2(X bsr 2, Acc + 1).


%% @doc return the now in ms unit
now_ms() ->
    {A, B, C} = now(),
    (A * 1000000 + B + C / 1000000) * 1000.

%% @spec id_to_integer(identify()) -> integer()
%% @doc convert id to integer, the first byte in binary is the low endian
id_to_integer(X) ->
    id_to_integer(X, 1, 0).

id_to_integer(<<C, Rest/binary>>, M, Acc) ->
    Acc1 = C * M,
    Acc2 = Acc1 + Acc,
    id_to_integer(Rest, M * 256, Acc2);
id_to_integer(<<>>, _M, Acc) ->
    Acc.

%% @spec integer_to_id(integer(), integer()) -> identify()
%% @doc convert integer to id
integer_to_id(N, Len) when is_integer(N) ->
    Bin = integer_to_id(N),
    L = byte_size(Bin),
    if L < Len ->
	    binary_append(Bin, Len - L, 0);
       true  ->
	    Bin
    end.

integer_to_id(0) ->
    <<>>;
integer_to_id(N) ->
    <<(N band 16#ff), (integer_to_id(N bsr 8))/binary>>.


binary_append(Bin, 0, _V) ->
    Bin;
binary_append(Bin, N, V) ->
    binary_append(<<Bin/binary, V>>, N-1, V).
    

%% @spec distance(identify(), identify()) -> identify()
%% @doc return the distance based XOR operation
distance(X, Y) when is_binary(X) andalso is_binary(Y) ->
    X1 = id_to_integer(X),
    Y1 = id_to_integer(Y),
    distance(X1, Y1);
distance(X, Y) when is_integer(X) andalso is_integer(Y) ->
     X bxor Y.

%% @spec takewhile(fun(), term(), list()) -> list()
%% @doc take elements from List, where Pred(Element) return {true, Acc},
%%  the function stop when Pred(Element) return break
takewhile(Pred, Acc, List) when is_function(Pred, 2) andalso is_list(List) ->
	takewhile1(Pred, Acc, List, []).

takewhile1(_Pred, _Acc, [], AccL) ->
	lists:reverse(AccL);
takewhile1(Pred, Acc, [H | T], AccL) ->
	case Pred(H, Acc) of
		{true, Acc2} ->			
			takewhile1(Pred, Acc2, T, [H | AccL]);
		false ->
			takewhile1(Pred, Acc, T, AccL);
		break ->
			takewhile1(Pred, Acc, [], AccL)
	end.


%% @doc start the timer, the Timer can be either integer(in ms unit) or infinity
start_timer(infinity, Dest, _Msg) when is_pid(Dest) orelse is_atom(Dest) ->
    {timer, infinity};
start_timer(Time, Dest, Msg) when is_pid(Dest) orelse is_atom(Dest) ->
    erlang:start_timer(Time, Dest, Msg).

%% cancel the timer
cancel_timer({timer, infinity}) ->
    false;
cancel_timer(Timer) when is_reference(Timer) ->
    erlang:cancel_timer(Timer).

