%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the util functions
-module(kad_util).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").

-export([randid/0, id/1, idinc/1]).
-export([byte_padding/1, binary_append/3]).
-export([log2/1]).
-export([now_ms/0]).
-export([id_to_integer/1, integer_to_id/2, distance/2]).
-export([takewhile/3]).
-export([start_timer/3, cancel_timer/1]).

%% @doc gen the random 160-bytes identify
-spec randid() -> id().
randid() ->
    {A1, A2, A3} = erlang:now(),
    crypto:sha(<<A1:4, A2:4, A3:4>>).

%% @doc gen the identify 
-spec id(Data :: binary()) -> id().
id(Data) when is_binary(Data) ->
    crypto:sha(Data).

%% @doc return the indentify increase one
-spec idinc(Id :: id()) -> id().
idinc(Id) when is_binary(Id) ->
    N = id_to_integer(Id),
    N2 = N + 1,
    integer_to_id(N2, ?NODE_ID_LEN div 8).

%% the padding bytes for msg
-spec byte_padding(Len :: non_neg_integer()) -> binary().
byte_padding(Len) ->
   binary_append(<<>>, Len,  0).
   
-spec binary_append(Bin :: binary(), N :: non_neg_integer(), V :: byte() ) -> binary().
binary_append(Bin, 0, _V) ->
    Bin;
binary_append(Bin, N, V) ->
    binary_append(<<Bin/binary, V>>, N-1, V).

%% @doc return the 
-spec log2( X :: non_neg_integer() ) -> non_neg_integer().
log2(X) when is_float(X) ->
    log2(trunc(X));
log2(X) when is_integer(X) ->
    log2(X, 0).

log2(0, Acc) -> Acc;
log2(X, Acc) ->
    log2(X bsr 1, Acc + 1).


%% @doc return the now in ms unit
-spec now_ms() -> pos_integer().
now_ms() ->
    {A, B, C} = now(),
    (A * 1000000 + B + C div 1000000) * 1000.

%% @doc convert id to integer, the first byte in binary is the low endian
-spec id_to_integer(X :: id()) -> non_neg_integer().
id_to_integer(X) ->
    id_to_integer(X, 1, 0).

id_to_integer(<<C, Rest/binary>>, M, Acc) ->
    Acc1 = C * M,
    Acc2 = Acc1 + Acc,
    id_to_integer(Rest, M * 256, Acc2);
id_to_integer(<<>>, _M, Acc) ->
    Acc.

%% @doc convert integer to id
-spec integer_to_id( N :: non_neg_integer(), Len :: pos_integer() ) -> binary().
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

%% @doc return the distance based XOR operation
-spec distance(X :: id(), Y :: id()) -> non_neg_integer().
distance(X, Y) when is_binary(X) andalso is_binary(Y) ->
    X1 = id_to_integer(X),
    Y1 = id_to_integer(Y),
    distance(X1, Y1);
distance(X, Y) when is_integer(X) andalso is_integer(Y) ->
     X bxor Y.

%% @doc take elements from List, where Pred(Element) return {true, Acc},
%%  the function stop when Pred(Element) return break
-spec takewhile(Pred :: fun((E :: any(), Acc:: any()) -> {'true', any()} | 'break' | 'false'),
											Acc :: any(),
											List :: list()) -> list(). 
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
-spec start_timer(timeout(), Dest :: pid(), Msg :: any()) -> {'timer', 'infinity'}) | ref().				
start_timer(infinity, Dest, _Msg) when is_pid(Dest) orelse is_atom(Dest) ->
    {timer, infinity};
start_timer(Time, Dest, Msg) when is_pid(Dest) orelse is_atom(Dest) ->
    erlang:start_timer(Time, Dest, Msg).

%% cancel the timer
cancel_timer({timer, infinity}) ->
    false;
cancel_timer(Timer) when is_reference(Timer) ->
    erlang:cancel_timer(Timer).


-ifdef(debug).
%% unit test
id_test_() ->
    Id = randid(),
    [    
     ?_assert(20 =:= byte_size(Id)),
     ?_assert(integer_to_id(id_to_integer(Id), 20) =:= Id),
     ?_assert(idinc(Id) == integer_to_id(id_to_integer(Id) + 1, 20)),
     ?_assert(distance(Id, Id) =:= 0)	 
    ].

log2_test_() ->
    [
     ?_assert(log2(2#10101110101010110) =:= 17)
    ].

takewhile_test_() ->
    L = [1, 20, 33, a, 23, 34, 332, a, b, 233],
    FTo100 = fun(E, _Acc) when is_atom(E) ->
		     false;
		(E, Acc) when is_integer(E) ->
		     Acc2 = Acc + E,
		     if Acc2 >= 100 ->
			     break;
			true ->
			     {true, Acc2}
		     end
	     end,

    [
     ?_assert(takewhile(FTo100, 0, L) =:= [1, 20, 33, 23, 34])
    ].
    
-endif.
