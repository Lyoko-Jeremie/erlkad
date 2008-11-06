%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc the kademlia k-buckets for rounting
-module(kad_routing).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").
-behaviour(gen_server).

-export([start_link/0, stop/1]).
-export([size/0, global_size/0]).
-export([update/1, closest/2, all_nodes/0]).
-export([random_nodes/2]).
-export([random_refresh_bucket/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
                            terminate/2, code_change/3]).

-record(state, {
	  size = 0,   % the contacts count
	  buckets,    % the buckets 
	  actives = [],% the bucket list which has contacts
	  laccess     % last access time
	 }).

-define(SERVER, ?MODULE).

%% @doc start the bucket
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc stop the kad_routing
stop(Reason) ->
    gen_server:cast(?SERVER, {stop, Reason}).

%% @doc return the contacts in buckets
-spec size() -> non_neg_integer().
size() ->
    gen_server:call(?SERVER, size).

%% @doc return the estimate global nodes count
-spec global_size() -> non_neg_integer().
global_size() ->
    gen_server:call(?SERVER, global_size).

%% @doc update the k-bucket
-spec update(Node :: contact()) -> 'ok'.
update(Node) ->
    gen_server:cast(?SERVER, {update, Node}).

%% @doc return the N closest nodes to Node
-spec closest(Node :: id(), N :: pos_integer()) -> {non_neg_integer(), [contact()]}.
closest(Node, N) ->
    gen_server:call(?SERVER, {closest, Node, N}).

all_nodes() ->
    gen_server:call(?SERVER, all_nodes).

%% @doc random select N nodes from the I bucket
-spec random_nodes(I :: bucket_index(), N :: pos_integer()) -> [contact()].
random_nodes(I, N) ->
    gen_server:call(?SERVER, {random, I, N}).

%% @doc random select REFRESH_NODE_COUNT nodes in bucket to refresh
-spec random_refresh_bucket() -> 'ok'.
random_refresh_bucket() ->
    [
     case catch random_nodes(I, ?REFRESH_NODE_COUNT) of
	 {'EXIT', {noproc, _}} ->
	     % the kad_routing gen_server has stop
	     exit(stop);
	 [] -> % don't has nodes in this bucket
	     ok;
	 [Node|_] -> % do find for this node
	     kad_api:find_node(Node, infinity, false, true)	    
     end
     || I <- lists:duplicate(?NODE_ID_LEN, dummy)],
    ok.
			
%%
%% gen_server callback
%% 
init(_Args) ->
    start_refresh_timer(),
    Buckets = array:new([{size, ?NODE_ID_LEN}, {fixed, true}, {default, []}]),
    State = #state{buckets = Buckets},
    ?LOG("routing server init state:~p\n", [State]),
    {ok, State}.	

handle_call(size, _From, State) ->
    {reply, State#state.size, State};
handle_call(global_size, _From, State) ->
    % the lowest level
    Low = get_lowest_level(State#state.actives),
    Count = 2 bsl (?NODE_ID_LEN - Low) * State#state.size,
    {reply, Count, State};
handle_call({closest, Node, N}, _From, State) ->
    Closest = do_closest(Node, N, State),
    {reply, Closest, State};
handle_call(all_nodes, _From, State) ->
    Reply = do_all_nodes(State),
    {reply, Reply, State};
handle_call({random, I, N}, _From, State = #state{buckets = Buckets, actives = Actives}) ->
    Reply = 
	case lists:member(I, Actives) of
	    false -> % this bucket is empty
		[];
	    true ->
		Bucket = array:get(I, Buckets),
		if length(Bucket) =< N ->
			Bucket;
		   true ->
			do_random_from_bucket(N, Bucket)
		end
	end,
    {reply, Reply, State};
handle_call(_Msg, _From, State) ->
    {noreply, State}.

handle_cast({stop, Reason}, State) ->
    {stop, Reason, State};
handle_cast({update, Node}, State) ->
    State2 = do_update(Node, State),
    {noreply, State2};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%%
%% internal API
%%

%% update the buckets
do_update(Node = #kad_contact{id = Id}, State = #state{buckets = Buckets, actives = Actives}) ->   
    % get the bucket	
    {Index, B} = get_bucket(Id, Buckets),	
    B2 = update_bucket(Node, B),
    ?LOG("do_update :~p ~p ~p~n", [Index, B2, Buckets]),
    Bs2 = array:set(Index, B2, Buckets),
    As2 = 
	case B of
	    [] ->
		[Index | Actives];
	    [_|_] ->
		Actives
	end,
    State#state{buckets = Bs2, actives = As2}.

%% get the bucket
get_bucket(Key, Buckets) ->
    Dist = kad_node:distance(Key),
    if Dist =:= 0 ->
	    erlang:error(badarg); % "the Key is self"
       true ->
	    ok
    end,
    ?LOG("dist in get_bucket:~p ~p ~p\n", [Key, kad_node:id(),Dist]),
    Index = kad_util:log2(Dist) - 1,
    % get the bucket
    Bucket =
    case catch array:get(Index, Buckets) of
	{'EXIT', {badarg, _}} ->
	    [];
	B ->
	    B
    end,
    {Index, Bucket}.
	
%% get the closest nodes
do_closest(Node, N, #state{buckets = Buckets, actives = Actives} = States) ->
    ?LOG("do_closest ~p ~p ~p ~n", [Node, N, States]),
    case catch get_bucket(Node, Buckets) of
	{'EXIT', {badarg, _}} ->
	   do_closest1(Actives, Buckets, N, [], 0);
	{I, B} ->
	    Acc1 = lists:sublist(B, N),
	    AccN1 = length(Acc1),
	    if AccN1 =:= N ->
		    {AccN1, Acc1};
	       true -> % not enough nodes
		    Actives2 = Actives -- [I],
		    do_closest1(Actives2, Buckets, N - AccN1, Acc1, AccN1)
	    end
	    
    end.
    
do_closest1(_Actives, _Buckets, 0, Acc, AccN) ->
    {AccN, lists:reverse(Acc)};
do_closest1([], _Buckets, _N, Acc, AccN) ->
    {AccN, lists:reverse(Acc)};
do_closest1([H|T], Buckets, N, Acc, AccN) ->
    Bucket = array:get(H, Buckets),
    Acc1 = lists:sublist(Bucket, N),
    AccN1 = length(Acc1),
    if AccN1 =:= N ->
	    {AccN + AccN1, Acc ++ Acc1};
       true ->
	    do_closest1(T, Buckets, N - AccN1, Acc ++ Acc1, AccN + AccN1)
    end.

%% return all the nodes
do_all_nodes(#state{buckets = Buckets, actives = Actives}) ->
    lists:foldl(fun(Index, Acc) ->
			Bucket = array:get(Index, Buckets),
			lists:append([Acc, Bucket])
		end,
		[], Actives).

%% random get N nodes from the specify bucket
do_random_from_bucket(N, Bucket) ->
    do_random_from_bucket1(N, Bucket, []).

do_random_from_bucket1(0, _Bucket, Acc) ->
    Acc;
do_random_from_bucket1(N, Bucket, Acc) ->
    L = length(Bucket),
    Index = random:uniform(L - 1) + 1,
    Node = lists:nth(Index, Bucket),
    Bucket2 = lists:sublist(Bucket, Index - 1) ++ lists:sublist(Bucket, Index + 1, L - Index),
    do_random_from_bucket1(N - 1, Bucket2, [Node | Acc]).
	
%% update the bucket
update_bucket(Node = #kad_contact{id = Id}, Bucket) ->
    case lists:keysearch(Id, #kad_contact.id, Bucket) of
	{value, _ExistNode} ->
	    ?LOG("the node exist in bucket\n"),
	    % move it to the end of list
	    Bucket2 = lists:keydelete(Id, #kad_contact.id, Bucket),
	    lists:append(Bucket2, [Node]);
	false -> % not exist in bucket
	    Len = length(Bucket),
	    if Len < ?KK -> % the bucket is not full
		 ?LOG("new node [~p:~p] ~p to k-bucket\n", [Node#kad_contact.ip, Node#kad_contact.port, Node#kad_contact.id]),
		 Bucket ++ [Node];
	       true -> % the bucket is full
		 [First = #kad_contact{id = Id, ip = Ip, port = Port} | Rest] = Bucket,
  		 % check the first Node
		 case kad_api:ping(Id, Ip, Port, true) of
		     {error, Reason} -> % ping the first Node is error, discard it
			 ?LOG("ping node:~p failed, reason:~p\n", [First, Reason]),
			 Rest ++ [Node];
		     {value, _} -> % the Node is valid
			 tl(Bucket) ++ [hd(Bucket)]
		 end
	    end
    end.


	    
%% get the lowest level in buckets
%% the first element is the lowest level which has nodes in route table
%% the level is between 0 - ?NODE_ID_LEN - 1
get_lowest_level([]) -> 
    160;
get_lowest_level([Low|_]) ->
    Low.  

%% start the refresh timer
start_refresh_timer() ->
    case timer:apply_interval(?BUCKET_REFRESH, ?MODULE, random_refresh_bucket, []) of
	{ok, _TRef} ->
	    ok;
	{error, Reason} ->
	    ?LOG("start the timer:apply_interval error\n"),
	    exit(Reason)
    end.
