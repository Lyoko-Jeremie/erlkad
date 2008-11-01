%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc handle the temp search result list
-module(kad_searchlist).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").

-export([new/1, add/2, is_closer/2, closest/3, to_list/1]).
-record(item, {
	  used = false, % this node is used?
	  dist,		  % the distance to target
	  node
	 }).

-record(searchlist, {
	  target,	    % the search target
	  size = 0,   % the element count
	  list = []   % store the search list
	 }).			
-type searchlist() :: #searchlist{}.

%% @doc return a new search list, the arg is the search target
-spec new(Tar :: id()) -> searchlist().
new(Tar) ->
    #searchlist{target = Tar}.

%% @doc add a contact to search list
-spec add(contact(), State :: searchlist()) -> searchlist().
add(#kad_contact{} = Node, State) ->
    D = kad_node:distance(Node, State#searchlist.target),
    List = [#item{dist = D, node = Node} | State#searchlist.list],
    SortL = lists:keysort(#item.dist, List),
    Size = State#searchlist.size + 1,
    State#searchlist{size = Size, list = SortL}.

%% @doc  return ture if the Id is closer than the closest node in list,
%%       otherwise return false
-spec is_closer(Id :: id(), State :: searchlist()) -> bool().
is_closer(Id, State) when is_binary(Id) ->
    case State#searchlist.list of
	[Closest | _] ->
	    D = kad_node:distance(Id, State#searchlist.target),
	    D < Closest#item.dist;
	[] ->
	    true
    end.	

%% @doc return N closet Nodes in searchlist, Used specify if the node
%%		must be unused
-spec closest(N :: pos_integer(), bool(), State :: searchlist()) -> list().
closest(N, false, State) ->
	lists:sublist(State#searchlist.list, N);
closest(N, true, State) ->
    F = fun(#item{used = true}, _Acc) -> false;
	   (#item{}, Acc) -> 
		if Acc < N ->
			{true, Acc + 1};
		   true ->
			break
		end
	end,
    kad_util:takewhile(F, 0, State#searchlist.list).

-spec to_list(State :: searchlist()) -> list().
to_list(State) ->
    State#searchlist.list.
%% 
%% internal API
%%


-ifdef(debug).
%% unit test
basic_test_() ->
	Id = <<12, 33, 43, 32, 112, 82, 89, 32, 11, 23, 
				 75, 218, 23, 34, 30, 32, 11, 23, 34, 93>>,
	Id2 = <<42, 33, 43, 32, 112, 82, 89, 32, 21, 23, 
				 75, 218, 23, 34, 30, 32, 11, 23, 34, 33>>,
	Id3 = <<12, 33, 43, 32, 112, 82, 89, 32, 11, 23, 
				 75, 218, 23, 34, 30, 32, 11, 33, 34, 93>>,
	Node = #kad_contact{id = Id2, ip= {211, 234, 2, 23}, port = 2343},
	S = new(Id),
	S2 = add(Node, S),
	[
	?_assert(is_closer(Id3, S2)),
	?_assert(closest(1, false, S2) =:= [Id3])
	].
	
-endif.
