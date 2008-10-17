%%% @author litao cheng <litaocheng@gmail.com>
%%% @copyright 2008 toquick.com.
%%% @doc handle the temp search result list
-module(kad_searchlistl).
-author('litaocheng@gmail.com').
-vsn('0.1').
-include("kad.hrl").

-export([new/1, add/2, iscloser/2, closest/3, to_list/1]).
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

%% @spec new(identify()) -> searchlist()
%% @doc return a new search list, the arg is the search target
new(Tar) ->
    #searchlist{target = Tar}.

%% @spec add(kad_contact(), SearchList) -> SearchList2
%% @doc add a contact to search list
add(#kad_contact{} = Node, State) ->
    D = kad_node:distance(Node, State#searchlist.target),
    List = [#item{dist = D, node = Node} | State#searchlist.list],
    SortL = lists:keysort(#item.dist, List),
    Size = State#searchlist.size + 1,
    State#searchlist{size = Size, list = SortL}.

%% @spec is_closer(identify(), SearchList) -> bool()
%% @doc  return ture if the Id is closer than the closest node in list,
%%       otherwise return false
is_closer(Id, State) when is_binary(Id) ->
    case State#searchlist.list of
	[Closest | _] ->
	    D = kad_node:distance(Id, State#searchlist.target),
	    D < Closest#item.dist;
	[] ->
	    true
    end.	

%% @spec closest(integer(), bool(), SearchList) -> list()
%% @doc return N closet Nodes in searchlist, Used specify if the node
%%		must be unused
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
    kad_util:takewhile(FUnUsed, 0, State#searchlist.list).


to_list(State) ->
    State#searchlist.list;
%% 
%% internal API
%%
