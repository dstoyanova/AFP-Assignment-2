-module(dice).

-export([dice/3]).

dice(N, Edges, Rolls) -> 
	dice_helper_1(N, Edges, Rolls, [1], 0, 0). 

dice_helper_1(_, _, _, _, M, _) when M =:= 100 -> -1;
dice_helper_1(N, Edges, Rolls, NDS, M, CR) -> 
	Nodes = dice_helper_2(Edges, NDS, nth(CR, Rolls)),
	case intersect_mine([N], Nodes) =:= [] of
		true -> dice_helper_1(N, Edges, Rolls, Nodes, M + 1, nextroll(Rolls, CR));
		_ -> M + 1
	end.

dice_helper_2(_, _, STP) when STP =:= 0 ->  [];
dice_helper_2(Edges, Nodes, STP) when STP =:= 1 ->  neighbours(Edges, Nodes);
dice_helper_2(Edges, Nodes, STP) -> dice_helper_2(Edges, neighbours(Edges, Nodes), STP - 1). 

neighbours(Edges, Nodes) -> 
	lists:foldl(fun(X, Res) -> union_mine(union_mine(neighbours_helper(Edges, X, [], 0), Res), Res) end, [], Nodes).

neighbours_helper(Edges, _, Results, CP) when length(Edges) =:= CP -> Results;
neighbours_helper(Edges, Node, Results, CP) -> 
	PNode = first(nth(CP, Edges)),
	case PNode =:= Node of
		true -> neighbours_helper(Edges, Node, [second(nth(CP, Edges))] ++ Results, CP + 1);
		_ -> neighbours_helper(Edges, Node, Results, CP + 1)
	end.

nextroll(Rolls, P) when length(Rolls) =:= P + 1 -> 0;
nextroll(_, P) -> P + 1.

first({A,_}) -> A.

second({_, B}) -> B.

nth(0,[H | _]) -> H;
nth(Index, [_ | T]) -> nth(Index - 1, T).

union_mine(List1, []) -> List1;
union_mine([], List2) -> List2;
union_mine(List1, List2) -> 
	sets:to_list(sets:union(sets:from_list(List1), sets:from_list(List2))).

intersect_mine(_, []) -> [];
intersect_mine([], _) -> [];
intersect_mine(List1, List2) -> 
	sets:to_list(sets:intersection(sets:from_list(List1), sets:from_list(List2))). 