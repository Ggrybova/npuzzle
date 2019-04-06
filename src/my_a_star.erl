%%%-------------------------------------------------------------------
%%% @author ggrybova
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 23. Mar 2019 3:28 PM
%%%-------------------------------------------------------------------
-module(my_a_star).
-author("ggrybova").

%% API
-export([a_star/4,
	h_weight/2,
	min_weight/2,
	record_to_from/8]).

-type pos()::{non_neg_integer(), non_neg_integer()}.

-spec a_star(Start::pos(), Goal::pos(), Heuristic::non_neg_integer(), FinalState::list()) -> list(pos()) | {error, Reason::te}.
a_star(Start, Goal, Heuristic, FinalState) ->
	F = case Heuristic of
			1 -> fun manhattan_distance/2;
			2 -> fun h_weight/2;
			3 -> fun chebyshev_distance/2;
			_ -> exit(self(),"Wrong Heuristic~n")
		end,
	Closed = ets:new(close, [bag]),					%% нода, которые уже встречались в ходе решения
	Opened = ets:new(open, [bag]),					%% ноды не проверенные алгоритмом(еще не смотрели их соседей)
	ets:insert(Opened, Start),						%% начинаем проверять со стартовой ноды
	From  = dict:store(Start, none, dict:new()),	%% связка из_ноды - в_ноду
	GWeight = dict:store(Start, 0, dict:new()), 	%% вес от старта до текущей ноды
	FWeight = dict:store(Start, F(Start, Goal), dict:new()),	%% вес от текущей ноды до цели
%%	io:format("Closed: ~p~nOpened: ~p~nFWeight: ~p~nFrom: ~p~n", [ets:tab2list(Closed), ets:tab2list(Opened), dict:to_list(FWeight), dict:to_list(From)]),
	case find_path(Goal, Closed, Opened, GWeight, FWeight, From, is_empty(Opened), F) of
		{ok, Path} ->
			compose_path(Goal, Path);
		_ -> {error, "Unsolvabled puzzle"}
	end.

find_path(Goal, Closed, Opened, GWeight, FWeight, From, Last, Fun) when is_integer(Last) ->
	{CurrNode, _} = min_weight(Opened, FWeight), %% нода с минимальным весом
%%	io:format("CurrNode: ~p, Opened: ~p~n", [CurrNode, ets:tab2list(Opened)]),
	case CurrNode of
		Goal ->
			io:format("From: ~p~n", [dict:to_list(From)]),
			get_path(Goal, From);
		{_Node, _Weight} ->
			ets:delete_object(Opened, CurrNode),
			ets:insert(Closed, CurrNode),
			Neighbours = get_neighbours(CurrNode),
			NotClosedNeighbours = [N || N <- Neighbours, ets:match(Closed, N) == []],
%%			io:format("Closed: ~p~nOpened: ~p~n", [ets:tab2list(Closed), ets:tab2list(Opened)]),
%%			io:format("Neighbours: ~p~nNotClosedNeighbours: ~p~n", [Neighbours, NotClosedNeighbours]),
			{_, GWeight0, FWeight0, From0} =
				choose_optimal_neighbour(CurrNode, Opened, GWeight, FWeight, From, Goal,NotClosedNeighbours, Fun),
			find_path(Goal, Closed, Opened, GWeight0, FWeight0, From0, is_empty(Opened), Fun)
	end;

find_path(_, _, _, _, _, _, notfound, _) ->
	notfound.

choose_optimal_neighbour(CurrNode, Opened, GWeight, FWeight, From, Goal, NotClosedNeighbours,Fun) ->
	{ok, TemptG} = dict:find(CurrNode, GWeight),
	lists:foldl(
		fun(Neighbour, {TemptG0, GWeight0, FWeight0, From0}) ->
%%			io:format("G: ~p~n", [TemptG0]),
			case ets:match(Opened, Neighbour) of
				[] ->
%%					io:format("do1~n", []),
					ets:insert(Opened, Neighbour),
					record_to_from(Neighbour, CurrNode, TemptG0, GWeight0, FWeight0, From0, Goal, Fun);
				_ ->
					case dict:find(Neighbour, GWeight0) of
						{ok, GNeighbour} when is_integer(GNeighbour), TemptG0 < GNeighbour ->
%%							io:format("GNeigh: ~p~n", [GNeighbour]),
%%							io:format("do2~n", []),
							record_to_from(Neighbour, CurrNode, TemptG0, GWeight0, FWeight0, From0, Goal, Fun);
						_ ->
							{TemptG0, GWeight0, FWeight0, From0}
					end
			end
		end, {TemptG+1, GWeight, FWeight, From}, NotClosedNeighbours).

record_to_from(Node, FromNode, TemptG, GWeight, FWeight, From, Goal, Fun) ->
%%	io:format("|----------------~n", []),
%%	io:format("Node: ~p, FromNode: ~p~n", [Node, FromNode]),
	From2  = dict:store(Node, FromNode, From),
	GWeight2 = dict:store(Node, TemptG, GWeight),
%%	io:format("	TemptG = ~p, H = ~p~n", [TemptG,  Fun(Node, Goal)]),
	FWeight2 = dict:store(Node, Fun(Node, Goal) + TemptG, FWeight),
%%	io:format("~nFrom2: ~p~nGWeight2: ~p~nFWeight2: ~p~n", [dict:to_list(From2),dict:to_list(GWeight2),dict:to_list(FWeight2)]),
%%	io:format("----------------|~n", []),
	{TemptG, GWeight2, FWeight2, From2}
.

manhattan_distance(Node, Goal) ->
	{X1, Y1} = Node,
	{X2, Y2} = Goal,
	(X1-X2) + abs(Y1-Y2).

chebyshev_distance(Node, Goal) ->
	{X1, Y1} = Node,
	{X2, Y2} = Goal,
	max((X1-X2),(Y1-Y2)).

h_weight(Node, Goal) ->
	{X1, Y1} = Node,
	{X2, Y2} = Goal,
%%	io:format("		~p^2~n", [math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2)]),
	round(math:sqrt(math:pow(X2 - X1, 2) + math:pow(Y2 - Y1, 2))).


is_empty(Ets) ->
	case ets:last(Ets) of
		'$end_of_table' -> notfound;
		El -> El
	end.

min_weight(Opened, Dict) ->
	ets:foldl(
		fun(Node, []) ->
%%			io:format("1:	Node: ~p~n", [Node]),
			{ok,WNode} = dict:find(Node, Dict),
			{Node, WNode};
			(Node, {_, W0} = Acc) ->
%%				io:format("2:	Node: ~p, Acc: ~p~n", [Node, Acc]),
				case dict:find(Node, Dict) of
					{ok,W} when W < W0 -> {Node, W};
					_ -> Acc
				end
		end, [], Opened).

get_neighbours({1,1}) -> [{1,2}, {2,1}];
get_neighbours({1,2}) -> [{1,1}, {1,3}, {2,2}];
get_neighbours({1,3}) -> [{1,2}, {2,3}];
get_neighbours({2,1}) -> [{1,1}, {2,2}, {3,1}];
get_neighbours({2,2}) -> [{1,1}, {2,1}, {2,3}, {3,2}];
get_neighbours({2,3}) -> [{1,3}, {2,2}, {3,3}];
get_neighbours({3,1}) -> [{2,1}, {3,2}];
get_neighbours({3,2}) -> [{2,2}, {3,1}, {3,3}];
get_neighbours({3,3}) -> [{2,3}, {3,2}].

get_path(Goal, From) ->
	get_path(Goal, From, []).

get_path(Node, From, Acc) ->
	case dict:find(Node, From) of
		{ok, none} -> {ok, [Node|Acc]};
		{ok,Next} -> get_path(Next, From, [Node|Acc]);
		Err ->
			Err
	end.

is_open(OpenQueue, State) ->
			ets:member(OpenQueue, State).

is_closed(ClosedSet, State) ->
	ets:member(ClosedSet, State).

compose_path(Goal, List) ->
	io:format("PATH: ~p~n", [List]),
	ok.


