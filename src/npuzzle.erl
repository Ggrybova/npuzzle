%%%-------------------------------------------------------------------
%%% @author ggrybova
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Mar 2019 8:37
%%%-------------------------------------------------------------------
-module(npuzzle).
-author("ggrybova").

%% API
-export([
    start/0,
    stop/0,
    solve_puzzle/2
]).

%% ===================================================================
%% API
%% ===================================================================

-spec start() -> ok.
start() ->
    {ok, _} = application:ensure_all_started(npuzzle),
    ok.

-spec stop() -> ok | {error, term()} .
stop() ->
    application:stop(acm).

solve_puzzle(FileName, Heuristic) ->
    get_input_data(FileName, Heuristic).

%% ===================================================================
%% Internal functions
%% ===================================================================

get_input_data(FileName, Heuristic) ->
    case file:open("input_files/"++FileName, read) of
        {ok, File} ->
            case read_map(File) of
                {ok, Data} -> io:format("Data: ~p~n", [dict:to_list(Data)]),
%%                    validate(Data), // is int, 0, size
                    {ok, Start} = dict:find(0, Data),
                    {ok, Size} = dict:find(size, Data),
                    Goal =
                        case Size of
                            P when P rem 2 == 0 ->
                                {P div 2, P div 2};
                            N when N rem 2 == 1 ->
                                {N div 2 + 1, N div 2 + 1};
                            _ -> stop()
                        end,
                    io:format("Size: ~p, Start: ~p, Goal: ~p~n", [Size, Start, Goal]),
                    FinalState = final_state(Size),
                    io:format("FinalState: ~p~n", [dict:to_list(FinalState)]),
                    if
                        Data == FinalState ->
                            io:format("SUCSES~n", []);
                        true ->
                            io:format("solving...~n", [])
                    end,
                    my_a_star:a_star(Start, Goal, Heuristic, FinalState);
                Error -> Error
            end;
        Error -> Error
    end.

read_map(File) ->
    case get_size(File) of
        {ok, Size} when is_integer(Size) andalso Size > 2 ->
            read_map(File, Size, dict:store(size, Size, dict:new()), 1);
        _ -> {error, "Wrong puzzle size"}
    end.

read_map(File, Size, State, Row) ->
    case io:get_line(File, '') of
        eof -> {ok, State};
        Line ->
            case string:str(Line, "#") of
                0 ->
                    case fill_state(lists:droplast(Line), Size, State, Row) of
                        {error, _} = E -> io:format("Exit1~n", []), E;
                        {R, S} -> read_map(File, Size, S, R)
                    end;
                N when is_integer(N) andalso N > 1 ->
                    case fill_state(string:left(Line, N-1), Size, State, Row) of
                        {error, _} = E -> io:format("Exit2~n", []), E;
                        {R, S} -> read_map(File, Size, S, R)
                    end;
                _ ->
                    read_map(File, Size, State, Row)
            end
    end.

get_size(File) ->
    case io:get_line(File, '') of
        eof -> error;
        Line ->
            case string:str(Line, "#") of
                1 -> get_size(File);
                N when is_integer(N) andalso N >= 0 ->
                    Line2 =
                        case N of
                            0 -> Line;
                            _ -> string:left(Line, N - 1)
                        end,
                    case string:words(Line2) of
                        1 ->
                            {S, _} = string:to_integer(Line2),
                            {ok, S};
                        _ -> error
                    end;
                _ -> error
            end
    end.

fill_state([], _, State, Row) ->
%%    io:format("N: ~p~n", [Row]),
    {Row, State};

fill_state(Line, Size, State, Row) ->
%%    io:format("RowN: ~p~n", [Row]),
    case string:words(Line) of
        Size ->
            Words = string:tokens(Line, " "),
%%            io:format("**  Words: ~p~n", [Words]),
            case fill_row(Row, 1, Size, Words, State) of
                {ok, State2} -> {Row+1, State2};
                Error -> Error
            end;
        N when N < Size ->
            case string:tokens(Line, " ") of
                [] -> {Row, State}; %% !!!!!
                _ -> {error, "You have not enough sells in row"}
            end;
        _ ->
            {error, "You have more sells in row than puzzle size"}
    end.

fill_row(_, _, _, [], State) ->
%%    io:format("~p: {~p,~p}; Words: []~n", [Size, X, Y]),
    {ok, State};

fill_row(X, Y, Size, [Word|Tail], State) when X =< Size ->
%%    io:format("~p: {~p,~p}; Words: ~p~n", [Size, X, Y, Words]),
    {Nbr, _} = string:to_integer(Word),
    State2 = dict:store(Nbr, {X, Y}, State),
    fill_row(X, Y+1, Size, Tail, State2);

fill_row(_, _, _, _, _) ->
    {error, "You have more rows than puzzle size"}.

final_state(3) ->
    State0 = dict:store(size, 3, dict:new()),
    State1 = dict:store(0, {2, 2}, State0),
    State2 = dict:store(1, {1, 1}, State1),
    State3 = dict:store(2, {1, 2}, State2),
    State4 = dict:store(3, {1, 3}, State3),
    State5 = dict:store(4, {2, 3}, State4),
    State6 = dict:store(5, {3, 3}, State5),
    State7 = dict:store(6, {3, 2}, State6),
    State8 = dict:store(7, {3, 1}, State7),
    dict:store(8, {2, 1}, State8);

final_state(_) ->
    io:format("STOP!!!~n", []),
    exit(final_state).
