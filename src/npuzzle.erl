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
    solve_puzzle/1
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

solve_puzzle(FileName) ->
    get_input_data(FileName).

%% ===================================================================
%% Internal functions
%% ===================================================================

get_input_data(FileName) ->
    case file:open("input_files/"++FileName, read) of
        {ok, File} ->
            case read_map(File) of
                {ok, Data} -> io:format("Data: ~p~n", [dict:to_list(Data)]);
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
    io:format("N: ~p~n", [Row]),
    {Row, State};

fill_state(Line, Size, State, Row) ->
    io:format("RowN: ~p~n", [Row]),
    case string:words(Line) of
        Size ->
            Words = string:tokens(Line, " "),
            io:format("**  Words: ~p~n", [Words]),
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
    State2 = dict:store({X, Y}, Nbr, State),
    fill_row(X, Y+1, Size, Tail, State2);

fill_row(_, _, _, _, _) ->
    {error, "You have more rows than puzzle size"}.
