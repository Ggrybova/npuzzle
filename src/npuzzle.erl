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
            io:format("     Size: ~p~n", [Size]),
            read_map(File, Size, dict:store(size, Size, dict:new()));
        _ -> {error, "Wrong puzzle size"}
    end.

read_map(File, Size, State) ->
    case io:get_line(File, '') of
        eof -> {ok, State};
        Line ->
            case string:str(Line, "#") of
                0 ->
                    fill_state(lists:droplast(Line), Size, State),
                    read_map(File, Size, State);
                N when is_integer(N) andalso N > 1 ->
                    fill_state(string:left(Line, N-1), Size, State),
                    read_map(File, Size, State);
                _ ->
                    read_map(File, Size, State)
            end
    end.

get_size(File) ->
    case io:get_line(File, '') of
        eof -> error;
        Line ->
            case string:str(Line, "#") of
                0 ->
                    case string:words(Line) of
                        1 ->
                            {S, _} = string:to_integer(Line),
                            {ok, S};
                        _ ->
                            error
                    end;
                N when is_integer(N) andalso N > 0 ->
                    get_size(File);
                _ ->
                    error
            end
    end.

fill_state(Line, Size, State) ->
    io:format("State: ~p~n", [dict:to_list(State)]).

