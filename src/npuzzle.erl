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
                {ok, Data} -> io:format("Data: ~p~n", [Data]);
                Error -> Error
            end;
        Error -> Error
    end.

read_map(File) ->
    read_map(File, []).

read_map(File, Acc) ->
    case io:get_line(File, '') of
        eof -> Acc;
        Line ->
            io:format("Line: ~p~n", [Line]),
            read_map(File, [Line | Acc])
    end.
