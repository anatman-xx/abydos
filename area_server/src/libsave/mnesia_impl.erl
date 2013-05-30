-module(libsave.mnesia_impl).

-import(error_logger).
-import(application).
-import(mnesia).
-import(lists).
-import(timer).
-import(rpc).

-import(obj_sup).
-import(obj).

-include("save_data.hrl").
-include("obj.hrl").

% API
-export([
    init/0,
    create_area/0,
    join_area/0,
    save/2,
    save_player/4,
    restore/1,
    next/0,
    next/1,
    restore_all/0
    ]).

% Temporary fix for restore_all
-export([
    ugly_init/0
    ]).

init() ->
    Info = mnesia:start(),
    Size = mnesia:table_info(save_data, size),
    error_logger:info_report([{save_data, size, Size, Info}]),
    spawn(?MODULE, ugly_init, []).

ugly_init() ->
    timer:sleep(1000),
    restore_all().

restore_all() ->
    error_logger:info_report([{libsave, restoring_all}]),
    case next() of
        {error, '$end_of_table'} ->
            error_logger:info_report([{error, nothing_to_restore}]);
        {ok, State} ->
            {ok, Pid} = obj_sup:start(State#obj.type, State),
            obj:async_call(Pid, post_init),
            restore_all(State#obj.id);
	{error, no_table} ->
            error_logger:info_report([{error, no_table}])
    end.

restore_all(PrevId) ->
    case next(PrevId) of
        {error, '$end_of_table'} ->
            done;
        {ok, State} ->
            {ok, Pid} = obj_sup:start(obj, State),
            obj:async_call(Pid, post_init),
            restore_all(State#obj.id)
    end.

create_area() ->
    case lists:member(save_data, mnesia:system_info(tables)) of 
        true ->
            pass;
        false ->
            mnesia:create_table(save_data,
            [{disc_only_copies, [node()]},
            {attributes, record_info(fields, save_data)}])
    end.

join_area() ->
    fix_later.

save(Id, State) ->
    F = fun() ->
        mnesia:write(#save_data{id=Id, state=State, node=node()})
    end,
    mnesia:transaction(F),
    error_logger:info_report([{libsave, saved, Id}]).

save_player(Id, Account, Name, State) ->
    % Perhaps make a round robin of character servers
    {ok, CharSrv} = application:get_env(areasrv, charsrv),
    Reply = rpc:call(CharSrv, charsrv, save, [Id, Account, Name, State]),
    error_logger:info_report([{Reply}]).

restore(Id) ->
    F = fun () ->
        case mnesia:read(save_data, Id) of
            [Rec] ->
                {ok, Rec#save_data.state};
            [] ->
                {error, no_obj}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} ->
            error_logger:info_report([{libsave, restored, Id}]),
            Result;
        {error, Reason} ->
            error_logger:error_report([{?MODULE, restore, error, Id, 
                Reason}])
    end.

next() ->
    F = fun() ->
        case mnesia:first(save_data) of
            [] ->
                {error, no_obj};
            '$end_of_table' ->
                {error, '$end_of_table'};
            Id ->
                {ok, Id}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} ->
            case Result of 
                {ok, Id} ->
                    restore(Id);
                {error, '$end_of_table'} ->
                    {error, '$end_of_table'};
                Error ->
                    error_logger:info_report([Error])
            end;
        {aborted, {no_exists, save_data}} ->
            {error, no_table};
        {error, Reason} ->
            error_logger:error_report([{?MODULE, next, error, 
                Reason}])
    end.

next(Id) ->
    F = fun() ->
        case mnesia:next(save_data, Id) of
            [] ->
                {error, no_obj};
            '$end_of_table' ->
                {error, '$end_of_table'};
            NextId ->
                {ok, NextId}
        end
    end,
    case mnesia:transaction(F) of
        {atomic, Result} ->
            case Result of 
                {ok, NextId} ->
                    restore(NextId);
                {error, '$end_of_table'} ->
                    {error, '$end_of_table'};
                Error ->
                    error_logger:info_report([Error])
            end;
        {error, Reason} ->
            error_logger:error_report([{?MODULE, next, error, Id, 
                Reason}])
    end.
