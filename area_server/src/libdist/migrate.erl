-module(migrate).

-include("obj.hrl").

-define(MAX_MESSAGES, 1000).

% API
-export([
    init/0,
    migrate/4
    ]).

% handlers
-export([
    ]).

% internal exports
-export([
    migrate_loop/4
    ]).

init() ->
	ok.

migrate(AreaSrv, Type, ObjState, TypeState) ->
	Reply = rpc:call(AreaSrv, obj_sup, start, [Type, TypeState]),
	case Reply of
		{ok, NewPid} ->
			?MODULE:migrate_loop(AreaSrv, NewPid, 0, ObjState);
		{error, Reason} ->
			{migration_failed, Reason}
	end.

% Migrated objects enter this loop to die gracefully.
migrate_loop(AreaSrv, NewPid, NrMsg, #obj{id=Id} = State) ->
	receive 
		Event ->
			NewPid ! Event,
			?MODULE:migrate_loop(AreaSrv, NewPid, NrMsg + 1, State)
	after 1000 ->
        % If we migrated to a new area, unregister from the shared obj 
        % registry.
        case std_funs:area_name() == std_funs:area_name(AreaSrv) of
            true ->
                ok;
            false ->
                dist_funs:unregister_obj(Id)
        end,
		libstd:unregister_obj(Id),
		%error_logger:info_report([{migration_successful, R}]),
        exit(normal)
	end;

% Force shutdow after 1000 messages
migrate_loop(_AreaSrv, _NewPid, ?MAX_MESSAGES, _State) ->
    error_logger:info_report([{migration_forced, 
        {max_messages, ?MAX_MESSAGES}}]),
    exit(normal).

