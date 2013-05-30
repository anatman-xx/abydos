%%----------------------------------------------------------------------
%% @author Christian Flodihn <christian@flodihn.se>
%% @copyright Chrisitan Flodihn
%% @doc
%% This module implements a group object which is created when players
%% (or NPS's) group together.
%% @end
%%----------------------------------------------------------------------
-module(libobj.group).


-import(error_logger).
-import(dict).
-import(io).

-import(obj).

%% @headerfile "obj.hrl"
-include("obj.hrl").

%% @headerfile "vec.hrl"
-include("vec.hrl").

% Required exports
-export([
    init/1,
    post_init/1
    ]).

% Group object exports
-export([
    add/4,
    info/2,
    set_leader/3,
    command/3,
    command/4
    ]).

%%----------------------------------------------------------------------
%% spec init(State) -> ok
%% where 
%%      State = obj()
%% @doc
%% Initiates the group object.
%% @end
%%----------------------------------------------------------------------
init(State) ->
    {ok, State#obj{parents=[obj]}}.

post_init(State) ->
    {ok, State}.

%%----------------------------------------------------------------------
%% spec add(From, Obj, State) -> {noreply, State}
%% where 
%%      From = pid(),
%%      Obj= pid(),
%%      State = obj()
%% @doc
%% Add an object to the group.
%% @end
%%----------------------------------------------------------------------
add(_From, Id, Obj, State) ->
    case get_members(State) of 
        undefined ->
            Dict = dict:new(),
            NewDict = dict:append(Id, Obj, Dict),
            NewState = save_members(NewDict, State),
            set_group(Obj, self()),
            {noreply, NewState};
        Dict ->
            NewDict = dict:append(Id, Obj, Dict),
            NewState = save_members(NewDict, State),
            set_group(Obj, self()),
            {noreply, NewState}
    end.

set_group(Obj, Group) ->
    obj:async_call(Obj, set_group, [Group]).

set_leader(_From, Obj, State) ->
    {ok, _Reply, NewState} = obj:call_self(set_property, [leader, Obj], 
        State),
    {noreply, NewState}.

command(From, Fun, State) ->
    command(From, Fun, [], State).

command(_From, Fun, Args, State) ->
    case get_members(State) of
        undefined ->
            {noreply, State};
        Dict ->
            %Perhaps make a check so only leader can send commands.
            send_command(dict:fetch_keys(Dict), Fun, Args, Dict),
            {noreply, State}
    end.

send_command([], _Fun, _Args, _Dict) ->
    done;
  
send_command([Key | Rest], Fun, Args, Dict) ->
    [Pid] = dict:fetch(Key, Dict),
    %error_logger:info_report([{obj, async_call, Pid, Fun, Args}]),
    obj:async_call(Pid, Fun, Args),
    send_command(Rest, Fun, Args, Dict).

info(_From, State) ->
    case get_members(State) of
        undefined ->
            io:format("Group is empty.~n", []),
            {noreply, State};
        Dict ->
            io:format("~n=============== Group Members ===============~n", 
                []),
            print_members(dict:fetch_keys(Dict), Dict),
            {noreply, State}
    end.

print_members([], _Dict) ->
    io:format("~n=============================================~n", []);

print_members([Key | Rest], Dict) ->
    [Value] = dict:fetch(Key, Dict),
    io:format("Id: ~p pid: ~p.~n", [Key, Value]),
    print_members(Rest, Dict).

save_members(Dict, State) ->
    case obj:call_self(set_property, [members, Dict], State) of
        {ok, _Reply, NewState} ->
            NewState
    end.

get_members(State) ->
    case obj:call_self(get_property, [members], State) of
        {ok, undefined, _NewState} ->
            undefined;
        {ok, Dict, _NewState} ->
            Dict
    end.

