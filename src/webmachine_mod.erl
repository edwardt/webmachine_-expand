%%%-------------------------------------------------------------------
%%% File    : webmachine_mod.erl
%%% Author  : Cuong Le <cuonglb@facemain.com>
%%% Description : management modules for webmachine application
%%%
%%% Created : 31 Dec 2009 by Cuong Le <cuonglb@facemain.com>
%%%-------------------------------------------------------------------
-module(webmachine_mod).
-version('0.1').
-author('Cuong Le <cuonglb@facemain.com>').

%% API
-export([
	 start/0,
	 start_modules/0,
	 stop_modules/0,
	 get_opt/3
	]).

-export([
	 behaviour_info/1
	]).

-include("webmachine_mod.hrl").

-record(webmachine_module, 
	{
	  module, 
	  opts
	 }
       ).

%%====================================================================
%% API
%%====================================================================

%% @spec behaviour(Val) -> terms | undefined
%% @doc define functions callback behaviour
behaviour_info(callbacks)->
    [{start,1},{stop,0}];
behaviour_info(_Other) ->
    undefined.

%% @spec start() -> ok
%% @doc create new ets webmachine_modules table
start()->
    ets:new(webmachine_modules,[
			       named_table,
			       public,
			       {keypos, #webmachine_module.module}
			      ]),
    ok.

%% @spec start_modules() -> ok
%% @doc start modules
start_modules()->
    case webmachine_config:get_opt(module) of
	undefined ->
	    ok;
	Modules ->
	    lists:foreach(
	      fun({Module, Opts})->
		      module(start, Module, Opts),
		      io:format("** start module : ~p~n",[Module])
	      end, Modules
	     )
    end.

%% @spec stop_modules() -> ok
%% @doc stop modules
stop_modules()->
    case webmachine_config:get_opt(module) of
	undefined ->
	    ok;
	Modules ->
	    lists:foreach(
	      fun({Module, _Opts}) ->
		      module(stop, Module, []),
		      io:format("** stop module : ~p~n",[Module])
	      end, Modules
	     )
    end.


%% @spec module(start | stop, Module, Opts) -> error | ok
%% @doc start/stop module
module(start, Module, Opts)->
    ets:insert(webmachine_modules,
	       #webmachine_module{
		 module = Module,
		 opts = Opts
		 }),
    case catch Module:start(Opts) of
	{'EXIT', _Reason} ->
	    ets:delete(webmachine_modules, {Module}),
	    error;
	_ ->
	    ok
    end;
module(stop, Module, _) ->
    R = case catch Module:stop() of
	    {'EXIT', _Reason} ->
		error;
	    {wait, Process} ->
		wait_for_process_stop(Process, erlang:monitor(process, Process)),
		ok;
	    _ ->
		ok
	end,
    if
	R == ok ->
	    ets:delete(webmachine_modules, {Module}),
	    ok;
	true ->
	    error
    end.

%% @spec get_opt(Opt, Opts, Default) -> {undefined, Opt} | Default | Val
%% @doc get option with key from options terms
get_opt(Opt, Opts, Default)->
    case lists:keysearch(Opt, 1, Opts) of
	false ->
	    if
		Default == [] ->
		    throw({undefined, Opt});
		true ->
		    Default
	    end;
	{value, {_, Val}}->
	    Val
    end.
    

%%====================================================================
%% Internal functions
%%====================================================================

%% @spec wait_for_process_stop(Process, Ref) -> ok
%% @doc waiting for stop process with monitor ref.
wait_for_process_stop([], Ref) ->
    receive
	{'DOWN', Ref, _, _, _} ->
	    ok
    after 5000 ->
	    ok
    end;
wait_for_process_stop(Process, Ref) ->
    receive
	{'DOWN', Ref, _, _, _} ->
	    ok
    after 5000 ->
	    catch exit(whereis(Process), kill),
	    wait_for_process_stop([], Ref)
    end.
