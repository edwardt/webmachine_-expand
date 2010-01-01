%%%-------------------------------------------------------------------
%%% File    : webmachine_config.erl
%%% Author  : CuongLe <cuongle@facemain.com>
%%% Description : management config file of webmachine application
%%%
%%% Created : 31 Dec 2009 by CuongLe <cuongle@facemain.com>
%%%-------------------------------------------------------------------
-module(webmachine_config).
-version('0.1').
-author('Cuong Le <cuongle@facemain.com').

%% API
-export([
	 start/0,
	 get_opt/1,
	 db_init/0
	]).

-include("webmachine_config.hrl").

%%====================================================================
%% API
%%====================================================================

%% @spec start() -> ok
%% @doc create config table for store module's option and load config (terms) from file
start()->
    create_config_table(),
    ConfigFile = filename:join([env_config(), ?CONFIG_FILE]),
    load_config_from_file(ConfigFile),
    ok.

%% @spec get_opt(Opt) -> Val | undefined
%% @doc return Val with the key is Opt in the table config
get_opt(Opt)->
    case ets:lookup(config, Opt) of
	[#config{val = Val}]->
	    Val;
	_->
	    undefined
    end.

%% @spec
%% @doc
db_init()->
    case mnesia:system_info(extra_db_nodes) of
	[]->
	    mnesia:create_schema([node()]);
	_ ->
	    ok
    end,
    mnesia:start(),
    mnesia:wait_for_tables(mnesia:system_info(local_tables), infinity).
    
%%====================================================================
%% Internal functions
%%====================================================================


%% @spec create_config_table() -> ok
%% @doc create table mnesia::config
create_config_table()->
    mnesia:create_table(config, [{disc_copies, [node()]},{attributes, record_info(fields, config)}]),
    mnesia:add_table_copy(config, node(), ram_copies),
    ok.


%% @spec process_term(Term, State) -> State
%% @doc anonymous function for process term
process_term(Term, State)->
    case Term of
	{module, Val} ->
	    add_opt(module, Val, State);
	{_Opt, _Val} ->
	    State
    end.


%% @spec add_opt(Opt, Val, State) -> State
%% @doc add the config record in to opts list
add_opt(Opt, Val, State)->
    State#state{
      opts = [#config{key = Opt, val = Val} | State#state.opts]
     }.


%% @spec env_config()-> string()
%% @doc return string() value is path of config file
env_config()->
    % get application env
    case application:get_env(config) of
	{ok, Path}->
	    Path;
	undefined ->
	    % get os export env
	    case os:getenv("APP_CONFIG_PATH") of
		false ->
		    {ok, Application} = application:get_application(),
		    code:lib_dir(Application, conf);
		Path ->
		    Path
	    end
    end.


%% @spec load_config_from_file(ConfigFile)-> ok | error
%% @doc return ok if file config open success otherwise return error
load_config_from_file(ConfigFile)->
    case file:consult(ConfigFile) of
	{ok, Terms}->
	    State = #state{},
	    Res = lists:foldl(fun process_term/2, State, Terms),
	    set_opts(Res),
	    ok;
	{error, _Reason}->
	    error
    end.


%% @spec set_opts(State) -> ok | exit
%% @doc return ok if mnesia database delete/write Opts success otherwise exit
set_opts(State)->
    Opts = lists:reverse(State#state.opts),
    F = fun()->
		Keys = mnesia:all_keys(config),
		lists:foreach(fun(Key)->
				      mnesia:delete({config, Key})
			      end, Keys),
		lists:foreach(fun(Opt)->
				      mnesia:write(Opt)
			      end, Opts)
	end,
    case mnesia:transaction(F) of
	{atomic, _}->
	    ok;
	{aborted, {no_exists, Table}} ->
	    exit(io_lib:format("Error reading Mnesia database : ~p~n",[Table]))
    end.
