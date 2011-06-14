%%
%% This file is part of ErlDokan.
%% Copyright (C) 2011  Jan Klötzke <jan DOT kloetzke AT freenet DOT de>
%%
%% ErlDokan is free software: you can redistribute it and/or modify it under
%% the terms of the GNU Lesser General Public License as published by the Free
%% Software Foundation, either version 3 of the License, or (at your option)
%% any later version.
%%
%% ErlDokan is distributed in the hope that it will be useful,
%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%% GNU Lesser General Public License for more details.
%%
%% You should have received a copy of the GNU Lesser General Public License
%% along with ErlDokan. If not, see <http://www.gnu.org/licenses/>.
%%
-module(erldokan).
-behavior(gen_server).

-export([start/3, start/4, start_link/3, start_link/4,
         reply/2]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

-export([behaviour_info/1]).

-define(DRV_NAME,         "erldokan_drv").
-define(DRV_CTRL_MOUNT,   0).
-define(DRV_CTRL_UNMOUNT, 1).

-record(state, {port, mod, state}).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].

behaviour_info(callbacks) ->
    [{init, 1}, {handle_info, 2}, {terminate, 2}, {code_change, 3}
		| behaviour_info_dokan_cb()];

behaviour_info(_Other) ->
    undefined.


behaviour_info_dokan_cb() ->
	[
		{cleanup, 4},
		{close_file, 4},
		{create_directory, 4},
		{create_file, 8},
		{delete_directory, 4},
		{delete_file, 4},
		{find_files, 4},
		{find_files_with_pattern, 5},
		{flush_file_buffers, 4},
		{get_disk_free_space, 3},
		{get_file_information, 4},
		{get_volume_information, 3},
		{lock_file, 6},
		{move_file, 6},
		{open_directory, 4},
		{read_file, 6},
		{set_allocation_size, 5},
		{set_end_of_file, 5},
		{set_file_attributes, 5},
		{set_file_time, 7},
		{unlock_file, 6},
		{unmount, 3},
		{write_file, 6}
	].


start(Mod, Args, Options) ->
	{DokanOpt, GenSrvOpt} = split_options(Options),
	gen_server:start(?MODULE, {Mod, Args, DokanOpt}, GenSrvOpt).

start(Name, Mod, Args, Options) ->
	{DokanOpt, GenSrvOpt} = split_options(Options),
	gen_server:start(Name, ?MODULE, {Mod, Args, DokanOpt}, GenSrvOpt).

start_link(Mod, Args, Options) ->
	{DokanOpt, GenSrvOpt} = split_options(Options),
	gen_server:start_link(?MODULE, {Mod, Args, DokanOpt}, GenSrvOpt).

start_link(Name, Mod, Args, Options) ->
	{DokanOpt, GenSrvOpt} = split_options(Options),
	gen_server:start_link(Name, ?MODULE, {Mod, Args, DokanOpt}, GenSrvOpt).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% gen_server callbacks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%% @hidden
init({Mod, Args, Options}) ->
	process_flag(trap_exit, true),
	SupportedOps = scan_module(Mod),
	case erl_ddll:load(code:priv_dir(?MODULE), ?DRV_NAME) of
		ok -> ok;
		{error, already_loaded} -> ok;
		LoadErr -> throw({stop, LoadErr})
	end,
	Port = open_port({spawn_driver, ?DRV_NAME}, [binary]),
	case erlang:port_call(Port, ?DRV_CTRL_MOUNT, {Options, SupportedOps}) of
		ok -> ok;
		{error, _} = InitErr -> throw({stop, InitErr})
	end,
	case catch Mod:init(Args) of
		{ok, State} ->
			{ok, #state{port=Port, mod=Mod, state=State}};
		{ok, State, Timeout} ->
			{ok, #state{port=Port, mod=Mod, state=State}, Timeout};
		ModErr ->
			port_close(Port),
			ModErr
	end.


%% @hidden
handle_call(_Request, _From, State) ->
	{reply, undefined, State}.


%% @hidden
handle_cast(_Request, State) ->
	{noreply, State}.


%% @hidden
handle_info({Port, _, _, _} = Request, #state{port=Port} = S) ->
	handle_request(Request, S);

handle_info({'EXIT', Port, Reason}, #state{port=Port} = S) ->
	{stop, {abort, Reason}, S};

handle_info({Port, unmounted, Reason}, #state{port=Port} = S) ->
	case Reason of
		0  -> {stop, normal, S};
		-1 -> {stop, dokan_error, S};
		-2 -> {stop, dokan_drive_letter_error, S};
		-3 -> {stop, dokan_driver_install_error, S};
		-4 -> {stop, dokan_start_error, S};
		-5 -> {stop, dokan_mount_error, S};
		-6 -> {stop, dokan_mount_point_error, S};
		_  -> {stop, unknown_error, S}
	end;

handle_info(Msg, #state{mod=Mod, state=State} = S) ->
	Reply = try
		Mod:handle_info(Msg, State)
	catch
		throw:Term -> Term
	end,
	case Reply of
		{noreply, NewState} ->
			{noreply, S#state{state=NewState}};
		{noreply, NewState, Timeout} ->
			{noreply, S#state{state=NewState}, Timeout};
		{stop, Reason, NewState} ->
			{stop, Reason, S#state{state=NewState}};
		Else ->
			Else
	end.


%% @hidden
terminate(Reason, #state{port=Port, mod=Mod, state=State}) ->
	catch port_close(Port),
	erl_ddll:unload(?DRV_NAME),
	Mod:terminate(Reason, State).


%% @hidden
code_change(OldVsn, #state{mod=Mod, state=State} = S, Extra) ->
	{ok, NewState} = Mod:code_change(OldVsn, State, Extra),
	{ok, S#state{state=NewState}}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Dokan veneer functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

handle_request({Port, Req, Op, Args}, #state{mod=Mod, state=State} = S) ->
	%io:format("=> #~p ~p ~p~n", [Req, Op, Args]),
	From = {Port, Req},
	Result = try
		apply(Mod, Op, [State, From | Args])
	catch
		throw:Term -> Term
	end,
	case Result of
		{noreply, NewState} ->
			{noreply, S#state{state=NewState}};
		{reply, Reply, NewState} ->
			reply(From, Reply),
			{noreply, S#state{state=NewState}};
		{stop, Reason, Reply, NewState} ->
			reply(From, Reply),
			{stop, Reason, S#state{state=NewState}};
		{stop, Reason, NewState} ->
			{stop, Reason, S#state{state=NewState}};
		Else ->
			Else
	end.


reply({Port, Req}, Reply) when is_binary(Reply) ->
	Status = term_to_binary(ok),
	port_command(Port, [<<Req:32/native-unsigned, (size(Status)):32/native-unsigned>>,
		Status, Reply]);

reply({Port, Req}, Reply) ->
	%io:format("<- #~p ~p~n", [Req, Reply]),
	Status = term_to_binary(Reply),
	port_command(Port, [<<Req:32/native-unsigned, (size(Status)):32/native-unsigned>>,
		Status]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Local utility functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

split_options(Options) ->
	split_options_loop(Options, [], []).


split_options_loop([], DokAcc, GenAcc) ->
	{DokAcc, GenAcc};

split_options_loop([{Prop, _}=Opt | Rest], DokAcc, GenAcc)
when (Prop == mountpoint) or
     (Prop == threads) or
     (Prop == debug_output) or
     (Prop == drive_type) ->
	split_options_loop(Rest, [Opt | DokAcc], GenAcc);

split_options_loop([Opt | Rest], DokAcc, GenAcc) ->
	split_options_loop(Rest, DokAcc, [Opt | GenAcc]).


%% Shamelessly taken from fuserl
scan_module(Module) ->
	{module, Module} = code:ensure_loaded(Module),
	[Fun || {Fun, Arity} <- behaviour_info_dokan_cb(),
		erlang:function_exported(Module, Fun, Arity)].

