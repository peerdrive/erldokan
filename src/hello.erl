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
-module(hello).
-include("erldokan.hrl").

-export([create_file/8, open_directory/4, find_files/4]).
-export([init/1, handle_info/2, terminate/2, code_change/3]).

init(_Args) ->
	{ok, undefined}.

handle_info(Msg, State) ->
	io:format("hello: handle_info(~p)~n", [Msg]),
	{norepy, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



create_file(S, _From, _FileName, _AccMode, _ShMode, _CrDisp, _Flags, _FI) ->
	{reply, {error, -2}, S}.

open_directory(S, _From, _FileName, _FI) ->
	{reply, #dokan_reply_open{context=1, is_directory=true}, S}.

find_files(S, _From, <<"\\">>, _FI) ->
	Root = [
		#dokan_reply_find{
			file_attributes = ?FILE_ATTRIBUTE_READONLY,
			file_size = 0,
			file_name = <<"test.txt">>
		}
	],
	{reply, Root, S};

find_files(S, _From, _FileName, _FI) ->
	{reply, [], S}.
