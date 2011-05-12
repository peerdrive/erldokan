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

-export([create_file/8, open_directory/4, find_files/4, create_directory/4,
         get_file_information/4, read_file/6]).
-export([init/1, handle_info/2, terminate/2, code_change/3]).

-record(state, {vnodes}).
-record(file, {data}).
-record(dir, {listing}).

init(_Args) ->
	State = #state{
		vnodes=gb_trees:from_orddict([
			{0, #dir{listing=[{"text.txt", 1}]}},
			{1, #file{data= <<"Hello World...">>}}])
	},
	{ok, State}.

handle_info(Msg, State) ->
	io:format("hello: handle_info(~p)~n", [Msg]),
	{norepy, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.



create_file(S, _From, FileName, _AccMode, _ShMode, _CrDisp, _Flags, _FI) ->
	case lookup(FileName, S) of
		#file{} ->
			{reply, #dokan_reply_open{is_directory=false}, S};
		#dir{} ->
			{reply, #dokan_reply_open{is_directory=true}, S};
		_ ->
			{reply, {error, -2}, S}
	end.


open_directory(S, _From, FileName, _FI) ->
	case lookup(FileName, S) of
		#dir{} ->
			{reply, #dokan_reply_open{is_directory=true}, S};
		_ ->
			{reply, {error, -2}, S}
	end.


create_directory(#state{vnodes=VNodes} = S, _From, FileName, _FI) ->
	case lookup(FileName, S) of
		#dir{} ->
			{reply, {error, -183}, S}; % already exists
		#file{} ->
			{reply, {error, -183}, S}; % already exists
		{stop, DirIno, Name} ->
			case lists:any(fun(C) -> C == $\\ end, Name) of
				true ->
					{reply, {error, -3}, S}; % path not found
				false ->
					{Max, _} = gb_trees:largest(VNodes), Ino = Max+1,
					VN2 = gb_trees:enter(Ino, #dir{listing=[]}, VNodes),
					VN3 = add_dir_entry(DirIno, Ino, Name, VN2),
					{reply, #dokan_reply_open{is_directory=true}, S#state{vnodes=VN3}}
			end;

		error ->
			{reply, {error, -3}, S} % path not found
	end.


add_dir_entry(DirIno, Ino, Name, VNodes) ->
	Dir = #dir{listing=List} = gb_trees:get(DirIno, VNodes),
	NewList = orddict:store(Name, Ino, List),
	gb_trees:update(DirIno, Dir#dir{listing=NewList}, VNodes).


find_files(#state{vnodes=VNodes} = S, _From, Path, _FI) ->
	case lookup(Path, S) of
		#dir{listing=Dir} ->
			DirEntries = lists:map(
				fun({Name, Ino}) ->
					{unicode:characters_to_binary(Name), gb_trees:get(Ino, VNodes)}
				end,
				Dir),
			List = lists:foldl(
				fun
					({Name, #dir{}}, Acc) ->
						[#dokan_reply_find{
							file_attributes = ?FILE_ATTRIBUTE_DIRECTORY,
							file_size = 0,
							file_name = Name
						} | Acc];
					({Name, #file{data=Data}}, Acc) ->
						[#dokan_reply_find{
							file_attributes = ?FILE_ATTRIBUTE_READONLY,
							file_size = size(Data),
							file_name = Name
						} | Acc]
				end,
				[],
				DirEntries),
			{reply, List, S};

		_ ->
			{reply, {error, -2}, S}
	end.


get_file_information(S, _From, FileName, _FI) ->
	case lookup(FileName, S) of
		#file{data=Data} ->
			Attr = #dokan_reply_fi{
				file_attributes = ?FILE_ATTRIBUTE_READONLY,
				file_size = size(Data)
			},
			{reply, Attr, S};
		#dir{} ->
			Attr = #dokan_reply_fi{
				file_attributes = ?FILE_ATTRIBUTE_DIRECTORY
			},
			{reply, Attr, S};
		_ ->
			{reply, {error, -2}, S}
	end.


read_file(S, _From, FileName, Length, Offset, _FI) ->
	case lookup(FileName, S) of
		#file{data=Data} ->
			Size = size(Data),
			InOffset = if
				Offset >= Size -> Size;
				true -> Offset
			end,
			InLength = if
				InOffset+Length > Size -> Size-InOffset;
				true -> Length
			end,
			{reply, binary:part(Data, InOffset, InLength), S};
		_ ->
			{reply, {error, -2}, S}
	end.


lookup(BinName, #state{vnodes=VNodes} = S) ->
	case walk(BinName, S) of
		Ino when is_integer(Ino) ->
			gb_trees:get(Ino, VNodes);
		Error ->
			Error
	end.


walk(BinName, State) ->
	case unicode:characters_to_list(BinName, utf8) of
		{error, _, _} ->
			error;
		{incomplete, _, _} ->
			error;
		Name ->
			do_walk(Name, 0, State)
	end.


do_walk("", Ino, _S) ->
	Ino;

do_walk("\\", Ino, _S) ->
	Ino;

do_walk([$\\ | Path], DirIno, #state{vnodes=VNodes} = S) ->
	{Name, Rest} = lists:splitwith(fun(C) -> C =/= $\\ end, Path),
	case gb_trees:get(DirIno, VNodes) of
		#dir{listing=Dir} ->
			case orddict:find(Name, Dir) of
				{ok, Ino} ->
					do_walk(Rest, Ino, S);
				error ->
					{stop, DirIno, Path}
			end;

		#file{} ->
			error %% can't descend from a file
	end;

do_walk(_, _, _) ->
	error.

