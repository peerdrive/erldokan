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
-include("winerror.hrl").

-export([create_file/8, open_directory/4, find_files/4, create_directory/4,
         get_file_information/4, read_file/6, write_file/6, delete_file/4,
         delete_directory/4, close_file/4, move_file/6, set_end_of_file/5]).
-export([init/1, handle_info/2, terminate/2, code_change/3]).

-record(state, {vnodes}).
-record(file, {data, deleted=false}).
-record(dir, {listing, deleted=false}).

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



create_file(S, _From, FileName, _AccMode, _ShMode, _CrDisp, _Flags, _DFI) ->
	case lookup(FileName, S) of
		#file{} ->
			{reply, #dokan_reply_open{is_directory=false}, S};
		#dir{} ->
			{reply, #dokan_reply_open{is_directory=true}, S};
		_ ->
			{reply, {error, -?ERROR_FILE_NOT_FOUND}, S}
	end.


open_directory(S, _From, FileName, _DFI) ->
	case lookup(FileName, S) of
		#dir{} ->
			{reply, #dokan_reply_open{is_directory=true}, S};
		_ ->
			{reply, {error, -?ERROR_FILE_NOT_FOUND}, S}
	end.


create_directory(#state{vnodes=VNodes} = S, _From, FileName, _DFI) ->
	case lookup(FileName, S) of
		#dir{} ->
			{reply, {error, -?ERROR_ALREADY_EXISTS}, S};
		#file{} ->
			{reply, {error, -?ERROR_ALREADY_EXISTS}, S};
		{stop, DirIno, Name} ->
			case lists:any(fun(C) -> C == $\\ end, Name) of
				true ->
					{reply, {error, -?ERROR_PATH_NOT_FOUND}, S};
				false ->
					{Max, _} = gb_trees:largest(VNodes), Ino = Max+1,
					VN2 = gb_trees:enter(Ino, #dir{listing=[]}, VNodes),
					VN3 = add_dir_entry(DirIno, Name, Ino, VN2),
					{reply, #dokan_reply_open{is_directory=true}, S#state{vnodes=VN3}}
			end;

		error ->
			{reply, {error, -?ERROR_PATH_NOT_FOUND}, S}
	end.


add_dir_entry(DirIno, Name, Ino, VNodes) ->
	Dir = #dir{listing=List} = gb_trees:get(DirIno, VNodes),
	NewList = orddict:store(Name, Ino, List),
	gb_trees:update(DirIno, Dir#dir{listing=NewList}, VNodes).


close_file(#state{vnodes=VNodes} = S, _From, FileName, DFI) ->
	case DFI#dokan_file_info.delete_on_close of
		true ->
			{ok, Parent, Name, Ino} = walk(FileName, S),
			NewVNodes = gb_trees:delete(Ino, del_dir_entry(Parent, Name, VNodes)),
			{reply, ok, S#state{vnodes=NewVNodes}};

		false ->
			{reply, ok, S}
	end.


del_dir_entry(DirIno, Name, VNodes) ->
	Dir = #dir{listing=Listing} = gb_trees:get(DirIno, VNodes),
	NewListing = orddict:erase(Name, Listing),
	gb_trees:update(DirIno, Dir#dir{listing=NewListing}, VNodes).


find_files(#state{vnodes=VNodes} = S, _From, Path, _DFI) ->
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
			{reply, {error, -?ERROR_FILE_NOT_FOUND}, S}
	end.


get_file_information(S, _From, FileName, _DFI) ->
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
			{reply, {error, -?ERROR_FILE_NOT_FOUND}, S}
	end.


read_file(S, _From, FileName, Length, Offset, _DFI) ->
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
			{reply, {error, -?ERROR_FILE_NOT_FOUND}, S}
	end.


write_file(#state{vnodes=VNodes} = S, _From, FileName, Data, Offset, DFI) ->
	case walk(FileName, S) of
		{ok, _, _, Ino} ->
			case gb_trees:get(Ino, VNodes) of
				#file{data=OldData} = File ->
					NewData = case DFI#dokan_file_info.write_to_eof of
						false ->
							do_write(OldData, Data, Offset);
						true ->
							<<OldData/binary, Data/binary>>
					end,
					NewFile = File#file{data=NewData},
					S2 = S#state{vnodes=gb_trees:update(Ino, NewFile, VNodes)},
					{reply, {ok, size(Data)}, S2};

				_ ->
					{reply, {error, -?ERROR_FILE_NOT_FOUND}, S}
			end;

		_ ->
			{reply, {error, -?ERROR_FILE_NOT_FOUND}, S}
	end.


do_write(OldData, Data, Offset) ->
	OldSize = size(OldData),
	Length = size(Data),
	Prefix = if
		Offset <  OldSize -> binary:part(OldData, 0, Offset);
		Offset == OldSize -> OldData;
		Offset >  OldSize -> <<OldData/binary, 0:((Offset-OldSize)*8)>>
	end,
	Postfix = if
		Offset+Length < OldSize ->
			binary:part(OldData, Offset+Length, OldSize-Offset-Length);
		true ->
			<<>>
	end,
	<<Prefix/binary, Data/binary, Postfix/binary>>.


delete_file(#state{vnodes=VNodes} = S, _From, FileName, _DFI) ->
	case walk(FileName, S) of
		{ok, _, _, Ino} ->
			case gb_trees:get(Ino, VNodes) of
				#file{} = File ->
					NewFile = File#file{deleted=true},
					{reply, ok, S#state{vnodes=gb_trees:update(Ino, NewFile, VNodes)}};
				#dir{} ->
					{reply, {error, -?ERROR_ACCESS_DENIED}, S}
			end;

		_ ->
			{reply, {error, -?ERROR_FILE_NOT_FOUND}, S}
	end.


delete_directory(#state{vnodes=VNodes} = S, _From, FileName, _DFI) ->
	case walk(FileName, S) of
		{ok, _, _, Ino} ->
			case gb_trees:get(Ino, VNodes) of
				#dir{listing=Listing} = Dir when Listing == [] ->
					NewDir = Dir#dir{deleted=true},
					{reply, ok, S#state{vnodes=gb_trees:update(Ino, NewDir, VNodes)}};
				#dir{} ->
					{reply, {error, -?ERROR_DIR_NOT_EMPTY}, S};
				#file{} ->
					{reply, {error, -?ERROR_ACCESS_DENIED}, S}
			end;

		_ ->
			{reply, {error, -?ERROR_FILE_NOT_FOUND}, S}
	end.


% TODO: check for open handles
move_file(#state{vnodes=VNodes}=S, _From, OldName, NewName, Replace, _DFI) ->
	case walk(OldName, S) of
		{ok, OldParent, OldPName, Ino} ->
			case walk(NewName, S) of
				{ok, NewParent, NewPName, ExistIno} ->
					OldType = element(1, gb_trees:get(Ino, VNodes)),
					ExistType = element(1, gb_trees:get(ExistIno, VNodes)),
					if
						OldType =/= ExistType -> throw({reply, {error, -?ERROR_ACCESS_DENIED}, S});
						ExistType =/= file -> throw({reply, {error, -?ERROR_ACCESS_DENIED}, S});
						not Replace -> throw({reply, {error, -?ERROR_ACCESS_DENIED}, S});
						true -> ok
					end,
					VN1 = del_dir_entry(OldParent, OldPName, VNodes),
					VN2 = del_dir_entry(NewParent, NewPName, VN1),
					VN3 = gb_trees:delete(ExistIno, VN2),
					VN4 = add_dir_entry(NewParent, NewPName, Ino, VN3),
					{reply, ok, S#state{vnodes=VN4}};

				{stop, NewParent, NewPName} ->
					case lists:any(fun(C) -> C == $\\ end, NewPName) of
						true ->
							{reply, {error, -?ERROR_PATH_NOT_FOUND}, S};
						false ->
							VN1 = del_dir_entry(OldParent, OldPName, VNodes),
							VN2 = add_dir_entry(NewParent, NewPName, Ino, VN1),
							{reply, ok, S#state{vnodes=VN2}}
					end;

				error ->
					{reply, {error, -?ERROR_PATH_NOT_FOUND}, S}
			end;

		_ ->
			{reply, {error, -?ERROR_FILE_NOT_FOUND}, S}
	end.


set_end_of_file(#state{vnodes=VNodes}=S, _From, FileName, Offset, _DFI) ->
	case walk(FileName, S) of
		{ok, _, _, Ino} ->
			case gb_trees:get(Ino, VNodes) of
				#file{data=Data} = File ->
					NewFile = if
						Offset > size(Data) ->
							File#file{data=binary:part(Data, 0, Offset)};
						true ->
							File
					end,
					{reply, ok, S#state{vnodes=gb_trees:update(Ino, NewFile, VNodes)}};

				#dir{} ->
					{reply, {error, -?ERROR_ACCESS_DENIED}, S}
			end;

		_ ->
			{reply, {error, -?ERROR_FILE_NOT_FOUND}, S}
	end.


lookup(BinName, #state{vnodes=VNodes} = S) ->
	case walk(BinName, S) of
		{ok, _, _, Ino} ->
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
			do_walk(Name, 0, 0, "\\", State)
	end.


do_walk("", Ino, Parent, Name, _S) ->
	{ok, Parent, Name, Ino};

do_walk("\\", Ino, Parent, Name, _S) ->
	{ok, Parent, Name, Ino};

do_walk([$\\ | Path], DirIno, _, _, #state{vnodes=VNodes} = S) ->
	{Name, Rest} = lists:splitwith(fun(C) -> C =/= $\\ end, Path),
	case gb_trees:get(DirIno, VNodes) of
		#dir{listing=Dir} ->
			case orddict:find(Name, Dir) of
				{ok, Ino} ->
					do_walk(Rest, Ino, DirIno, Path, S);
				error ->
					{stop, DirIno, Path}
			end;

		#file{} ->
			error %% can't descend from a file
	end;

do_walk(_, _, _, _, _) ->
	error.

