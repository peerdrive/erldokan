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
         delete_directory/4, close_file/4, move_file/6, set_end_of_file/5,
		 set_file_attributes/5, set_file_time/7]).
-export([init/1, handle_info/2, terminate/2, code_change/3]).

-record(state, {vnodes, handles, re}).
-record(file, {data, attr=?FILE_ATTRIBUTE_NORMAL, ctime=0, atime=0, mtime=0, deleted=false}).
-record(dir, {listing, deleted=false}).
-record(handle, {ino, parent, name}).

init(_Args) ->
	{ok, Re} = re:compile(<<"\\\\"/utf8>>),
	State = #state{
		vnodes=gb_trees:from_orddict([
			{0, #dir{listing=[{<<"hello.txt"/utf8>>, 1}]}},
			{1, #file{data= <<"Hello World...">>, ctime=get_time(), mtime=get_time()}}]),
		handles=gb_trees:from_orddict([{0, undefined}]),
		re = Re
	},
	{ok, State}.

handle_info(Msg, State) ->
	io:format("hello: handle_info(~p)~n", [Msg]),
	{noreply, State}.

terminate(_Reason, _State) ->
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

create_file(S, _From, FileName, _AccMode, _ShMode, CrDisp, _Flags, _DFI) ->
	#state{vnodes=VNodes} = S,
	case walk(FileName, S) of
		{ok, Parent, Name, Ino} ->
			case gb_trees:get(Ino, VNodes) of
				#file{} when (CrDisp == ?OPEN_EXISTING) or
				             (CrDisp == ?OPEN_ALWAYS) ->
					{Ctx, S2} = handle_add(#handle{ino=Ino, parent=Parent, name=Name}, S),
					{reply, #dokan_reply_open{context=Ctx, is_directory=false, existed=true}, S2};

				#file{} = File when (CrDisp == ?CREATE_ALWAYS) or
				                    (CrDisp == ?TRUNCATE_EXISTING) ->
					S2 = S#state{vnodes=gb_trees:update(Ino, File#file{data= <<>>}, VNodes)},
					{Ctx, S3} = handle_add(#handle{ino=Ino, parent=Parent, name=Name}, S2),
					{reply, #dokan_reply_open{context=Ctx, is_directory=false, existed=true}, S3};

				#file{} when CrDisp == ?CREATE_NEW ->
					{reply, {error, ?ERROR_FILE_EXISTS}, S};

				#dir{} when CrDisp == ?OPEN_EXISTING ->
					{Ctx, S2} = handle_add(#handle{ino=Ino, parent=Parent, name=Name}, S),
					{reply, #dokan_reply_open{context=Ctx, is_directory=true, existed=true}, S2};

				#dir{} ->
					{reply, {error, ?ERROR_ACCESS_DENIED}, S}
			end;

		{stop, DirIno, Name} when (CrDisp == ?CREATE_ALWAYS) or
		                          (CrDisp == ?CREATE_NEW) or
		                          (CrDisp == ?OPEN_ALWAYS) ->
			{Max, _} = gb_trees:largest(VNodes), Ino = Max+1,
			File = #file{data= <<>>, ctime=get_time(), mtime=get_time()},
			VN2 = gb_trees:enter(Ino, File, VNodes),
			VN3 = add_dir_entry(DirIno, Name, Ino, VN2),
			Handle = #handle{ino=Ino, parent=DirIno, name=Name},
			{Ctx, S2} = handle_add(Handle, S#state{vnodes=VN3}),
			{reply, #dokan_reply_open{context=Ctx, is_directory=false, existed=false}, S2};

		{stop, _, _} ->
			{reply, {error, ?ERROR_FILE_NOT_FOUND}, S};

		error ->
			{reply, {error, ?ERROR_FILE_NOT_FOUND}, S}
	end.


open_directory(S, _From, FileName, _DFI) ->
	case walk(FileName, S) of
		{ok, Parent, Name, Ino} ->
			#state{vnodes=VNodes} = S,
			case gb_trees:get(Ino, VNodes) of
				#dir{} ->
					{Ctx, S2} = handle_add(#handle{ino=Ino, parent=Parent, name=Name}, S),
					{reply, #dokan_reply_open{context=Ctx, is_directory=true, existed=true}, S2};
				#file{} ->
					{reply, {error, ?ERROR_ACCESS_DENIED}, S}
			end;
		_ ->
			{reply, {error, ?ERROR_PATH_NOT_FOUND}, S}
	end.


create_directory(#state{vnodes=VNodes} = S, _From, FileName, _DFI) ->
	case lookup(FileName, S) of
		#dir{} ->
			{reply, {error, ?ERROR_ALREADY_EXISTS}, S};
		#file{} ->
			{reply, {error, ?ERROR_ALREADY_EXISTS}, S};
		{stop, DirIno, Name} ->
			{Max, _} = gb_trees:largest(VNodes), Ino = Max+1,
			VN2 = gb_trees:enter(Ino, #dir{listing=[]}, VNodes),
			VN3 = add_dir_entry(DirIno, Name, Ino, VN2),
			Handle = #handle{ino=Ino, parent=DirIno, name=Name},
			{Ctx, S2} = handle_add(Handle, S#state{vnodes=VN3}),
			{reply, #dokan_reply_open{context=Ctx, is_directory=true, existed=false}, S2};

		error ->
			{reply, {error, ?ERROR_PATH_NOT_FOUND}, S}
	end.


close_file(#state{vnodes=VNodes, handles=Handles} = S, _From, FileName, DFI) ->
	Ctx = DFI#dokan_file_info.context,
	S2 = if
		Ctx =/= 0 -> S#state{handles=gb_trees:delete(Ctx, Handles)};
		true -> S
	end,
	case DFI#dokan_file_info.delete_on_close of
		true ->
			{ok, Parent, Name, Ino} = walk(FileName, S2),
			NewVNodes = gb_trees:delete(Ino, del_dir_entry(Parent, Name, VNodes)),
			{reply, ok, S2#state{vnodes=NewVNodes}};

		false ->
			{reply, ok, S2}
	end.


find_files(#state{vnodes=VNodes} = S, _From, _Path, DFI) ->
	#dir{listing=Dir} = get_vnode(S, DFI),
	DirEntries = lists:map(
		fun({Name, Ino}) ->
			{Name, gb_trees:get(Ino, VNodes)}
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
			({Name, #file{} = File}, Acc) ->
				#file{data=Data, attr=Attr, ctime=CTime, atime=ATime,
					mtime=MTime} = File,
				[#dokan_reply_find{
					creation_time = CTime,
					last_access_time = ATime,
					last_write_time = MTime,
					file_attributes = Attr,
					file_size = size(Data),
					file_name = Name
				} | Acc]
		end,
		[],
		DirEntries),
	FullList = [
		#dokan_reply_find{
			file_attributes = ?FILE_ATTRIBUTE_DIRECTORY,
			file_size = 0,
			file_name = <<".">>
		},
		#dokan_reply_find{
			file_attributes = ?FILE_ATTRIBUTE_DIRECTORY,
			file_size = 0,
			file_name = <<"..">>
		}
		| List],
	{reply, FullList, S}.


get_file_information(S, _From, _FileName, DFI) ->
	case get_vnode(S, DFI) of
		#file{data=Data, attr=Attr, ctime=CTime, atime=ATime, mtime=MTime} ->
			Info = #dokan_reply_fi{
				creation_time = CTime,
				last_access_time = ATime,
				last_write_time = MTime,
				file_attributes = Attr,
				file_size = size(Data)
			},
			{reply, Info, S};

		#dir{} ->
			Info = #dokan_reply_fi{
				file_attributes = ?FILE_ATTRIBUTE_DIRECTORY
			},
			{reply, Info, S}
	end.


read_file(S, _From, _FileName, Length, Offset, DFI) ->
	case get_vnode(S, DFI) of
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
			{reply, {error, ?ERROR_ACCESS_DENIED}, S}
	end.


write_file(#state{vnodes=VNodes} = S, _From, _FileName, Data, Offset, DFI) ->
	Ino = get_ino(S, DFI),
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

		#dir{} ->
			{reply, {error, ?ERROR_ACCESS_DENIED}, S}
	end.


delete_file(#state{vnodes=VNodes} = S, _From, _FileName, DFI) ->
	Ino = get_ino(S, DFI),
	case gb_trees:get(Ino, VNodes) of
		#file{} = File ->
			NewFile = File#file{deleted=true},
			{reply, ok, S#state{vnodes=gb_trees:update(Ino, NewFile, VNodes)}};
		#dir{} ->
			{reply, {error, ?ERROR_ACCESS_DENIED}, S}
	end.


delete_directory(#state{vnodes=VNodes} = S, _From, _FileName, DFI) ->
	Ino = get_ino(S, DFI),
	case gb_trees:get(Ino, VNodes) of
		#dir{listing=Listing} = Dir when Listing == [] ->
			NewDir = Dir#dir{deleted=true},
			{reply, ok, S#state{vnodes=gb_trees:update(Ino, NewDir, VNodes)}};
		#dir{} ->
			{reply, {error, ?ERROR_DIR_NOT_EMPTY}, S};
		#file{} ->
			{reply, {error, ?ERROR_ACCESS_DENIED}, S}
	end.


% TODO: check for open handles
move_file(S, _From, _OldName, NewName, Replace, DFI) ->
	#state{vnodes=VNodes, handles=Handles} = S,
	#handle{
		ino    = Ino,
		parent = OldParent,
		name   = OldPName
	} = gb_trees:get(DFI#dokan_file_info.context, Handles),
	case walk(NewName, S) of
		{ok, NewParent, NewPName, ExistIno} ->
			OldType = element(1, gb_trees:get(Ino, VNodes)),
			ExistType = element(1, gb_trees:get(ExistIno, VNodes)),
			if
				OldType =/= ExistType -> throw({reply, {error, ?ERROR_ACCESS_DENIED}, S});
				ExistType =/= file -> throw({reply, {error, ?ERROR_ACCESS_DENIED}, S});
				not Replace -> throw({reply, {error, ?ERROR_ACCESS_DENIED}, S});
				true -> ok
			end,
			VN1 = del_dir_entry(OldParent, OldPName, VNodes),
			VN2 = del_dir_entry(NewParent, NewPName, VN1),
			VN3 = gb_trees:delete(ExistIno, VN2),
			VN4 = add_dir_entry(NewParent, NewPName, Ino, VN3),
			{reply, ok, S#state{vnodes=VN4}};

		{stop, NewParent, NewPName} ->
				VN1 = del_dir_entry(OldParent, OldPName, VNodes),
			VN2 = add_dir_entry(NewParent, NewPName, Ino, VN1),
			{reply, ok, S#state{vnodes=VN2}};

		error ->
			{reply, {error, ?ERROR_PATH_NOT_FOUND}, S}
	end.


set_end_of_file(#state{vnodes=VNodes}=S, _From, _FileName, Offset, DFI) ->
	Ino = get_ino(S, DFI),
	case gb_trees:get(Ino, VNodes) of
		#file{data=Data} = File ->
			Size = size(Data),
			NewFile = if
				Offset < Size ->
					File#file{data=binary:part(Data, 0, Offset)};
				Offset > Size ->
					File#file{data= <<Data/binary, 0:((Offset-Size)*8)>>};
				true ->
					File
			end,
			{reply, ok, S#state{vnodes=gb_trees:update(Ino, NewFile, VNodes)}};

		#dir{} ->
			{reply, {error, ?ERROR_ACCESS_DENIED}, S}
	end.


set_file_attributes(#state{vnodes=VNodes}=S, _From, _FileName, Attr, DFI) ->
	Ino = get_ino(S, DFI),
	case gb_trees:get(Ino, VNodes) of
		#file{} = File ->
			NewFile = File#file{attr=Attr},
			{reply, ok, S#state{vnodes=gb_trees:update(Ino, NewFile, VNodes)}};

		#dir{} ->
			{reply, {error, ?ERROR_ACCESS_DENIED}, S}
	end.


set_file_time(#state{vnodes=VNodes}=S, _From, _FileName, CTime, ATime, MTime, DFI) ->
	Ino = get_ino(S, DFI),
	case gb_trees:get(Ino, VNodes) of
		#file{} = File ->
			File1 = if
				CTime =/= 0 -> File#file{ctime=CTime};
				true -> File
			end,
			File2 = if
				ATime =/= 0 -> File1#file{atime=ATime};
				true -> File1
			end,
			File3 = if
				MTime =/= 0 -> File2#file{mtime=MTime};
				true -> File2
			end,
			{reply, ok, S#state{vnodes=gb_trees:update(Ino, File3, VNodes)}};

		#dir{} ->
			{reply, {error, ?ERROR_ACCESS_DENIED}, S}
	end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


handle_add(Handle, #state{handles=Handles} = S) ->
	{Max, _} = gb_trees:largest(Handles),
	Ctx = Max+1,
	NewHandles = gb_trees:enter(Ctx, Handle, Handles),
	{Ctx, S#state{handles=NewHandles}}.


add_dir_entry(DirIno, Name, Ino, VNodes) ->
	Dir = #dir{listing=List} = gb_trees:get(DirIno, VNodes),
	NewList = orddict:store(Name, Ino, List),
	gb_trees:update(DirIno, Dir#dir{listing=NewList}, VNodes).


del_dir_entry(DirIno, Name, VNodes) ->
	Dir = #dir{listing=Listing} = gb_trees:get(DirIno, VNodes),
	NewListing = orddict:erase(Name, Listing),
	gb_trees:update(DirIno, Dir#dir{listing=NewListing}, VNodes).


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


get_ino(#state{handles=Handles}, DFI) ->
	#handle{ino=Ino} = gb_trees:get(DFI#dokan_file_info.context, Handles),
	Ino.


get_vnode(#state{vnodes=VNodes} = S, DFI) ->
	Ino = get_ino(S, DFI),
	gb_trees:get(Ino, VNodes).


lookup(Name, #state{vnodes=VNodes} = S) ->
	case walk(Name, S) of
		{ok, _, _, Ino} ->
			gb_trees:get(Ino, VNodes);
		Error ->
			Error
	end.


walk(Name, #state{re=Re} = State) ->
	if
		Name == <<"\\"/utf8>> ->
			{ok, 0, <<"\\"/utf8>>, 0};
		true ->
			% drop first empty string (Name always begins with a \)
			[_ | SplitName] = re:split(Name, Re),
			do_walk(SplitName, 0, 0, <<"\\"/utf8>>, State)
	end.


do_walk([], Ino, Parent, Name, _S) ->
	{ok, Parent, Name, Ino};

do_walk([Name | Rest], DirIno, _ParentIno, _DirName, #state{vnodes=VNodes} = S) ->
	case gb_trees:get(DirIno, VNodes) of
		#dir{listing=Dir} ->
			case orddict:find(Name, Dir) of
				{ok, Ino} ->
					do_walk(Rest, Ino, DirIno, Name, S);

				error ->
					case Rest of
						[] ->
							{stop, DirIno, Name};
						_ ->
							error
					end
			end;

		#file{} ->
			error %% can't descend from a file
	end.


get_time() ->
	(calendar:datetime_to_gregorian_seconds(calendar:universal_time()) -
	(146097*4+366)*24*3600) * 10000000.

