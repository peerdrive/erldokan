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
-ifndef(ERLDOKAN_HRL).
-define(ERLDOKAN_HRL, true).

-define(FILE_ATTRIBUTE_ARCHIVE, 16#20).
-define(FILE_ATTRIBUTE_COMPRESSED, 16#800).
-define(FILE_ATTRIBUTE_DEVICE, 16#40).
-define(FILE_ATTRIBUTE_DIRECTORY, 16#10).
-define(FILE_ATTRIBUTE_ENCRYPTED, 16#4000).
-define(FILE_ATTRIBUTE_HIDDEN, 16#2).
-define(FILE_ATTRIBUTE_NORMAL, 16#80).
-define(FILE_ATTRIBUTE_NOT_CONTENT_INDEXED, 16#2000).
-define(FILE_ATTRIBUTE_OFFLINE, 16#1000).
-define(FILE_ATTRIBUTE_READONLY, 16#1).
-define(FILE_ATTRIBUTE_REPARSE_POINT, 16#400).
-define(FILE_ATTRIBUTE_SPARSE_FILE, 16#200).
-define(FILE_ATTRIBUTE_SYSTEM, 16#4).
-define(FILE_ATTRIBUTE_TEMPORARY, 16#100).

-record(dokan_file_info, {
	context,
	process_id,
	is_directory,
	delete_on_close,
	paging_io,
	synchronous_io,
	nocache,
	write_to_eof
}).

%%
%% CreateFile constants
%%

% dwCreationDisposition
-define(CREATE_NEW,        1).
-define(CREATE_ALWAYS,     2).
-define(OPEN_EXISTING,     3).
-define(OPEN_ALWAYS,       4).
-define(TRUNCATE_EXISTING, 5).

% dwDesiredAccess
-define(MAXIMUM_ALLOWED,  16#2000000).
-define(GENERIC_READ,     16#80000000).
-define(GENERIC_WRITE,    16#40000000).
-define(GENERIC_EXECUTE,  16#20000000).
-define(GENERIC_ALL,      16#10000000).

-define(DELETE,                   16#00010000).
-define(READ_CONTROL,             16#00020000).
-define(WRITE_DAC,                16#00040000).
-define(WRITE_OWNER,              16#00080000).
-define(SYNCHRONIZE,              16#00100000).
-define(STANDARD_RIGHTS_REQUIRED, 16#000F0000).
-define(STANDARD_RIGHTS_READ,     ?READ_CONTROL).
-define(STANDARD_RIGHTS_WRITE,    ?READ_CONTROL).
-define(STANDARD_RIGHTS_EXECUTE,  ?READ_CONTROL).
-define(STANDARD_RIGHTS_ALL,      16#001F0000).
-define(SPECIFIC_RIGHTS_ALL,      16#0000FFFF).
-define(ACCESS_SYSTEM_SECURITY,   16#1000000).

-define(FILE_LIST_DIRECTORY,       16#00000001).
-define(FILE_READ_DATA,            16#00000001).
-define(FILE_ADD_FILE,             16#00000002).
-define(FILE_WRITE_DATA,           16#00000002).
-define(FILE_ADD_SUBDIRECTORY,     16#00000004).
-define(FILE_APPEND_DATA,          16#00000004).
-define(FILE_CREATE_PIPE_INSTANCE, 16#00000004).
-define(FILE_READ_EA,              16#00000008).
-define(FILE_READ_PROPERTIES,      16#00000008).
-define(FILE_WRITE_EA,             16#00000010).
-define(FILE_WRITE_PROPERTIES,     16#00000010).
-define(FILE_EXECUTE,              16#00000020).
-define(FILE_TRAVERSE,             16#00000020).
-define(FILE_DELETE_CHILD,         16#00000040).
-define(FILE_READ_ATTRIBUTES,      16#00000080).
-define(FILE_WRITE_ATTRIBUTES,     16#00000100).

% dwShareMode
-define(FILE_SHARE_READ,	16#00000001).
-define(FILE_SHARE_WRITE,	16#00000002).
-define(FILE_SHARE_DELETE,	16#00000004).

% dwFlagsAndAttributes
-define(FILE_FLAG_BACKUP_SEMANTICS,   16#02000000).
-define(FILE_FLAG_DELETE_ON_CLOSE,    16#04000000).
-define(FILE_FLAG_NO_BUFFERING,       16#20000000).
-define(FILE_FLAG_OPEN_NO_RECALL,     16#00100000).
-define(FILE_FLAG_OPEN_REPARSE_POINT, 16#00200000).
-define(FILE_FLAG_OVERLAPPED,         16#40000000).
-define(FILE_FLAG_POSIX_SEMANTICS,    16#01000000).
-define(FILE_FLAG_RANDOM_ACCESS,      16#10000000).
-define(FILE_FLAG_SEQUENTIAL_SCAN,    16#08000000).
-define(FILE_FLAG_WRITE_THROUGH,      16#80000000).


-record(dokan_reply_open, {context, is_directory}).

-record(dokan_reply_fi, {
	file_attributes,
	creation_time = 0,
	last_access_time = 0,
	last_write_time = 0,
	volume_serial_number,
	file_size,
	number_of_links,
	file_index
}).

-record(dokan_reply_find, {
	file_attributes,
	creation_time = 0,
	last_access_time = 0,
	last_write_time = 0,
	file_size,
	file_name
}).

-record(dokan_reply_volinfo, {
	volume_name,
	volume_serial_number,
	maximum_component_length,
	file_system_flags,
	file_system_name
}).

-endif.
