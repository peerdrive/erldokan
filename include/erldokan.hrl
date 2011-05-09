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
