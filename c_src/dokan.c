/*
 * This file is part of ErlDokan.
 * Copyright (C) 2011  Jan Klötzke <jan DOT kloetzke AT freenet DOT de>
 *
 * ErlDokan is free software: you can redistribute it and/or modify it under
 * the terms of the GNU Lesser General Public License as published by the Free
 * Software Foundation, either version 3 of the License, or (at your option)
 * any later version.
 *
 * ErlDokan is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with ErlDokan. If not, see <http://www.gnu.org/licenses/>.
 */

//#define NDEBUG

#include <windows.h>
#include <winbase.h>
#include <assert.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>

#include <erl_driver.h>
#include <ei.h>
#include <dokan.h>

#define DEFAULT_MOUNT_POINT L"M:\\"
#define IND_TERM_SIZE 48
#define IND_IOV_SIZE 4
#define IND_STRINGS_SIZE 2

#define ARRAY_SIZE(a) (sizeof(a) / sizeof((a)[0]))
#define CONTAINER_OF(_ptr, _type, _member) \
	((_type *)((char *)(_ptr) - offsetof(_type, _member)))

struct list {
	struct list *next;
	struct list *prev;
};

struct indication {
	struct list node;
	uint32_t id;

	ErlDrvTermData ind_data[IND_TERM_SIZE];
	int ind_pos, ind_args;
	char *strings[IND_STRINGS_SIZE];

	HANDLE event;
	int status;

	ErlIOVec      resp;
	ErlDrvBinary *resp_bin[IND_IOV_SIZE];
	SysIOVec      resp_iov[IND_IOV_SIZE];
};

struct ind_queue {
	CRITICAL_SECTION lock;
	struct list head;
};

struct self {
	ErlDrvPort port;

	int mounted;
	int abort;
	int unmounted;

	/*
	 * Indication queues. The order here is also the locking order if multiple
	 * queues must be locked simultaneously.
	 */
	struct ind_queue indSendQ;    /* Send to emulator */
	struct ind_queue indRespQ;    /* Wait for response from emulator */
	struct ind_queue indFreeQ;    /* Reusable indications */
	struct ind_queue indLaundryQ; /* Dirty indications */

	LONG nextId;

	HANDLE outputEvent;
	HANDLE laundryEvent;

	/* Dokan structures */
	DOKAN_OPERATIONS dokanOps;
	DOKAN_OPTIONS    dokanArgs;
	ErlDrvTid        dokanThread;
	int              dokanResult;
};

struct parse_state {
	struct self *drv;
	const char *buf;
	int index;
	int size;
};


enum {
	ATOM_OK = 0,
	ATOM_ERROR,
	ATOM_TRUE,
	ATOM_FALSE,
	ATOM_CLEANUP,
	ATOM_CLOSE_FILE,
	ATOM_CREATE_DIRECTORY,
	ATOM_CREATE_FILE,
	ATOM_DELETE_DIRECTORY,
	ATOM_DELETE_FILE,
	ATOM_FIND_FILES,
	ATOM_FIND_FILES_WITH_PATTERN,
	ATOM_FLUSH_FILE_BUFFERS,
	ATOM_GET_DISK_FREE_SPACE,
	ATOM_GET_FILE_INFORMATION,
	ATOM_GET_VOLUME_INFORMATION,
	ATOM_LOCK_FILE,
	ATOM_MOVE_FILE,
	ATOM_OPEN_DIRECTORY,
	ATOM_READ_FILE,
	ATOM_SET_ALLOCATION_SIZE,
	ATOM_SET_END_OF_FILE,
	ATOM_SET_FILE_ATTRIBUTES,
	ATOM_SET_FILE_TIME,
	ATOM_UNLOCK_FILE,
	ATOM_UNMOUNT,
	ATOM_WRITE_FILE,
	ATOM_DOKAN_FILE_INFO,
	_ATOM_COUNT
};

static char *atom_templates[_ATOM_COUNT] = {
	"ok",
	"error",
	"true",
	"false",
	"cleanup",
	"close_file",
	"create_directory",
	"create_file",
	"delete_directory",
	"delete_file",
	"find_files",
	"find_files_with_pattern",
	"flush_file_buffers",
	"get_disk_free_space",
	"get_file_information",
	"get_volume_information",
	"lock_file",
	"move_file",
	"open_directory",
	"read_file",
	"set_allocation_size",
	"set_end_of_file",
	"set_file_attributes",
	"set_file_time",
	"unlock_file",
	"unmount",
	"write_file",
	"dokan_file_info",
};

static ErlDrvTermData atom_table[_ATOM_COUNT];

static struct self *FromFileInfo(PDOKAN_FILE_INFO fileInfo)
{
	return (struct self *)(unsigned long)fileInfo->DokanOptions->GlobalContext;
}

static void IOVecCopy(ErlIOVec *dst, ErlIOVec *src)
{
	int i;

	dst->size = src->size;
	dst->vsize = src->vsize;
	for (i=1; i<src->vsize; i++) {
		dst->iov[i] = src->iov[i];
		dst->binv[i] = src->binv[i];
		driver_binary_inc_refc(dst->binv[i]);
	}
}

static void IOVecFree(ErlIOVec *ev)
{
	int i;

	for (i=1; i<ev->vsize; i++)
		driver_binary_dec_refc(ev->binv[i]);

	ev->vsize = 0;
	ev->size = 0;
}

static int IOVecGetOffset(ErlIOVec *ev, size_t off, char **base, size_t *size)
{
	int i = 1;

	if (off >= ev->size)
		return -1;

	while (i < ev->vsize) {
		if (off < ev->iov[i].iov_len) {
			*base = ev->iov[i].iov_base + off;
			*size = ev->iov[i].iov_len - off;
			return 0;
		} else {
			off -= ev->iov[i].iov_len;
			i++;
		}
	}

	return -1;
}

static void QueueInit(struct ind_queue *q)
{
	q->head.next = &q->head;
	q->head.prev = &q->head;
	InitializeCriticalSection(&q->lock);
}

static void QueueDestroy(struct ind_queue *q)
{
	DeleteCriticalSection(&q->lock);
}

static void QueueLock(struct ind_queue *q)
{
	EnterCriticalSection(&q->lock);
}

static void QueueUnlock(struct ind_queue *q)
{
	LeaveCriticalSection(&q->lock);
}

static void QueueRemoveLocked(struct indication *ind)
{
	ind->node.prev->next = ind->node.next;
	ind->node.next->prev = ind->node.prev;
}

static struct indication *QueueGetLocked(struct ind_queue *q)
{
	struct indication *ind;

	if (q->head.next == &q->head)
		return NULL;

	ind = CONTAINER_OF(q->head.next, struct indication, node);
	QueueRemoveLocked(ind);
	return ind;
}

static struct indication *QueueGet(struct ind_queue *q)
{
	struct indication *ind;

	QueueLock(q);
	ind = QueueGetLocked(q);
	QueueUnlock(q);

	return ind;
}

static void QueuePutLocked(struct ind_queue *q, struct indication *ind)
{
	q->head.prev->next = &ind->node;
	ind->node.next = &q->head;
	ind->node.prev = q->head.prev;
	q->head.prev = &ind->node;
}

static void QueuePut(struct ind_queue *q, struct indication *ind)
{
	QueueLock(q);
	QueuePutLocked(q, ind);
	QueueUnlock(q);
}

#define IND_ADD_ARG0(_ind, _type) \
	do { \
		int _pos = (_ind)->ind_pos; \
		assert(_pos+1 < IND_TERM_SIZE); \
		(_ind)->ind_data[_pos] = (_type); \
		(_ind)->ind_pos += 1; \
	} while (0)

#define IND_ADD_ARG1(_ind, _type, _arg0) \
	do { \
		int _pos = (_ind)->ind_pos; \
		assert(_pos+2 < IND_TERM_SIZE); \
		(_ind)->ind_data[_pos+0] = (_type); \
		(_ind)->ind_data[_pos+1] = (ErlDrvTermData)(_arg0); \
		(_ind)->ind_pos += 2; \
	} while (0)

#define IND_ADD_BOOL(_ind, _arg) \
	do { \
		IND_ADD_ARG1((_ind), ERL_DRV_ATOM, (_arg) \
			? atom_table[ATOM_TRUE] \
			: atom_table[ATOM_FALSE]); \
	} while (0)

#define IND_ADD_ARG2(_ind, _type, _arg0, _arg1) \
	do { \
		int _pos = (_ind)->ind_pos; \
		assert(_pos+3 < IND_TERM_SIZE); \
		(_ind)->ind_data[_pos+0] = (_type); \
		(_ind)->ind_data[_pos+1] = (ErlDrvTermData)(_arg0); \
		(_ind)->ind_data[_pos+2] = (ErlDrvTermData)(_arg1); \
		(_ind)->ind_pos += 3; \
	} while (0)

static void IndAddString(struct indication *ind, LPCWSTR str)
{
	int len, i;

	i = 0;
	while (ind->strings[i] && i < IND_STRINGS_SIZE) i++;
	assert(i < IND_STRINGS_SIZE);

	/* Get size requirement */
	len = WideCharToMultiByte(CP_UTF8, /* CodePage */
	                          0,       /* dwFlags */
	                          str,     /* lpWideCharStr */
	                          -1,      /* cchWideChar */
	                          NULL,    /* lpMultiByteStr */
	                          0,       /* cbMultiByte */
	                          NULL,    /* lpDefaultChar */
	                          NULL);   /* lpUsedDefaultChar */
	if (len <= 0)
		return;

	ind->strings[i] = driver_alloc(len);
	if (!ind->strings[i])
		return;

	/* Convert to UTF-8 */
	len = WideCharToMultiByte(CP_UTF8,         /* CodePage */
	                          0,               /* dwFlags */
	                          str,             /* lpWideCharStr */
	                          -1,              /* cchWideChar */
	                          ind->strings[i], /* lpMultiByteStr */
	                          len,             /* cbMultiByte */
	                          NULL,            /* lpDefaultChar */
	                          NULL);           /* lpUsedDefaultChar */
	if (len <= 0)
		return;

	IND_ADD_ARG2(ind, ERL_DRV_BUF2BINARY, (ErlDrvTermData)ind->strings[i],
		len-1);
	ind->ind_args++;
}

static void IndAddUInt(struct indication *ind, ErlDrvUInt data)
{
	IND_ADD_ARG1(ind, ERL_DRV_UINT, data);
	ind->ind_args++;
}

static void IndAddInt64(struct indication *ind, LONGLONG *num)
{
	IND_ADD_ARG1(ind, ERL_DRV_INT64, num);
	ind->ind_args++;
}

static void IndAddBuffer(struct indication *ind, const void *buffer,
                         unsigned long size)
{
	IND_ADD_ARG2(ind, ERL_DRV_BUF2BINARY, (ErlDrvTermData)buffer, size);
	ind->ind_args++;
}

static void IndAddBool(struct indication *ind, BOOL data)
{
	IND_ADD_BOOL(ind, data);
	ind->ind_args++;
}

static void IndAddFileInfo(struct indication *ind, PDOKAN_FILE_INFO fileInfo)
{
	IND_ADD_ARG1(ind, ERL_DRV_ATOM, atom_table[ATOM_DOKAN_FILE_INFO]);
	IND_ADD_ARG1(ind, ERL_DRV_UINT64, &fileInfo->Context);
	IND_ADD_ARG1(ind, ERL_DRV_UINT, fileInfo->ProcessId);
	IND_ADD_BOOL(ind, fileInfo->IsDirectory);
	IND_ADD_BOOL(ind, fileInfo->DeleteOnClose);
	IND_ADD_BOOL(ind, fileInfo->PagingIo);
	IND_ADD_BOOL(ind, fileInfo->SynchronousIo);
	IND_ADD_BOOL(ind, fileInfo->Nocache);
	IND_ADD_BOOL(ind, fileInfo->WriteToEndOfFile);
	IND_ADD_ARG1(ind, ERL_DRV_TUPLE, 9);
	ind->ind_args++;
}

static void IndAddDone(struct indication *ind)
{
	IND_ADD_ARG0(ind, ERL_DRV_NIL);
	IND_ADD_ARG1(ind, ERL_DRV_LIST, ind->ind_args + 1);
	IND_ADD_ARG1(ind, ERL_DRV_TUPLE, 4);
}

static struct indication *AllocIndication(struct self *self, int tag)
{
	struct indication *ind;

	ind = QueueGet(&self->indFreeQ);
	if (!ind) {
		ind = driver_alloc(sizeof(*ind));
		if (!ind)
			return NULL;
		memset(ind, 0, sizeof(*ind));

		ind->resp.iov = ind->resp_iov;
		ind->resp.binv = ind->resp_bin;

		ind->event = CreateEvent(NULL, FALSE, FALSE, NULL);
		if (!ind->event)
			goto err_event;
	}

	ind->id = InterlockedIncrement(&self->nextId);
	ind->ind_data[0] = ERL_DRV_PORT;
	ind->ind_data[1] = driver_mk_port(self->port);
	ind->ind_data[2] = ERL_DRV_UINT;
	ind->ind_data[3] = ind->id;
	ind->ind_data[4] = ERL_DRV_ATOM;
	ind->ind_data[5] = atom_table[tag];
	ind->ind_pos = 6;
	ind->ind_args = 0;

	return ind;

err_event:
	driver_free(ind);
	return NULL;
}

static void FreeIndication(struct self *self, struct indication *ind)
{
	/*
	 * We cannot free any referenced binaries directly because
	 * driver_binary_dec_refc() is not thread safe. Queue it instead and let us
	 * call back.
	 */
	QueuePut(&self->indLaundryQ, ind);
	SetEvent(self->laundryEvent);
}

static void LaunderIndication(struct indication *ind)
{
	int i;

	IOVecFree(&ind->resp);

	for (i=0; i<IND_STRINGS_SIZE; i++) {
		if (ind->strings[i]) {
			driver_free(ind->strings[i]);
			ind->strings[i] = NULL;
		}
	}
}

static void DisposeIndication(struct indication *ind)
{
	LaunderIndication(ind);
	CloseHandle(ind->event);
	driver_free(ind);
}

static int SendIndication(struct self *self, struct indication *ind)
{
	ind->status = 1;

	QueueLock(&self->indSendQ);
	if (self->mounted)
		QueuePut(&self->indSendQ, ind);
	else
		ind->status = -ERROR_DEV_NOT_EXIST;
	QueueUnlock(&self->indSendQ);
	SetEvent(self->outputEvent);

	while (ind->status > 0) {
		if (WaitForSingleObject(ind->event, INFINITE) != WAIT_OBJECT_0)
			return -GetLastError();
	}

	return ind->status;
}

static int RewindParser(struct parse_state *ps)
{
	int ver;

	ps->index = 0;

	if (ei_decode_version(ps->buf, &ps->index, &ver))
		return -ERROR_GEN_FAILURE;

	return 0;
}

static int InitParser(struct self *self, struct parse_state *ps,
                       struct indication *ind)
{
	char *base;
	size_t size;

	if (IOVecGetOffset(&ind->resp, 8, &base, &size))
		return -ERROR_GEN_FAILURE;

	ps->drv  = self;
	ps->buf  = base;
	ps->size = size;

	return RewindParser(ps);
}

static void Abort(struct self *self)
{
	self->abort = 1;
	SetEvent(self->outputEvent);
}

#define CHECK(x) \
	do { \
		if ((x)) \
			goto badarg; \
	} while (0)

/*
 * ok | {error, ErrNo}
 */
static int ParseGenericResponse(struct parse_state *ps)
{
	int type, size;
	long ret;
	char tag[6];

	CHECK(ei_get_type(ps->buf, &ps->index, &type, &size));
	switch (type) {
		case ERL_ATOM_EXT:
			CHECK(size != 2);
			CHECK(ei_decode_atom(ps->buf, &ps->index, tag));
			CHECK(strcmp(tag, "ok"));
			return 0;

		case ERL_SMALL_TUPLE_EXT:
			CHECK(ei_decode_tuple_header(ps->buf, &ps->index, &size));
			CHECK(size != 2);
			CHECK(ei_get_type(ps->buf, &ps->index, &type, &size));
			CHECK(type != ERL_ATOM_EXT || size != sizeof("error")-1);
			CHECK(ei_decode_atom(ps->buf, &ps->index, tag));
			CHECK(strcmp(tag, "error"));
			CHECK(ei_decode_long(ps->buf, &ps->index, &ret));
			CHECK(ret >= 0);
			return ret;

		default:
			Abort(ps->drv);
	}

	return -ERROR_GEN_FAILURE;

badarg:
	Abort(ps->drv);
	return -ERROR_GEN_FAILURE;
}

static int ParseUndefined(struct parse_state *ps)
{
	char tag[sizeof("undefined")];
	int type, size;

	if (ei_get_type(ps->buf, &ps->index, &type, &size))
		return -1;
	if (type !=  ERL_ATOM_EXT)
		return -1;
	if (size != sizeof("undefined")-1)
		return -1;
	if (ei_decode_atom(ps->buf, &ps->index, tag))
		return -1;
	if (strcmp(tag, "undefined"))
		return -1;

	return 0;
}

#define REPLY_OPEN_TAG "dokan_reply_open"

/*
 * #dokan_reply_open{} | {error, ErrNo}
 */
static int ParseOpenResponse(struct parse_state *ps, ULONG64 *ctx, UCHAR *isDir)
{
	int type, size, ret;
	char tag[sizeof(REPLY_OPEN_TAG)];

	if (ei_decode_tuple_header(ps->buf, &ps->index, &size))
		goto fail;
	if (size != 3)
		goto fail;
	CHECK(ei_get_type(ps->buf, &ps->index, &type, &size));
	if (type != ERL_ATOM_EXT || size != sizeof(REPLY_OPEN_TAG)-1)
		goto fail;
	CHECK(ei_decode_atom(ps->buf, &ps->index, tag));
	if (strcmp(tag, REPLY_OPEN_TAG))
		goto fail;
	if (ei_decode_ulonglong(ps->buf, &ps->index, ctx))
		CHECK(ParseUndefined(ps));
	if (ei_decode_boolean(ps->buf, &ps->index, &ret))
		CHECK(ParseUndefined(ps));
	*isDir = ret;

	return 0;

fail:
	RewindParser(ps);
	ret = ParseGenericResponse(ps);
	CHECK(ret >= 0);
	return ret;

badarg:
	Abort(ps->drv);
	return -ERROR_GEN_FAILURE;
}

static int __stdcall
ReqCreateFile(LPCWSTR fileName, DWORD accessMode, DWORD shareMode,
              DWORD creationDisposition, DWORD flags, PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_CREATE_FILE);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddUInt(ind, accessMode);
	IndAddUInt(ind, shareMode);
	IndAddUInt(ind, creationDisposition);
	IndAddUInt(ind, flags);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseOpenResponse(&ps, &fileInfo->Context, &fileInfo->IsDirectory);

out:
	FreeIndication(self, ind);
	return ret;
}

static int __stdcall
ReqOpenDirectory(LPCWSTR fileName, PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_OPEN_DIRECTORY);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseOpenResponse(&ps, &fileInfo->Context, &fileInfo->IsDirectory);

out:
	FreeIndication(self, ind);
	return ret;
}

static int __stdcall
ReqCreateDirectory(LPCWSTR fileName, PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_CREATE_DIRECTORY);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseOpenResponse(&ps, &fileInfo->Context, &fileInfo->IsDirectory);

out:
	FreeIndication(self, ind);
	return ret;
}

#define REPLY_FIND_TAG "dokan_reply_find"

/*
 * [#dokan_reply_find{}] | {error, ErrNo}
 */
static int ParseFFResponse(struct parse_state *ps, PFillFindData fillFindData,
                           PDOKAN_FILE_INFO fileInfo)
{
	int i, type, size, length, ret, buf_len;
	char *buf;
	char tag[sizeof(REPLY_FIND_TAG)];

	buf_len = 64;
	buf = driver_alloc(buf_len);
	if (!buf)
		return -ERROR_OUTOFMEMORY;

	if (ei_decode_list_header(ps->buf, &ps->index, &length))
		goto fail;

	for (i=0; i<length; i++) {
		WIN32_FIND_DATAW findData;
		unsigned long long tmp;
		long dummy;

		CHECK(ei_decode_tuple_header(ps->buf, &ps->index, &size));
		CHECK(size != 7);

		CHECK(ei_get_type(ps->buf, &ps->index, &type, &size));
		CHECK(type != ERL_ATOM_EXT || size != sizeof(REPLY_FIND_TAG)-1);
		CHECK(ei_decode_atom(ps->buf, &ps->index, tag));
		CHECK(strcmp(tag, REPLY_FIND_TAG));

		memset(&findData, 0, sizeof(findData));

		CHECK(ei_decode_ulong(ps->buf, &ps->index, &findData.dwFileAttributes));

		CHECK(ei_decode_ulonglong(ps->buf, &ps->index, &tmp));
		findData.ftCreationTime.dwLowDateTime = tmp;
		findData.ftCreationTime.dwHighDateTime = tmp >> 32;
		CHECK(ei_decode_ulonglong(ps->buf, &ps->index, &tmp));
		findData.ftLastAccessTime.dwLowDateTime = tmp;
		findData.ftLastAccessTime.dwHighDateTime = tmp >> 32;
		CHECK(ei_decode_ulonglong(ps->buf, &ps->index, &tmp));
		findData.ftLastWriteTime.dwLowDateTime = tmp;
		findData.ftLastWriteTime.dwHighDateTime = tmp >> 32;
		CHECK(ei_decode_ulonglong(ps->buf, &ps->index, &tmp));
		findData.nFileSizeLow = tmp;
		findData.nFileSizeHigh = tmp >> 32;

		CHECK(ei_get_type(ps->buf, &ps->index, &type, &size));
		CHECK(type != ERL_BINARY_EXT);
		if (buf_len < size) {
			driver_free(buf);
			buf_len = size;
			buf = driver_alloc(buf_len);
			if (!buf)
				return -ERROR_OUTOFMEMORY;
		}
		CHECK(ei_decode_binary(ps->buf, &ps->index, buf, &dummy));
		size = MultiByteToWideChar(CP_UTF8, 0, buf, dummy, findData.cFileName,
			ARRAY_SIZE(findData.cFileName)-1);
		findData.cFileName[size] = 0;

		fillFindData(&findData, fileInfo);
	}

	/* Parse tail of list (but not if list was empty) */
	if (length > 0)
		CHECK(ei_decode_list_header(ps->buf, &ps->index, &length));

	driver_free(buf);
	return 0;

fail:
	RewindParser(ps);
	ret = ParseGenericResponse(ps);
	CHECK(ret >= 0);
	driver_free(buf);
	return ret;

badarg:
	Abort(ps->drv);
	driver_free(buf);
	return -ERROR_GEN_FAILURE;
}

static int __stdcall
ReqFindFiles(LPCWSTR fileName, PFillFindData fillFindData,
             PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_FIND_FILES);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseFFResponse(&ps, fillFindData, fileInfo);

out:
	FreeIndication(self, ind);
	return ret;
}

static int __stdcall
ReqFindFilesWithPattern(LPCWSTR fileName, LPCWSTR searchPattern,
                        PFillFindData fillFindData, PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_FIND_FILES_WITH_PATTERN);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddString(ind, searchPattern);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseFFResponse(&ps, fillFindData, fileInfo);

out:
	FreeIndication(self, ind);
	return ret;
}

#define REPLY_FILE_INFO "dokan_reply_fi"

/*
 * #dokan_reply_fi{} | {error, ErrNo}
 */
static int ParseFileInfoResponse(struct parse_state *ps,
                                 LPBY_HANDLE_FILE_INFORMATION fileInfo)
{
	char tag[sizeof(REPLY_FILE_INFO)];
	int size, ret, type;
	unsigned long long tmp;

	if (ei_decode_tuple_header(ps->buf, &ps->index, &size))
		goto fail;
	if (size != 9)
		goto fail;

	CHECK(ei_get_type(ps->buf, &ps->index, &type, &size));
	CHECK(type != ERL_ATOM_EXT || size != sizeof(REPLY_FILE_INFO)-1);
	CHECK(ei_decode_atom(ps->buf, &ps->index, tag));
	CHECK(strcmp(tag, REPLY_FILE_INFO));

	CHECK(ei_decode_ulong(ps->buf, &ps->index, &fileInfo->dwFileAttributes));

	CHECK(ei_decode_ulonglong(ps->buf, &ps->index, &tmp));
	fileInfo->ftCreationTime.dwLowDateTime = tmp;
	fileInfo->ftCreationTime.dwHighDateTime = tmp >> 32;
	CHECK(ei_decode_ulonglong(ps->buf, &ps->index, &tmp));
	fileInfo->ftLastAccessTime.dwLowDateTime = tmp;
	fileInfo->ftLastAccessTime.dwHighDateTime = tmp >> 32;
	CHECK(ei_decode_ulonglong(ps->buf, &ps->index, &tmp));
	fileInfo->ftLastWriteTime.dwLowDateTime = tmp;
	fileInfo->ftLastWriteTime.dwHighDateTime = tmp >> 32;

	CHECK(ei_decode_ulong(ps->buf, &ps->index, &fileInfo->dwVolumeSerialNumber));

	CHECK(ei_decode_ulonglong(ps->buf, &ps->index, &tmp));
	fileInfo->nFileSizeLow = tmp;
	fileInfo->nFileSizeHigh = tmp >> 32;

	CHECK(ei_decode_ulong(ps->buf, &ps->index, &fileInfo->nNumberOfLinks));

	CHECK(ei_decode_ulonglong(ps->buf, &ps->index, &tmp));
	fileInfo->nFileIndexLow = tmp;
	fileInfo->nFileIndexHigh = tmp >> 32;

	return 0;

fail:
	RewindParser(ps);
	ret = ParseGenericResponse(ps);
	CHECK(ret >= 0);
	return ret;

badarg:
	Abort(ps->drv);
	return -ERROR_GEN_FAILURE;
}

static int __stdcall
ReqGetFileInformation(LPCWSTR fileName,
                      LPBY_HANDLE_FILE_INFORMATION fileInformation,
                      PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_GET_FILE_INFORMATION);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseFileInfoResponse(&ps, fileInformation);

out:
	FreeIndication(self, ind);
	return ret;
}

static int __stdcall
ReqCleanup(LPCWSTR fileName, PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_CLEANUP);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseGenericResponse(&ps);

out:
	FreeIndication(self, ind);
	return ret;
}

static int __stdcall
ReqCloseFile(LPCWSTR fileName, PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_CLOSE_FILE);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseGenericResponse(&ps);

out:
	FreeIndication(self, ind);
	return ret;
}

static int ParseReadResponse(struct self *self, struct indication *ind,
                             void *buffer, DWORD bufferLength, LPDWORD readLength)
{
	struct parse_state ps;
	char *base;
	size_t size;
	int ret;

	/* First check status */
	if ((ret = InitParser(self, &ps, ind)))
		return ret;
	if ((ret = ParseGenericResponse(&ps)))
		return ret;

	/* Looks good. Get the data... */
	if (IOVecGetOffset(&ind->resp, 4, &base, &size))
		return -ERROR_GEN_FAILURE;
	if (IOVecGetOffset(&ind->resp, 8 + *((uint32_t *)base), &base, &size))
		return -ERROR_GEN_FAILURE;

	if (bufferLength < size)
		size = bufferLength; /* Or reject? */

	memcpy(buffer, base, size);
	*readLength = size;
	return 0;
}

static int __stdcall
ReqReadFile(LPCWSTR fileName, LPVOID buffer, DWORD bufferLength,
            LPDWORD readLength, LONGLONG offset, PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	int ret;

	ind = AllocIndication(self, ATOM_READ_FILE);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddUInt(ind, bufferLength);
	IndAddInt64(ind, &offset);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = ParseReadResponse(self, ind, buffer, bufferLength, readLength);

out:
	FreeIndication(self, ind);
	return ret;
}

/*
 * {ok, BytesWritten} | {error, ErrNo}
 */
static int ParseWriteResponse(struct parse_state *ps, LPDWORD bytesWritten)
{
	int type, size;
	char tag[6];

	CHECK(ei_decode_tuple_header(ps->buf, &ps->index, &size));
	CHECK(size != 2);
	CHECK(ei_get_type(ps->buf, &ps->index, &type, &size));
	CHECK(type != ERL_ATOM_EXT || size > sizeof(tag)-1);
	CHECK(ei_decode_atom(ps->buf, &ps->index, tag));

	if (!strcmp(tag, "ok")) {
		CHECK(ei_decode_ulong(ps->buf, &ps->index, bytesWritten));
		return 0;
	} else if (!strcmp(tag, "error")) {
		long ret;
		CHECK(ei_decode_long(ps->buf, &ps->index, &ret));
		return ret;
	}

badarg:
	Abort(ps->drv);
	return -ERROR_GEN_FAILURE;
}

static int __stdcall
ReqWriteFile(LPCWSTR fileName, LPCVOID buffer, DWORD bytesToWrite,
             LPDWORD bytesWritten, LONGLONG offset, PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	*bytesWritten = 0; /* nothing written yet */

	ind = AllocIndication(self, ATOM_WRITE_FILE);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddBuffer(ind, buffer, bytesToWrite);
	IndAddInt64(ind, &offset);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseWriteResponse(&ps, bytesWritten);

out:
	FreeIndication(self, ind);
	return ret;
}

static int __stdcall
ReqFlushFileBuffers(LPCWSTR fileName, PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_FLUSH_FILE_BUFFERS);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseGenericResponse(&ps);

out:
	FreeIndication(self, ind);
	return ret;
}

static int __stdcall
ReqDeleteFile(LPCWSTR fileName, PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_DELETE_FILE);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseGenericResponse(&ps);

out:
	FreeIndication(self, ind);
	return ret;
}

static int __stdcall
ReqDeleteDirectory(LPCWSTR fileName, PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_DELETE_DIRECTORY);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseGenericResponse(&ps);

out:
	FreeIndication(self, ind);
	return ret;
}

static int __stdcall
ReqMoveFile(LPCWSTR fileName, LPCWSTR newFileName, BOOL replaceIfExisting,
            PDOKAN_FILE_INFO fileInfo)
{
	struct self *self = FromFileInfo(fileInfo);
	struct indication *ind;
	struct parse_state ps;
	int ret;

	ind = AllocIndication(self, ATOM_MOVE_FILE);
	if (!ind)
		return -ERROR_OUTOFMEMORY;

	/* fill indication */
	IndAddString(ind, fileName);
	IndAddString(ind, newFileName);
	IndAddBool(ind, replaceIfExisting);
	IndAddFileInfo(ind, fileInfo);
	IndAddDone(ind);

	if ((ret = SendIndication(self, ind)))
		goto out;

	/* parse response */
	ret = InitParser(self, &ps, ind);
	if (ret)
		goto out;
	ret = ParseGenericResponse(&ps);

out:
	FreeIndication(self, ind);
	return ret;
}

static int ReplyOk(char **rbuf, int rlen)
{
	int i = 0;

	/* There should be enough room already in the buffer */
	ei_encode_version(*rbuf, &i);
	ei_encode_atom(*rbuf, &i, "ok");
	return i;
}

static int ReplyError(char **rbuf, int rlen, char *error)
{
	ei_x_buff x;
	int len;

	if (ei_x_new_with_version(&x))
		return -1;
	ei_x_encode_tuple_header(&x, 2);
	ei_x_encode_atom(&x, "error");
	ei_x_encode_atom(&x, error);

	len = x.index;
	if (len > rlen)
		*rbuf = driver_alloc(len);

	if (!*rbuf) {
		ei_x_free(&x);
		return -1;
	}
	memcpy(*rbuf, x.buff, len);

	ei_x_free(&x);
	return len;
}

static void *MainThread(void *arg)
{
	struct self *self = (struct self *)arg;

	self->dokanResult = DokanMain(&self->dokanArgs, &self->dokanOps);

	self->unmounted = 1;
	SetEvent(self->outputEvent);

	return NULL;
}

#define SUPPORTED_OP(_tag, _field, _callback) \
	if (!strcmp(_tag, atom)) \
		self->dokanOps._field = _callback

static int Mount(struct self *self, char *buf, int len, char **rbuf, int rlen)
{
	int index = 0, tmp, size, i;
	char atom[MAXATOMLEN];

	/* set defaults */
	self->dokanArgs.GlobalContext = (unsigned long)self;
	self->dokanArgs.Version = DOKAN_VERSION;
	self->dokanArgs.ThreadCount = 0;
	if (self->dokanArgs.MountPoint)
		driver_free((void *)self->dokanArgs.MountPoint);
	self->dokanArgs.MountPoint = driver_alloc(sizeof(DEFAULT_MOUNT_POINT));
	if (!self->dokanArgs.MountPoint)
		return ReplyError(rbuf, rlen, "enomem");
	wcscpy((LPWSTR)self->dokanArgs.MountPoint, DEFAULT_MOUNT_POINT);

	if (ei_decode_version(buf, &index, &tmp))
		goto badarg;
	if (ei_decode_tuple_header(buf, &index, &tmp))
		goto badarg;
	if (tmp != 2)
		goto badarg;

	/* Parse options */
	if (ei_decode_list_header(buf, &index, &size))
		goto badarg;

	for (i=0; i<size; i++) {
		if (ei_decode_tuple_header(buf, &index, &tmp))
			goto badarg;
		if (tmp != 2)
			goto badarg;
		if (ei_decode_atom(buf, &index, atom))
			goto badarg;

		if (!strcmp(atom, "mountpoint")) {
			int type, len;
			long x;

			if (ei_get_type(buf, &index, &type, &len))
				goto badarg;
			if (type != ERL_BINARY_EXT)
				goto badarg;

			if (self->dokanArgs.MountPoint) {
				driver_free((void *)self->dokanArgs.MountPoint);
				self->dokanArgs.MountPoint = NULL;
			}

			self->dokanArgs.MountPoint = driver_alloc(len+2);
			if (!self->dokanArgs.MountPoint)
				return ReplyError(rbuf, rlen, "enomem");
			memset((void *)self->dokanArgs.MountPoint, 0, len+2);
			if (ei_decode_binary(buf, &index, (void *)self->dokanArgs.MountPoint, &x))
				goto badarg;
		} else if (!strcmp(atom, "threads")) {
			long count;
			if (ei_decode_long(buf, &index, &count))
				goto badarg;
			self->dokanArgs.ThreadCount = count;
		} else if (!strcmp(atom, "debug_output")) {
			if (ei_decode_atom(buf, &index, atom))
				goto badarg;

			if (!strcmp(atom, "true"))
				self->dokanArgs.Options |= DOKAN_OPTION_DEBUG;
			else if (!strcmp(atom, "stderr"))
				self->dokanArgs.Options |= DOKAN_OPTION_DEBUG | DOKAN_OPTION_STDERR;
			else if (!strcmp(atom, "false"))
				self->dokanArgs.Options |= 0;
			else
				goto badarg;
		} else if (!strcmp(atom, "drive_type")) {
			if (ei_decode_atom(buf, &index, atom))
				goto badarg;

			if (!strcmp(atom, "network"))
				self->dokanArgs.Options |= DOKAN_OPTION_NETWORK;
			else if (!strcmp(atom, "removable"))
				self->dokanArgs.Options |= DOKAN_OPTION_REMOVABLE;
			else if (!strcmp(atom, "hdd"))
				self->dokanArgs.Options |= 0;
			else
				goto badarg;
		} else
			goto badarg;
	}

	/* Parse the list tail (but not in case of empty list) */
	if (size > 0)
		if (ei_decode_list_header(buf, &index, &size))
			goto badarg;

	/* Parse supported operations */
	if (ei_decode_list_header(buf, &index, &size))
		goto badarg;

	memset(&self->dokanOps, 0, sizeof(self->dokanOps));
	for (i=0; i<size; i++) {
		if (ei_decode_atom(buf, &index, atom))
			goto badarg;

		SUPPORTED_OP("cleanup", Cleanup, ReqCleanup);
		SUPPORTED_OP("close_file", CloseFile, ReqCloseFile);
		SUPPORTED_OP("create_directory", CreateDirectory, ReqCreateDirectory);
		SUPPORTED_OP("create_file", CreateFile, ReqCreateFile);
		SUPPORTED_OP("delete_file", DeleteFile, ReqDeleteFile);
		SUPPORTED_OP("delete_directory", DeleteDirectory, ReqDeleteDirectory);
		SUPPORTED_OP("find_files", FindFiles, ReqFindFiles);
		SUPPORTED_OP("find_files_with_pattern", FindFilesWithPattern, ReqFindFilesWithPattern);
		SUPPORTED_OP("flush_file_buffers", FlushFileBuffers, ReqFlushFileBuffers);
		SUPPORTED_OP("get_file_information", GetFileInformation, ReqGetFileInformation);
		SUPPORTED_OP("move_file", MoveFile, ReqMoveFile);
		SUPPORTED_OP("open_directory", OpenDirectory, ReqOpenDirectory);
		SUPPORTED_OP("read_file", ReadFile, ReqReadFile);
		SUPPORTED_OP("write_file", WriteFile, ReqWriteFile);
	}

	/* Parse the list tail (but not in case of empty list) */
	if (size > 0)
		if (ei_decode_list_header(buf, &index, &size))
			goto badarg;

	/* Let's get ready to rumble... */
	self->mounted = 1;
	if (erl_drv_thread_create("dokan_main", &self->dokanThread, MainThread,
	                          self, NULL)) {
		self->mounted = 0;
		return ReplyError(rbuf, rlen, "thread_create_failed");
	}

	return ReplyOk(rbuf, rlen);

badarg:
	return ReplyError(rbuf, rlen, "badarg");
}


static int init(void)
{
	int i;

	for (i=0; i<_ATOM_COUNT; i++)
		atom_table[i] = driver_mk_atom(atom_templates[i]);

	return 0;
}

static ErlDrvData start(ErlDrvPort port, char* cmd)
{
	struct self *self = driver_alloc(sizeof(struct self));

	memset(self, 0, sizeof(*self));
	self->port = port;
	QueueInit(&self->indSendQ);
	QueueInit(&self->indRespQ);
	QueueInit(&self->indFreeQ);
	QueueInit(&self->indLaundryQ);

	self->outputEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
	if (!self->outputEvent)
		goto err_event1;
	if (driver_select(port, (ErlDrvEvent)self->outputEvent,
	                  ERL_DRV_WRITE | ERL_DRV_USE, 1)) {
		CloseHandle(self->outputEvent);
		goto err_event1;
	}

	self->laundryEvent = CreateEvent(NULL, TRUE, FALSE, NULL);
	if (!self->laundryEvent)
		goto err_event2;
	if (driver_select(port, (ErlDrvEvent)self->laundryEvent,
	                  ERL_DRV_READ | ERL_DRV_USE, 1)) {
		CloseHandle(self->laundryEvent);
		goto err_event2;
	}

	return (ErlDrvData)self;

err_event2:
	driver_select(port, (ErlDrvEvent)self->outputEvent,
	              ERL_DRV_WRITE | ERL_DRV_USE, 0);
err_event1:
	QueueDestroy(&self->indLaundryQ);
	QueueDestroy(&self->indFreeQ);
	QueueDestroy(&self->indRespQ);
	QueueDestroy(&self->indSendQ);
	driver_free(self);
	return ERL_DRV_ERROR_GENERAL;
}

static void stop(ErlDrvData handle)
{
	struct self *self = (struct self *)handle;
	struct indication *ind;

	/* Gracefully unmount volume */
	if (self->mounted) {
		/* Don't accept new requests anymore */
		self->mounted = 0;

		/* Cancel already pending requests */
		QueueLock(&self->indSendQ);
		while ((ind = QueueGetLocked(&self->indSendQ))) {
			ind->status = -ERROR_DEV_NOT_EXIST;
			SetEvent(ind->event);
		}
		QueueUnlock(&self->indSendQ);
		QueueLock(&self->indRespQ);
		while ((ind = QueueGetLocked(&self->indRespQ))) {
			ind->status = -ERROR_DEV_NOT_EXIST;
			SetEvent(ind->event);
		}
		QueueUnlock(&self->indRespQ);

		/* Unmount */
		DokanRemoveMountPoint(self->dokanArgs.MountPoint); // TODO: error handling

		/* Wait for main thread to terminate */
		erl_drv_thread_join(self->dokanThread, NULL);
	}

	/*
	 * Clean up...
	 */
	while ((ind = QueueGetLocked(&self->indLaundryQ)))
		DisposeIndication(ind);
	while ((ind = QueueGetLocked(&self->indFreeQ)))
		DisposeIndication(ind);
	while ((ind = QueueGetLocked(&self->indRespQ)))
		DisposeIndication(ind);
	while ((ind = QueueGetLocked(&self->indSendQ)))
		DisposeIndication(ind);

	/*
	 * De-selecting and closing handles is a bit odd. The calls below will
	 * deselect the event handles. Once it is safe to close the handles the
	 * emulator will call 'stop_select' where we can finally close the
	 * handle...
	 */
	driver_select(self->port, (ErlDrvEvent)self->laundryEvent,
	              ERL_DRV_READ | ERL_DRV_USE, 0);
	driver_select(self->port, (ErlDrvEvent)self->outputEvent,
	              ERL_DRV_WRITE | ERL_DRV_USE, 0);

	if (self->dokanArgs.MountPoint)
		driver_free((void *)self->dokanArgs.MountPoint);

	QueueDestroy(&self->indLaundryQ);
	QueueDestroy(&self->indFreeQ);
	QueueDestroy(&self->indRespQ);
	QueueDestroy(&self->indSendQ);
	driver_free(self);
}

static void stop_select(ErlDrvEvent event, void *reserved)
{
	CloseHandle((HANDLE)event);
}

#define DRV_CTRL_MOUNT   0
#define DRV_CTRL_UNMOUNT 1

static int call(ErlDrvData handle, unsigned int command, char *buf, int len,
                char **rbuf, int rlen, unsigned int *flags)
{
	struct self *self = (struct self *)handle;

	switch (command) {
		case DRV_CTRL_MOUNT:
			if (self->mounted)
				return ReplyError(rbuf, rlen, "already_mounted");
			return Mount(self, buf, len, rbuf, rlen);

		case DRV_CTRL_UNMOUNT:
			break;
	}

	return -1;
}

/*
 * Housekeeping callback.
 */
static void ready_input(ErlDrvData handle, ErlDrvEvent event)
{
	struct self *self = (struct self *)handle;
	struct indication *ind;

	ResetEvent(self->laundryEvent);

	QueueLock(&self->indFreeQ);
	QueueLock(&self->indLaundryQ);
	while ((ind = QueueGetLocked(&self->indLaundryQ))) {
		LaunderIndication(ind);
		QueuePutLocked(&self->indFreeQ, ind);
	}
	QueueUnlock(&self->indLaundryQ);
	QueueUnlock(&self->indFreeQ);
}

/*
 * This callback is used to get into the context of the emulator when a dokan
 * callback has queued a new indication.
 */
static void ready_output(ErlDrvData handle, ErlDrvEvent event)
{
	struct self *self = (struct self *)handle;
	struct indication *ind;

	ResetEvent(self->outputEvent);

	if (self->abort) {
		driver_failure_atom(self->port, "bad_response");
		return;
	}

	if (self->unmounted) {
		ErlDrvTermData terms[] = {
			ERL_DRV_PORT, driver_mk_port(self->port),
			ERL_DRV_ATOM, driver_mk_atom("unmounted"),
			ERL_DRV_INT, (ErlDrvTermData)self->dokanResult,
			ERL_DRV_TUPLE, 3
		};

		self->mounted = 0;
		self->unmounted = 0;
		erl_drv_thread_join(self->dokanThread, NULL);

		/* let the emulator know... */
		driver_output_term(self->port, terms, ARRAY_SIZE(terms));
	}

	QueueLock(&self->indSendQ);
	QueueLock(&self->indRespQ);
	while ((ind = QueueGetLocked(&self->indSendQ))) {
		if (driver_output_term(self->port, ind->ind_data, ind->ind_pos) <= 0) {
			/* Could not deliver message. Abort request... */
			ind->status = -ERROR_DEV_NOT_EXIST;
			SetEvent(ind->event);
		} else
			QueuePutLocked(&self->indRespQ, ind);
	}
	QueueUnlock(&self->indRespQ);
	QueueUnlock(&self->indSendQ);
}

/*
 * The handling of the ErlIOVec is a bit tricky. The runtime system may choose
 * to pack small binaries for optimal performace. The code below can cope with
 * this but relies on the assumption that binaries are not split, though.
 *
 * Note that the first vector is reserved for the emulator and must be skipped.
 */
static void outputv(ErlDrvData handle, ErlIOVec *ev)
{
	struct self *self = (struct self *)handle;
	uint32_t reqId;
	struct indication *ind = NULL;
	struct list *iter;
	char *base;
	size_t size;

	if (!self->mounted)
		return;

	if (ev->vsize > IND_IOV_SIZE)
		goto badarg;

	/* 1st parameter is the request number */
	if (IOVecGetOffset(ev, 0, &base, &size))
		goto badarg;
	if (size < 4)
		goto badarg;
	reqId = *((uint32_t*)base);

	/* find the matching pending request */
	QueueLock(&self->indRespQ);
	for (iter = self->indRespQ.head.next; iter != &self->indRespQ.head;
	     iter = iter->next) {
		struct indication *cur = CONTAINER_OF(iter, struct indication, node);

		if (cur->id == reqId) {
			ind = cur;
			QueueRemoveLocked(ind);
			break;
		}
	}
	QueueUnlock(&self->indRespQ);

	if (!ind)
		goto badarg;

	IOVecCopy(&ind->resp, ev);
	ind->status = 0;
	SetEvent(ind->event);

	return;

badarg:
	driver_failure_atom(self->port, "bad_response_iov");
}

static ErlDrvEntry dokan_driver_entry = {
	.driver_name     = "erldokan_drv",
	.init            = init,
	.start           = start,
	.stop            = stop,
	.call            = call,
	.outputv         = outputv,
	.ready_input     = ready_input,
	.ready_output    = ready_output,
	.extended_marker = ERL_DRV_EXTENDED_MARKER,
	.major_version   = ERL_DRV_EXTENDED_MAJOR_VERSION,
	.minor_version   = ERL_DRV_EXTENDED_MINOR_VERSION,
	.driver_flags    = ERL_DRV_FLAG_USE_PORT_LOCKING,
	.stop_select     = stop_select,
};

DRIVER_INIT(erldokan_drv)
{
	return &dokan_driver_entry;
}

