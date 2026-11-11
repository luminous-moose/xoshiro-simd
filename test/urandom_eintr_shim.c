// Test-only LD_PRELOAD shim for check_urandom_fallback.sh's second run. It is
// linked into one library together with getrandom_enosys_shim.c, so the
// /dev/urandom fallback in read_entropy (lib/xs256_common.c) is taken, and on
// top of that the first read() from every /dev/urandom descriptor fails with
// EINTR, exactly as if a signal handler installed without SA_RESTART had run
// while the read was blocked. Only a read_urandom that retries on EINTR gets
// its entropy. Each injection appends "eintr" to $XOSHIRO_SHIM_LOG, so the
// script can tell an injection really happened.
//
// A real signal can't do this deterministically: a 32-byte read from
// /dev/urandom never blocks, so a signal would almost never land inside it.
//
// Every intercepted function forwards to the next definition (libc's) found
// with dlsym(RTLD_NEXT, ...). Covered entry points:
// - open and open64: OCaml's C flags include -D_FILE_OFFSET_BITS=64, which
//   makes glibc's headers rename open to open64, so on Linux the library calls
//   open64. The BSDs have only open.
// - read, and glibc's __read_chk, which _FORTIFY_SOURCE substitutes for read
//   when the buffer's size is known at compile time (it isn't in today's build,
//   but inlining could change that).
// - close, so a descriptor number reused after /dev/urandom is closed isn't
//   mistaken for it.

// Neither of the first two may apply to this file: _FORTIFY_SOURCE would give
// open and read inline definitions in the headers that clash with the ones
// below, and _FILE_OFFSET_BITS=64 would silently turn the open definition into
// open64. _TIME_BITS has to go with _FILE_OFFSET_BITS: Debian's 32-bit ports
// compile with both set to 64, and glibc rejects _TIME_BITS=64 on its own.
// Nothing here uses time_t, so the shim doesn't care which width it gets.
#undef _FORTIFY_SOURCE
#undef _FILE_OFFSET_BITS
#undef _TIME_BITS
#define _GNU_SOURCE

#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <unistd.h>

// Descriptors at or above this are never tracked. The probe opens only a
// handful of files, so its descriptors stay far below it.
#define MAX_TRACKED_FD 1024

// pending[fd] is 1 from opening /dev/urandom as fd until its first read.
static unsigned char pending[MAX_TRACKED_FD];

static void* next_symbol(const char* name) {
    void* fn = dlsym(RTLD_NEXT, name);
    if (fn == NULL) {
        // Every forwarded function exists in libc wherever it's called, so
        // this means the shim itself is broken: fail loudly, not quietly.
        abort();
    }
    return fn;
}

static void log_eintr(void) {
    const char* log = getenv("XOSHIRO_SHIM_LOG");
    if (log == NULL) {
        return;
    }
    // This open goes through the interposed open below, which is harmless:
    // the log isn't /dev/urandom, so it is just forwarded.
    int fd = open(log, O_WRONLY | O_APPEND | O_CREAT | O_CLOEXEC, 0600);
    if (fd >= 0) {
        static const char line[] = "eintr\n";
        ssize_t ignored = write(fd, line, sizeof line - 1);
        (void)ignored;
        close(fd);
    }
}

// open's mode argument is only passed when the flags need one. mode_t can be
// narrower than int (it's 16 bits on FreeBSD), so it arrives promoted to int.
static int mode_argument(int flags, va_list ap) {
    int needs_mode = (flags & O_CREAT) != 0;
#if defined(O_TMPFILE)
    needs_mode = needs_mode || (flags & O_TMPFILE) == O_TMPFILE;
#endif
    return needs_mode ? va_arg(ap, int) : 0;
}

static int tracked_open(
    int (*real_open)(const char*, int, ...),
    const char* path,
    int flags,
    int mode
) {
    int fd = real_open(path, flags, mode);
    if (fd >= 0 && fd < MAX_TRACKED_FD) {
        pending[fd] = strcmp(path, "/dev/urandom") == 0;
    }
    return fd;
}

int open(const char* path, int flags, ...) {
    static int (*real_open)(const char*, int, ...);
    if (real_open == NULL) {
        real_open = (int (*)(const char*, int, ...))next_symbol("open");
    }
    va_list ap;
    va_start(ap, flags);
    int mode = mode_argument(flags, ap);
    va_end(ap);
    return tracked_open(real_open, path, flags, mode);
}

// glibc only. Elsewhere nothing calls it, so it never looks up a missing
// symbol.
int open64(const char* path, int flags, ...) {
    static int (*real_open64)(const char*, int, ...);
    if (real_open64 == NULL) {
        real_open64 = (int (*)(const char*, int, ...))next_symbol("open64");
    }
    va_list ap;
    va_start(ap, flags);
    int mode = mode_argument(flags, ap);
    va_end(ap);
    return tracked_open(real_open64, path, flags, mode);
}

// Returns 1, after logging, if this read from fd should fail with EINTR.
static int inject_eintr(int fd) {
    if (fd < 0 || fd >= MAX_TRACKED_FD || !pending[fd]) {
        return 0;
    }
    pending[fd] = 0;
    log_eintr();
    return 1;
}

ssize_t read(int fd, void* buf, size_t count) {
    static ssize_t (*real_read)(int, void*, size_t);
    if (real_read == NULL) {
        real_read = (ssize_t (*)(int, void*, size_t))next_symbol("read");
    }
    if (inject_eintr(fd)) {
        errno = EINTR;
        return -1;
    }
    return real_read(fd, buf, count);
}

// glibc only, like open64.
ssize_t __read_chk(int fd, void* buf, size_t count, size_t buflen);
ssize_t __read_chk(int fd, void* buf, size_t count, size_t buflen) {
    static ssize_t (*real_read_chk)(int, void*, size_t, size_t);
    if (real_read_chk == NULL) {
        real_read_chk = (ssize_t (*)(int, void*, size_t, size_t))next_symbol(
            "__read_chk"
        );
    }
    if (inject_eintr(fd)) {
        errno = EINTR;
        return -1;
    }
    return real_read_chk(fd, buf, count, buflen);
}

int close(int fd) {
    static int (*real_close)(int);
    if (real_close == NULL) {
        real_close = (int (*)(int))next_symbol("close");
    }
    if (fd >= 0 && fd < MAX_TRACKED_FD) {
        pending[fd] = 0;
    }
    return real_close(fd);
}
