// Test-only LD_PRELOAD shim for check_urandom_fallback.sh. It makes both
// entropy calls that read_entropy in lib/xs256_common.c can use fail, so
// read_entropy has to take its /dev/urandom fallback, which no test host
// otherwise ever reaches: getrandom() (Linux, FreeBSD, DragonFly) fails with
// ENOSYS, as on a Linux kernel older than 3.17, and getentropy() (NetBSD,
// OpenBSD) fails with EIO. Defining both is harmless: only the one the library
// calls on a given OS ever gets called.
//
// Each call appends a line to $XOSHIRO_SHIM_LOG, so the script can tell the
// shim was really interposed rather than silently ignored (which would leave
// the fallback untested while everything passes).
//
// <sys/random.h> is deliberately not included: getrandom's prototype isn't
// needed, and leaving it out removes one chance of a declaration mismatch.
// getentropy's prototype comes from <unistd.h> on every OS here, which keeps
// the definition below checked against it.

#include <errno.h>
#include <fcntl.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

static void log_call(const char* line, size_t length) {
    const char* log = getenv("XOSHIRO_SHIM_LOG");
    if (log == NULL) {
        return;
    }
    int fd = open(log, O_WRONLY | O_APPEND | O_CREAT | O_CLOEXEC, 0600);
    if (fd >= 0) {
        ssize_t ignored = write(fd, line, length);
        (void)ignored;
        close(fd);
    }
}

ssize_t getrandom(void* buf, size_t buflen, unsigned int flags) {
    (void)buf;
    (void)buflen;
    (void)flags;
    static const char line[] = "getrandom\n";
    log_call(line, sizeof line - 1);
    // Set last, so nothing above can overwrite it.
    errno = ENOSYS;
    return -1;
}

int getentropy(void* buf, size_t buflen) {
    (void)buf;
    (void)buflen;
    static const char line[] = "getentropy\n";
    log_call(line, sizeof line - 1);
    errno = EIO;
    return -1;
}
