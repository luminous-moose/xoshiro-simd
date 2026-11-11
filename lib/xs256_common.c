#include "xs256_common.h"

#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "xs256_dispatch.h"
#include "xs256_scalar_step.h"

// Platform / OS portability

#if defined(_WIN32)
#include <windows.h>
#include <bcrypt.h>

static int read_entropy(void* buf, size_t len) {
    assert(len <= (size_t)(ULONG)-1u);
    NTSTATUS st = BCryptGenRandom(
        NULL,
        (PUCHAR)buf,
        (ULONG)len,
        BCRYPT_USE_SYSTEM_PREFERRED_RNG
    );
    return BCRYPT_SUCCESS(st) ? 0 : -1;
}

#else // !defined(_WIN32)
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#if defined(__linux__) || defined(__FreeBSD__) || defined(__DragonFly__) ||    \
    defined(__CYGWIN__)
#include <sys/random.h>
#define XS256_HAVE_GETRANDOM 1
#elif defined(__NetBSD__) || defined(__OpenBSD__) || defined(__APPLE__)
#if defined(__APPLE__)
#include <sys/random.h>
#endif // defined(__APPLE__)
#define XS256_HAVE_GETENTROPY 1
#endif // defined(__linux__) || etc

static int read_urandom(void* buf, size_t len) {
    int fd = open("/dev/urandom", O_RDONLY | O_CLOEXEC);
    if (fd < 0) {
        return -1;
    }
    size_t done = 0;
    // Loop in case of short reads (rare but possible)
    while (done < len) {
        ssize_t n = read(fd, (char*)buf + done, len - done);
        if (n < 0 && errno == EINTR) {
            continue;
        }
        if (n <= 0) {
            close(fd);
            return -1;
        }
        done += (size_t)n;
    }
    close(fd);
    return 0;
}

static int read_entropy(void* buf, size_t len) {
#if defined(XS256_HAVE_GETRANDOM)
    size_t done = 0;
    while (done < len) {
        ssize_t n = getrandom((char*)buf + done, len - done, 0);
        if (n < 0) {
            if (errno == EINTR) {
                continue;
            }
            return read_urandom(buf, len);
        }
        done += (size_t)n;
    }
    return 0;
#elif defined(XS256_HAVE_GETENTROPY)
    // A much nicer interface that doesn't require handling EINTR.
    if (getentropy(buf, len) == 0) {
        return 0;
    }
    return read_urandom(buf, len);
#else  // !defined(XS256_HAVE_GETRANDOM) && !defined(XS256_HAVE_GETENTROPY)
    return read_urandom(buf, len);
#endif // defined(XS256_HAVE_GETRANDOM)
}

#endif // defined(_WIN32)

// State initialization

static uint64_t splitmix64_next(uint64_t* state) {
    uint64_t z = (*state += 0x9e3779b97f4a7c15ULL);
    z = (z ^ (z >> 30)) * 0xbf58476d1ce4e5b9ULL;
    z = (z ^ (z >> 27)) * 0x94d049bb133111ebULL;
    return z ^ (z >> 31);
}

// Entropy mixer, inspired by NumPy's SeedSequence, but using 64 bit words
// and 64 bit constants.

static const uint64_t HASHMIX_INIT = 0x9e3779b97f4a7c15ULL; // Splitmix64
static const uint64_t HASHMIX_MULT = 0x00000100000001b3ULL; // FNV prime
static const uint64_t MIX_MULT_L = 0xbf58476d1ce4e5b9ULL;   // Splitmix64
static const uint64_t MIX_MULT_R = 0x94d049bb133111ebULL;   // Splitmix64

static uint64_t hashmix(uint64_t value, uint64_t* mix_state) {
    value ^= *mix_state;
    *mix_state *= HASHMIX_MULT;
    value *= *mix_state;
    value ^= value >> 32;
    return value;
}

static uint64_t mix(uint64_t x, uint64_t y) {
    uint64_t result = MIX_MULT_L * x - MIX_MULT_R * y;
    result ^= result >> 32;
    return result;
}

// For length == 4, a 256 bit seed, this function is a bijection on the full
// 256 bit input/output space.
static void mix_entropy(
    uint64_t pool[4],
    const uint64_t* entropy_array,
    size_t length
) {
    uint64_t mix_state = HASHMIX_INIT;

    // Add all the entropy up to the pool size, padding with 0 if necessary.
    for (size_t i = 0; i < 4; i++) {
        if (i < length) {
            pool[i] = hashmix(entropy_array[i], &mix_state);
        } else {
            pool[i] = hashmix(0, &mix_state);
        }
    }

    // Every slot is mixed into every other slot.
    for (size_t i_src = 0; i_src < 4; i_src++) {
        for (size_t i_dst = 0; i_dst < 4; i_dst++) {
            if (i_src != i_dst) {
                pool[i_dst] =
                    mix(pool[i_dst], hashmix(pool[i_src], &mix_state));
            }
        }
    }

    // Mix any remaining entropy into each pool word.
    for (size_t i_src = 4; i_src < length; i_src++) {
        for (size_t i_dst = 0; i_dst < 4; i_dst++) {
            pool[i_dst] =
                mix(pool[i_dst], hashmix(entropy_array[i_src], &mix_state));
        }
    }
}

static void jump(uint64_t s[static 4 * XS256_LANES], size_t lane) {
    static const uint64_t JUMP[] = {
        0x180ec6d33cfd0abaULL,
        0xd5a61266f0c9392cULL,
        0xa9582618e03fc9aaULL,
        0x39abdc4529b1661cULL
    };

    uint64_t s0 = 0;
    uint64_t s1 = 0;
    uint64_t s2 = 0;
    uint64_t s3 = 0;
    for (size_t i = 0; i < sizeof JUMP / sizeof *JUMP; i++) {
        for (size_t b = 0; b < 64; b++) {
            if (JUMP[i] & UINT64_C(1) << b) {
                s0 ^= s[lane];
                s1 ^= s[lane + XS256_LANES];
                s2 ^= s[lane + 2 * XS256_LANES];
                s3 ^= s[lane + 3 * XS256_LANES];
            }
            xs256plusplus_scalar_next(s, lane);
        }
    }

    s[lane] = s0;
    s[lane + XS256_LANES] = s1;
    s[lane + 2 * XS256_LANES] = s2;
    s[lane + 3 * XS256_LANES] = s3;
}

// Setup helpers

static void setup_lane0(xoshiro256pp* state, const uint64_t words[4]) {
    for (size_t i = 0; i < 4; i++) {
        state->s[i * XS256_LANES] = words[i];
    }
}

static void setup_lanes(xoshiro256pp* state) {
    for (size_t lane = 1; lane < XS256_LANES; lane++) {
        for (size_t i = 0; i < 4; i++) {
            state->s[i * XS256_LANES + lane] =
                state->s[i * XS256_LANES + lane - 1];
        }
        jump(state->s, lane);
    }
}

static int initialize_generator(xoshiro256pp* state, const uint64_t words[4]) {
    if (words[0] == 0 && words[1] == 0 && words[2] == 0 && words[3] == 0) {
        return XS256_ENTROPY_ALL_ZERO;
    }
    setup_lane0(state, words);
    setup_lanes(state);
    return 0;
}

// The generator is set up by first initializing the state of lane 0 through one
// of the provided methods. Lanes 1 through 3 are set up by using the xoshiro256
// jump functions, and then the function pointers are initialized to use the
// selected backend. The state is wiped before checking for errors, but every
// caller either discards the state on error or rebuilds it.
static int finish_setup(
    xoshiro256pp* state,
    const uint64_t words[4],
    xoshiro256pp_backend backend
) {
    memset(state, 0, sizeof(xoshiro256pp));
    int retval = initialize_generator(state, words);
    if (retval < 0) {
        return retval;
    }
    return xs256simd_dispatch_init(state, backend);
}

int xs256simd_setup(
    xoshiro256pp* state,
    const uint64_t seed,
    xoshiro256pp_backend backend
) {
    uint64_t sm_state = seed;
    uint64_t words[4];
    for (size_t i = 0; i < 4; i++) {
        words[i] = splitmix64_next(&sm_state);
    }
    // SplitMix64 can never produce the all 0 state.
    return finish_setup(state, words, backend);
}

// For all mixer input init functions, we check for the all-zero state and
// replace it with the state SplitMix64 gives for seed 42 (SplitMix64 can never
// produce the all-zero state). The main goal of this is to ensure that the
// OCaml split function can never return the all-zero state by chance.
static int finish_setup_mixed(
    xoshiro256pp* state,
    const uint64_t pool[4],
    xoshiro256pp_backend backend
) {
    int retval = finish_setup(state, pool, backend);
    if (retval == XS256_ENTROPY_ALL_ZERO) {
        return xs256simd_setup(state, 42, backend);
    }
    return retval;
}

// Assemble a uint64_t from up to 8 given bytes in little-endian order,
// independent of host endianness.
static uint64_t load_le64(const uint8_t* src, size_t avail) {
    uint64_t word = 0;
    for (size_t i = 0; i < avail; i++) {
        word |= (uint64_t)src[i] << (8 * i);
    }
    return word;
}

int xs256simd_setup_from_string(
    xoshiro256pp* state,
    const void* seed,
    size_t seed_length, // in bytes
    xoshiro256pp_backend backend
) {
    assert(seed_length > 0);
    const uint8_t* bytes = (const uint8_t*)seed;
    size_t n_words = (seed_length + 7) / 8;
    uint64_t* words = malloc(n_words * sizeof(uint64_t));
    assert(words != NULL);
    for (size_t i = 0; i < n_words; i++) {
        size_t offset = i * 8;
        size_t avail = seed_length - offset;
        words[i] = load_le64(bytes + offset, avail < 8 ? avail : 8);
    }

    uint64_t pool[4] = {0, 0, 0, 0};
    mix_entropy(pool, words, n_words);
    free(words);

    return finish_setup_mixed(state, pool, backend);
}

int xs256simd_setup_from_array(
    xoshiro256pp* state,
    const uint64_t* words,
    size_t n_words,
    xoshiro256pp_backend backend
) {
    assert(n_words > 0);
    uint64_t pool[4] = {0, 0, 0, 0};
    mix_entropy(pool, words, n_words);

    return finish_setup_mixed(state, pool, backend);
}

// This function does not bother to use the mixer: OS entropy is already uniform
// over 256 bits.
int xs256simd_setup_from_entropy(
    xoshiro256pp* state,
    xoshiro256pp_backend backend
) {
    uint64_t seed[4] = {0, 0, 0, 0};
    if (read_entropy(seed, sizeof(seed)) == -1) {
        return XS256_ENTROPY_READ_ERROR;
    }

    return finish_setup(state, seed, backend);
}

// This is the reseed path, and doesn't touch any of the function pointers,
// unlike all setup paths. On error it returns before state is changed. Old
// buffer contents are left in place, but the OCaml side marks them as used up.
int xs256simd_seed_from_entropy(xoshiro256pp* state) {
    uint64_t seed[4] = {0, 0, 0, 0};
    if (read_entropy(seed, sizeof(seed)) == -1) {
        return XS256_ENTROPY_READ_ERROR;
    }

    return initialize_generator(state, seed);
}

// Byte fills

void xs256simd_fill_bytes(xoshiro256pp* state, uint8_t* dst, size_t length) {
    size_t num_blocks = length / (XS256_LANES * sizeof(uint64_t));
    size_t remainder = length % (XS256_LANES * sizeof(uint64_t));

    state->xs256_fill_blocks_fn(state, dst, num_blocks);

    if (remainder > 0) {
        uint64_t buffer[XS256_LANES];
        state->xs256_fill_blocks_fn(state, buffer, 1);
        memcpy(
            dst + num_blocks * XS256_LANES * sizeof(uint64_t),
            buffer,
            remainder
        );
    }
}

#if XS256_BIG_ENDIAN

static inline __attribute__((always_inline)) void fill_bytes_be_transformed(
    xoshiro256pp* state,
    uint8_t* dst,
    size_t num_blocks,
    size_t remainder,
    uint64_t (*transform)(uint64_t)
) {
    for (size_t i = 0; i < num_blocks; i++) {
        uint64_t buffer[XS256_LANES];
        state->xs256_fill_blocks_fn(state, buffer, 1);
        for (size_t j = 0; j < XS256_LANES; j++) {
            uint64_t swapped = (*transform)(buffer[j]);
            memcpy(
                dst + i * XS256_LANES * sizeof(uint64_t) + j * sizeof(uint64_t),
                &swapped,
                sizeof(uint64_t)
            );
        }
    }
    if (remainder > 0) {
        uint64_t buffer[XS256_LANES];
        state->xs256_fill_blocks_fn(state, buffer, 1);
        for (size_t j = 0; j < XS256_LANES; j++) {
            buffer[j] = (*transform)(buffer[j]);
        }
        memcpy(
            dst + num_blocks * XS256_LANES * sizeof(uint64_t),
            buffer,
            remainder
        );
    }
}

// On big-endian systems, each element width needs its own word transform so
// Bigarray values match little-endian. int64 needs none because xoshiro256++
// natively outputs uint64_ts, so it uses xs256simd_fill_bytes.

void xs256simd_fill_bytes_be_int8(
    xoshiro256pp* state,
    uint8_t* dst,
    size_t length
) {
    size_t num_blocks = length / (XS256_LANES * sizeof(uint64_t));
    size_t remainder = length % (XS256_LANES * sizeof(uint64_t));

    fill_bytes_be_transformed(
        state,
        dst,
        num_blocks,
        remainder,
        xs256simd_swap_bytes64
    );
}

void xs256simd_fill_bytes_be_int16(
    xoshiro256pp* state,
    uint8_t* dst,
    size_t length
) {
    size_t num_blocks = length / (XS256_LANES * sizeof(uint64_t));
    size_t remainder = length % (XS256_LANES * sizeof(uint64_t));
    assert(remainder % 2 == 0);

    fill_bytes_be_transformed(
        state,
        dst,
        num_blocks,
        remainder,
        xs256simd_swap_quarters64
    );
}

void xs256simd_fill_bytes_be_int32(
    xoshiro256pp* state,
    uint8_t* dst,
    size_t length
) {
    size_t num_blocks = length / (XS256_LANES * sizeof(uint64_t));
    size_t remainder = length % (XS256_LANES * sizeof(uint64_t));
    assert(remainder % 4 == 0);

    fill_bytes_be_transformed(
        state,
        dst,
        num_blocks,
        remainder,
        xs256simd_swap_halves64
    );
}

#endif // XS256_BIG_ENDIAN
