#include "bit_reverse.h"

#include "bit_reverse_table.h"

uint64_t xoshiro_bitreverse64(uint64_t w) {
    uint64_t byte0 = (uint64_t)xoshiro_byte_bitreverse_table[(uint8_t)w];
    uint64_t byte1 = (uint64_t)xoshiro_byte_bitreverse_table[(uint8_t)(w >> 8)]
                     << 8;
    uint64_t byte2 = (uint64_t)xoshiro_byte_bitreverse_table[(uint8_t)(w >> 16)]
                     << 16;
    uint64_t byte3 = (uint64_t)xoshiro_byte_bitreverse_table[(uint8_t)(w >> 24)]
                     << 24;
    uint64_t byte4 = (uint64_t)xoshiro_byte_bitreverse_table[(uint8_t)(w >> 32)]
                     << 32;
    uint64_t byte5 = (uint64_t)xoshiro_byte_bitreverse_table[(uint8_t)(w >> 40)]
                     << 40;
    uint64_t byte6 = (uint64_t)xoshiro_byte_bitreverse_table[(uint8_t)(w >> 48)]
                     << 48;
    uint64_t byte7 = (uint64_t)xoshiro_byte_bitreverse_table[(uint8_t)(w >> 56)]
                     << 56;

    uint64_t t = byte0 | byte1 | byte2 | byte3 | byte4 | byte5 | byte6 | byte7;

    return __builtin_bswap64(t);
}