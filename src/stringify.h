#pragma once

#include "typedefs.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <string.h>

#define STATIC_STRINGIFY_BUFFER_SIZE 524288

extern char staticStringifyBuffer[STATIC_STRINGIFY_BUFFER_SIZE];

#define STRINGIFY_FUNCTION(T) stringify_##T

#define STRINGIFY_VALUE(T, value, out) STRINGIFY_FUNCTION(T)(value, out)

#define DECLARE_STRINGIFY_FUNCTION(T, argname)                                 \
  void STRINGIFY_FUNCTION(T)(T argname, charp STRINGIFY_out)

#define STRINGIFY_PUT(string)                                                  \
  strcat(STRINGIFY_out, string);                                               \
  STRINGIFY_out += strlen(STRINGIFY_out)

#define STRINGIFY_PUT_VALUE(T, value) STRINGIFY_VALUE(T, value, STRINGIFY_out)

#define STRINGIFY_DBG(T, value)                                                \
  do {                                                                         \
    staticStringifyBuffer[0] = 0;                                              \
    STRINGIFY_VALUE(T, value, staticStringifyBuffer);                          \
    puts(staticStringifyBuffer);                                               \
  } while (0)

DECLARE_STRINGIFY_FUNCTION(charp, s);
DECLARE_STRINGIFY_FUNCTION(size_t, n);
DECLARE_STRINGIFY_FUNCTION(uint64_t, n);
DECLARE_STRINGIFY_FUNCTION(int64_t, n);
DECLARE_STRINGIFY_FUNCTION(uint32_t, n);
DECLARE_STRINGIFY_FUNCTION(int32_t, n);
DECLARE_STRINGIFY_FUNCTION(uint16_t, n);
DECLARE_STRINGIFY_FUNCTION(int16_t, n);
DECLARE_STRINGIFY_FUNCTION(uint8_t, n);
DECLARE_STRINGIFY_FUNCTION(int8_t, n);
DECLARE_STRINGIFY_FUNCTION(double, n);
DECLARE_STRINGIFY_FUNCTION(char, c);
DECLARE_STRINGIFY_FUNCTION(voidp, ptr);
DECLARE_STRINGIFY_FUNCTION(bool, v);
