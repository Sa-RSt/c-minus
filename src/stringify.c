#include "stringify.h"
#include "typedefs.h"
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>

char staticStringifyBuffer[STATIC_STRINGIFY_BUFFER_SIZE];

DECLARE_STRINGIFY_FUNCTION(charp, s) { STRINGIFY_PUT(s); }

DECLARE_STRINGIFY_FUNCTION(size_t, n) {
  char buf[32];
  sprintf(buf, "%lu", n);
  STRINGIFY_PUT(buf);
}

DECLARE_STRINGIFY_FUNCTION(uint64_t, n) {
  char buf[32];
  sprintf(buf, "%lu", n);
  STRINGIFY_PUT(buf);
}

DECLARE_STRINGIFY_FUNCTION(int64_t, n) {
  char buf[32];
  sprintf(buf, "%li", n);
  STRINGIFY_PUT(buf);
}

DECLARE_STRINGIFY_FUNCTION(uint32_t, n) {
  char buf[32];
  sprintf(buf, "%u", n);
  STRINGIFY_PUT(buf);
}

DECLARE_STRINGIFY_FUNCTION(int32_t, n) {
  char buf[32];
  sprintf(buf, "%i", n);
  STRINGIFY_PUT(buf);
}

DECLARE_STRINGIFY_FUNCTION(uint16_t, n) {
  char buf[32];
  sprintf(buf, "%u", n);
  STRINGIFY_PUT(buf);
}

DECLARE_STRINGIFY_FUNCTION(int16_t, n) {
  char buf[32];
  sprintf(buf, "%i", n);
  STRINGIFY_PUT(buf);
}

DECLARE_STRINGIFY_FUNCTION(uint8_t, n) {
  char buf[32];
  sprintf(buf, "%u", n);
  STRINGIFY_PUT(buf);
}

DECLARE_STRINGIFY_FUNCTION(int8_t, n) {
  char buf[32];
  sprintf(buf, "%i", n);
  STRINGIFY_PUT(buf);
}

DECLARE_STRINGIFY_FUNCTION(double, n) {
  char buf[32];
  sprintf(buf, "%.3f", n);
  STRINGIFY_PUT(buf);
}

DECLARE_STRINGIFY_FUNCTION(bool, v) {
  if (v) {
    STRINGIFY_PUT("true");
  } else {
    STRINGIFY_PUT("false");
  }
}

DECLARE_STRINGIFY_FUNCTION(char, c) {
  char buf[2];
  buf[0] = c;
  buf[1] = 0;
  STRINGIFY_PUT(buf);
}

DECLARE_STRINGIFY_FUNCTION(voidp, ptr) {
  char buf[32];
  sprintf(buf, "%p", ptr);
  STRINGIFY_PUT(buf);
}
