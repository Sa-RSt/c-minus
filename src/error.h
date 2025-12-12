#pragma once

#include "stringify.h"
#include <stdint.h>
#include <stdio.h>

#define ERROR_TYPE_FIELDS                                                      \
  uint32_t line;                                                               \
  char *filename;                                                              \
  char *message;                                                               \
  void *caused;

typedef struct __anonymous_error {
  ERROR_TYPE_FIELDS
} __anonymous_error;

#define HEADER_ERROR_TYPE_FUNCTIONS(T)                                         \
  typedef __anonymous_error *__##T##_errptr_t;                                 \
  DECLARE_STRINGIFY_FUNCTION(__##T##_errptr_t, err);                           \
  DECLARE_STRINGIFY_FUNCTION(T, err);

#define DECLARE_ERROR_TYPE_FUNCTIONS(T)                                        \
  typedef __anonymous_error *__##T##_errptr_t;                                 \
                                                                               \
  DECLARE_STRINGIFY_FUNCTION(__##T##_errptr_t, err) {                          \
    static char fmted_str[4096];                                               \
    sprintf(fmted_str, "%s:%u - %s", err->filename, err->line, err->message);  \
    STRINGIFY_PUT(fmted_str);                                                  \
    if (err->caused != NULL) {                                                 \
      STRINGIFY_PUT(" - caused by:\n ");                                       \
      __anonymous_error *cast = (__anonymous_error *)err->caused;              \
      STRINGIFY_PUT_VALUE(__##T##_errptr_t, cast);                             \
    }                                                                          \
  }                                                                            \
                                                                               \
  DECLARE_STRINGIFY_FUNCTION(T, err) {                                         \
    STRINGIFY_PUT_VALUE(__##T##_errptr_t, ((__anonymous_error *)&err));        \
  }

#define CREATE_ERROR_FROM(target, msg, other_error)                            \
  do {                                                                         \
    (target)->line = __LINE__;                                                 \
    (target)->filename = __FILE_NAME__;                                        \
    (target)->message = msg;                                                   \
    (target)->caused = other_error;                                            \
  } while (0)

#define CREATE_ERROR(target, msg) CREATE_ERROR_FROM(target, msg, NULL)
