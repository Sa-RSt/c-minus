#pragma once

#include "stringify.h"
#include "vector.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

HEADER_VECTOR_TYPE(char, char)
HEADER_VECTOR_TYPE(Vector_char, Vector_char *)
HEADER_VECTOR_TYPE(Vector_Vector_char, Vector_Vector_char *)

char *charVecCreateCArray(const Vector_char *str);
Vector_char charVecFromCArray(const char *arr);
void charVecAppendCArray(Vector_char *dest, const char *src);
Vector_char charVecStripWhitespace(const Vector_char *src);
Vector_char charVecNormWhitespace(Vector_char *src);
int charVecStrcmp(const Vector_char *left, const Vector_char *right);
DECLARE_STRINGIFY_FUNCTION(Vector_char, str);
DECLARE_STRINGIFY_FUNCTION(Vector_Vector_char, vvc);

#define STRINGIFY_TO_CHAR_VEC(T, val)                                          \
  (staticStringifyBuffer[0] = 0,                                               \
   STRINGIFY_VALUE(T, val, staticStringifyBuffer),                             \
   charVecFromCArray(staticStringifyBuffer))
