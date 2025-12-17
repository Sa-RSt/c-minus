#include "char_vector.h"
#include "stringify.h"
#include "vector.h"
#include <ctype.h>
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>

#define GETSELF(x) x
#define CHARCMP(x, y) (x - y)
DECLARE_VECTOR_TYPE(char, char, GETSELF, CHARCMP)

char *charVecCreateCArray(const Vector_char *str) {
  const size_t len = vecLength_char(str);
  char *c = (char *)malloc(+1);
  for (size_t i = 0; i < len; i++) {
    c[i] = *vecIndex_char(str, i);
  }
  c[len] = 0;
  return c;
}

Vector_char charVecFromCArray(const char *arr) {
  return vecCopiedFromArray_char(arr, strlen(arr));
}

void charVecAppendCArray(Vector_char *dest, const char *src) {
  Vector_char temp = charVecFromCArray(src);
  vecExtend_char(dest, &temp);
  vecFree_char(&temp);
}

Vector_char charVecStripWhitespace(const Vector_char *src) {
  Vector_char ret = vecCreateWithCapacity_char(vecLength_char(src));
  bool middle = false;
  for (size_t i = 0; i < vecLength_char(src); i++) {
    char c = *vecIndex_char(src, i);
    if (!isspace(c) && !middle) {
      vecPushRight_char(&ret, c);
    } else {
      middle = true;
    }
  }

  while (vecLength_char(&ret) > 0 && isspace(vecPopRight_char(&ret))) {
  }

  return ret;
}

static int _lenStrcmp(const char *left, const char *right, size_t left_len,
                      size_t right_len) {
  if (left_len == 0 && right_len == 0) {
    return 0;
  }
  if (left_len == 0) {
    return 1;
  }
  if (right_len == 0) {
    return -1;
  }
  const int diff = *left - *right;
  if (diff) {
    return diff;
  }
  return _lenStrcmp(left + 1, right + 1, left_len - 1, right_len - 1);
}

int charVecStrcmp(const Vector_char *left, const Vector_char *right) {
  const char *leftv, *rightv;
  size_t left_len, right_len;
  leftv = vecBorrow_char(left, &left_len);
  rightv = vecBorrow_char(right, &right_len);
  return _lenStrcmp(leftv, rightv, left_len, right_len);
}

DECLARE_STRINGIFY_FUNCTION(Vector_char, str) {
  char *p = charVecCreateCArray(&str);
  STRINGIFY_PUT(p);
  free(p);
}

#define GETSELFPTR(x) (&x)
#define DIFF(x, y) (x - y)
DECLARE_VECTOR_TYPE(Vector_char, Vector_char *, GETSELFPTR, charVecStrcmp)
DECLARE_VECTOR_STRINGIFY_FUNCTION(Vector_char)
