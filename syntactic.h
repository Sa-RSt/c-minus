#pragma once

#include "char_vector.h"
#include "error.h"
#include "lexer.h"
#include "vector.h"
#include <stdint.h>

enum ASTNodeKind {
  PROGRAM_NODE,
  DECLARATION_LIST_NODE,
  DECLARATION_NODE
  // TODO: ... (seguir de acordo com o Livro ™)
};

enum AttributeType {
  VEC_ATTRIBUTE = 0b1,
  NODE_ATTRIBUTE = 0b11,
  INTEGER_ATTRIBUTE = 0b100,
  FP_NUMBER_ATTRIBUTE = 0b110,
  POINTER_ATTRIBUTE = 0b1000,
  TEXT_ATTRIBUTE = 0b1010
};

typedef struct Attribute Attribute;

HEADER_VECTOR_TYPE(Attribute)

struct ASTNode {
  Vector_char content;
  ASTNodeKind kind;
  uint32_t line;
  Vector_Attribute attributes;
};

HEADER_VECTOR_TYPE(ASTNode)
HEADER_VECTOR_TYPE(int64_t)
HEADER_VECTOR_TYPE(double)
HEADER_VECTOR_TYPE(voidp)

typedef union AttributeValue {
  Vector_ASTNode nodes;
  int64_t integer;
  Vector_int64_t integers;
  double fpnumber;
  Vector_double fpnumbers;
  void *pointer;
  Vector_voidp pointers;
  Vector_char text;
  Vector_Vector_char texts;
} AttributeValue;

typedef struct Attribute {
  Vector_char name;
  AttributeValue value;
  AttributeType type;
} Attribute;

typedef struct SyntaxError {
  ERROR_TYPE_FIELDS
  ASTNode node;
} SyntaxError;

HEADER_ERROR_TYPE_FUNCTIONS(SyntaxError)

bool generateAST(Vector_Token tokens, ASTNode *out, SyntaxError *err);
