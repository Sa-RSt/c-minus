#pragma once

#include "char_vector.h"
#include "error.h"
#include "lexer.h"
#include "vector.h"
#include <stdint.h>

typedef enum ASTNodeKind {
  PROGRAM_NODE,
  DECLARATION_LIST_NODE,
  DECLARATION_NODE,
  VAR_DECLARATION_NODE,
  FUN_DECLARATION_NODE,
  TYPE_SPECIFIER_NODE,
  ID_NODE,
  NUM_NODE
  // TODO: ... (seguir de acordo com o Livro ™)
} ASTNodeKind;

typedef enum AttributeType {
  VEC_ATTRIBUTE = 0b1,
  NODE_ATTRIBUTE = 0b11,
  INTEGER_ATTRIBUTE = 0b100,
  FP_NUMBER_ATTRIBUTE = 0b110,
  POINTER_ATTRIBUTE = 0b1000,
  TEXT_ATTRIBUTE = 0b1010,
  STRUCT_ATTRIBUTE = 0b1100
} AttributeType;

typedef struct Attribute Attribute;

HEADER_VECTOR_TYPE(Attribute, Vector_char *)

typedef struct ASTNode {
  Vector_char content;
  ASTNodeKind kind;
  uint32_t line;
  Vector_Attribute attributes;
} ASTNode;

HEADER_VECTOR_TYPE(ASTNode, ASTNodeKind)
HEADER_VECTOR_TYPE(int64_t, int64_t)
HEADER_VECTOR_TYPE(double, double)
HEADER_VECTOR_TYPE(voidp, voidp)
HEADER_VECTOR_TYPE(uint8_t, uint8_t)
HEADER_VECTOR_TYPE(Vector_uint8_t, Vector_uint8_t *)

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
  Vector_uint8_t structure;
  Vector_Vector_uint8_t structures;
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
