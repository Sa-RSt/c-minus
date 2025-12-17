#pragma once

#include "char_vector.h"
#include "error.h"
#include "lexer.h"
#include "stringify.h"
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
  NUM_NODE,
  PARAMS_NODE,
  PARAM_LIST_NODE,
  PARAM_NODE,
  EMPTY_SQR_BRACKETS_NODE, // corresponde a "[]", necessário para declarar
                           // parâmetros
  COMPOUND_STMT_NODE,
  LOCAL_DECLARATIONS_NODE,
  STATEMENT_LIST_NODE,
  STATEMENT_NODE,
  EXPRESSION_STMT_NODE,
  SELECTION_STMT_NODE,
  ITERATION_STMT_NODE,
  RETURN_STMT_NODE,
  EXPRESSION_NODE,
  VAR_NODE,
  SIMPLE_EXPRESSION_NODE,
  ADDITIVE_EXPRESSION_NODE,
  RELOP_NODE,
  TERM_NODE,
  ADDOP_NODE,
  MULOP_NODE,
  FACTOR_NODE,
  CALL_NODE,
  ARGS_NODE,
  ARG_LIST_NODE,
  EMPTY_NODE
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

HEADER_VECTOR_TYPE(Attribute, Vector_char)
typedef struct ASTNode ASTNode;
HEADER_VECTOR_TYPE(ASTNode, ASTNodeKind)

typedef struct ASTNode {
  Vector_char content;
  ASTNodeKind kind;
  uint32_t line;
  Vector_Attribute attributes;
  Vector_ASTNode children;
} ASTNode;

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

DECLARE_STRINGIFY_FUNCTION(ASTNode, n);

void printASTTree(const ASTNode *node, int indent);
bool generateAST(Vector_Token tokens, ASTNode *out, SyntaxError *err);
