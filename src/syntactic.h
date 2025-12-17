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

typedef struct ASTNode ASTNode;
HEADER_VECTOR_TYPE(ASTNode, ASTNodeKind)

typedef struct ASTNode {
  Vector_char content;
  ASTNodeKind kind;
  uint32_t line;
  Vector_Attribute attributes;
  Vector_ASTNode children;
} ASTNode;

typedef struct SyntaxError {
  ERROR_TYPE_FIELDS
  ASTNode node;
} SyntaxError;

HEADER_ERROR_TYPE_FUNCTIONS(SyntaxError)

void printASTTree(const ASTNode *node, size_t indent);
bool generateAST(Vector_Token tokens, ASTNode *out, SyntaxError *err);
