#pragma once

#include "char_vector.h"
#include "error.h"
#include "stringify.h"
#include "syntactic.h"
#include "vector.h"
#include <stdint.h>
#include <string.h>

typedef enum DataTypeKind {
  INT_ARRAY_TYPE,
  INT_VALUE_TYPE,
  VOID_TYPE
} DataTypeKind;
DECLARE_STRINGIFY_FUNCTION(DataTypeKind, dtk);

typedef struct DataType {
  DataTypeKind kind;
  size_t length;
} DataType;

DECLARE_STRINGIFY_FUNCTION(DataType, dt);

typedef struct Symbol {
  Vector_char name;
  DataType dataType;
  Vector_Vector_char scope; // (ex.: ["global", "main()", "while#1"])
  uint32_t line;
} Symbol;

DECLARE_STRINGIFY_FUNCTION(Symbol, sym);

HEADER_VECTOR_TYPE(Symbol, Vector_char *)
HEADER_VECTOR_TYPE(Vector_Symbol, voidp)

typedef struct SemanticError {
  ERROR_TYPE_FIELDS
  ASTNode node;
  size_t sourceLine;
} SemanticError;

HEADER_ERROR_TYPE_FUNCTIONS(SemanticError)

bool semanticize(ASTNode *ast, Vector_Vector_Symbol *out_symbol_tables,
                 SemanticError *err);
