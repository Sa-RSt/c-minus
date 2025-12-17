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

typedef struct ScopeEntry {
  Vector_char name;
  Vector_Vector_char symbolNames;
} ScopeEntry;
HEADER_VECTOR_TYPE(ScopeEntry, Vector_char *)
DECLARE_STRINGIFY_FUNCTION(ScopeEntry, se);
DECLARE_STRINGIFY_FUNCTION(Vector_ScopeEntry, vse);

typedef struct Symbol Symbol;
HEADER_VECTOR_TYPE(Symbol, Vector_char *)

typedef enum SymbolKind { VAR_SYMBOL, FUN_SYMBOL } SymbolKind;
DECLARE_STRINGIFY_FUNCTION(SymbolKind, sk);

typedef struct FunctionSymbol {
  DataTypeKind returnDataTypeKind;
  Vector_Symbol params;
} FunctionSymbol;

typedef struct VarSymbol {
  DataType dataType;
} VarSymbol;

typedef union SymbolSpec {
  FunctionSymbol fun;
  VarSymbol var;
} SymbolSpec;

typedef struct Symbol {
  Vector_char name;
  uint32_t line;
  ASTNode *node;
  SymbolKind kind;
  SymbolSpec spec;
  Vector_ScopeEntry scope;
  bool isParameter;
} Symbol;

DECLARE_STRINGIFY_FUNCTION(Symbol, sym);

HEADER_VECTOR_TYPE(Vector_Symbol, voidp)

typedef struct SemanticError {
  ERROR_TYPE_FIELDS
  ASTNode node;
  size_t sourceLine;
} SemanticError;

HEADER_ERROR_TYPE_FUNCTIONS(SemanticError)

bool semanticize(ASTNode *ast, Vector_Vector_Symbol *out_symbol_tables,
                 SemanticError *err, void *outCode);
