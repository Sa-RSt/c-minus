#pragma once

#include "error.h"
#include "semantic.h"
#include "syntactic.h"
#include <cstdio>
#include <stdio.h>

typedef struct GraphWriteError {
  ERROR_TYPE_FIELDS
  // não sei o que colocar aqui por enquanto
} GraphWriteError;

typedef struct SymbolTableWriteError {
  ERROR_TYPE_FIELDS
  // não sei o que colocar aqui por enquanto
} SymbolTableWriteError;

typedef struct AssemblyWriteError {
  ERROR_TYPE_FIELDS
  // não sei o que colocar aqui por enquanto
} AssemblyWriteError;

bool writeGraph(FILE *out, ASTNode graph, GraphWriteError *err);
bool writeSymbolTable(FILE *out, Vector_Symbol table,
                      SymbolTableWriteError *err);
bool writeAssembly(FILE *out, ASTNode graph, AssemblyWriteError *err);
