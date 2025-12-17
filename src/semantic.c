#include "semantic.h"
#include "ast_attribute_names.h"
#include "char_vector.h"
#include "error.h"
#include "stringify.h"
#include "syntactic.h"
#include "vector.h"
#include <assert.h>
#include <stdbool.h>

DECLARE_STRINGIFY_FUNCTION(DataTypeKind, dtk) {
  switch (dtk) {
  case INT_ARRAY_TYPE:
    STRINGIFY_PUT("int_array");
    break;
  case INT_VALUE_TYPE:
    STRINGIFY_PUT("int");
    break;
  case VOID_TYPE:
    STRINGIFY_PUT("void");
    break;
  default:
    STRINGIFY_PUT("desconhecido: ");
    STRINGIFY_PUT_VALUE(int32_t, (int32_t)dtk);
    break;
  }
}

DECLARE_STRINGIFY_FUNCTION(DataType, dt) {
  STRINGIFY_PUT_VALUE(DataTypeKind, dt.kind);
  if (dt.kind == INT_ARRAY_TYPE) {
    STRINGIFY_PUT("[");
    STRINGIFY_PUT_VALUE(size_t, dt.length);
    STRINGIFY_PUT("]");
  }
}

#define DATA_TYPE_KIND_GETTER(x) (x.kind)
#define DATA_TYPE_KIND_COMPARER(x, y) (x - y)

Vector_char scopeSlashRepresentation(const Vector_ScopeEntry *scope) {
  Vector_char res = vecCreateEmpty_char();
  for (size_t i = 0; i < vecLength_ScopeEntry(scope); i++) {
    vecExtend_char(&res, &vecIndex_ScopeEntry(scope, i)->name);
    vecPushRight_char(&res, '/');
  }
  if (vecLength_char(&res) > 0) {
    vecPopRight_char(&res); // remove barra no final
  }
  return res;
}

#define SE_NAME_GETTER(x) (&x.name)
DECLARE_VECTOR_TYPE(ScopeEntry, Vector_char *, SE_NAME_GETTER, charVecStrcmp)

DECLARE_STRINGIFY_FUNCTION(ScopeEntry, se) {
  STRINGIFY_PUT("ScopeEntry(name = ");
  STRINGIFY_PUT_VALUE(Vector_char, se.name);
  STRINGIFY_PUT("; symbolNames = ");
  STRINGIFY_PUT_VALUE(Vector_Vector_char, se.symbolNames);
  STRINGIFY_PUT(")");
}

DECLARE_VECTOR_STRINGIFY_FUNCTION(ScopeEntry);

DECLARE_STRINGIFY_FUNCTION(SymbolKind, sk) {
  switch (sk) {
  case VAR_SYMBOL:
    STRINGIFY_PUT("var");
    break;
  case FUN_SYMBOL:
    STRINGIFY_PUT("fun");
    break;
  default:
    STRINGIFY_PUT("SymbolKind desconhecido: ");
    STRINGIFY_PUT_VALUE(int32_t, (int32_t)sk);
    break;
  }
}

DECLARE_STRINGIFY_FUNCTION(Symbol, sym) {
  if (sym.isParameter) {
    STRINGIFY_PUT("param ");
  }
  STRINGIFY_PUT_VALUE(SymbolKind, sym.kind);
  STRINGIFY_PUT(" na linha ");
  STRINGIFY_PUT_VALUE(uint32_t, sym.line);
  if (sym.kind == VAR_SYMBOL) {
    STRINGIFY_PUT(" com escopo ");
    Vector_char repr = scopeSlashRepresentation(&sym.scope);
    STRINGIFY_PUT_VALUE(Vector_char, repr);
    vecFree_char(&repr);
    STRINGIFY_PUT(": ");
    STRINGIFY_PUT_VALUE(DataType, sym.spec.var.dataType);
  } else if (sym.kind == FUN_SYMBOL) {
    STRINGIFY_PUT(" retornando ");
    STRINGIFY_PUT_VALUE(DataTypeKind, sym.spec.fun.returnDataTypeKind);
  }
  STRINGIFY_PUT(" ");
  STRINGIFY_PUT_VALUE(Vector_char, sym.name);
}

#define SYMBOL_NAME_GETTER(x) (&x.name)
#define NULL_GETTER(x) (NULL)
#define NULL_COMPARER(x, y) 1
DECLARE_VECTOR_TYPE(Symbol, Vector_char *, SYMBOL_NAME_GETTER, charVecStrcmp)
DECLARE_VECTOR_STRINGIFY_FUNCTION(Symbol)
DECLARE_VECTOR_TYPE(Vector_Symbol, voidp, NULL_GETTER, NULL_COMPARER)

DECLARE_ERROR_TYPE_FUNCTIONS(SemanticError)

static Vector_ASTNode *getChildren(ASTNode *parent) {
  return &parent->children;
}

static ASTNode *getChild(ASTNode *parent) {
  Vector_ASTNode *children = getChildren(parent);
  assert(vecLength_ASTNode(children) == 1);
  return vecIndex_ASTNode(children, 0);
}

static Vector_Vector_char
duplicateVectorOfStrings(const Vector_Vector_char *vec) {
  size_t len = vecLength_Vector_char(vec);
  Vector_Vector_char out = vecCreateWithCapacity_Vector_char(len);
  for (size_t i = 0; i < len; i++) {
    vecPushRight_Vector_char(&out,
                             vecDuplicate_char(vecIndex_Vector_char(vec, i)));
  }
  return out;
}

static Vector_ScopeEntry duplicateScope(const Vector_ScopeEntry *scope) {
  size_t len = vecLength_ScopeEntry(scope);
  Vector_ScopeEntry out = vecCreateWithCapacity_ScopeEntry(len);
  for (size_t i = 0; i < len; i++) {
    ScopeEntry *entry = vecIndex_ScopeEntry(scope, i);
    ScopeEntry copy;
    copy.name = vecDuplicate_char(&entry->name);
    copy.symbolNames = duplicateVectorOfStrings(&entry->symbolNames);
    vecPushRight_ScopeEntry(&out, copy);
  }
  return out;
}

typedef enum TypeSpecifier { INT_TYPE_SPEC, VOID_TYPE_SPEC } TypeSpecifier;

static bool scopeEquals(const Vector_ScopeEntry *a,
                        const Vector_ScopeEntry *b) {
  size_t len = vecLength_ScopeEntry(a);
  if (len != vecLength_ScopeEntry(b)) {
    return false;
  }
  for (size_t i = 0; i < len; i++) {
    ScopeEntry *lhs = vecIndex_ScopeEntry(a, i);
    ScopeEntry *rhs = vecIndex_ScopeEntry(b, i);
    if (charVecStrcmp(&lhs->name, &rhs->name) != 0) {
      return false;
    }
  }
  return true;
}

static void scopePush(Vector_ScopeEntry *scope, const Vector_char *name) {
  ScopeEntry entry;
  entry.name = vecDuplicate_char(name);
  entry.symbolNames = vecCreateEmpty_Vector_char();
  vecPushRight_ScopeEntry(scope, entry);
}

static void scopePop(Vector_ScopeEntry *scope, Vector_Symbol *symTable) {
  ScopeEntry entry = vecPopRight_ScopeEntry(scope);
  for (size_t i = 0; i < vecLength_Vector_char(&entry.symbolNames); i++) {
    Vector_char *symName = vecIndex_Vector_char(&entry.symbolNames, i);
    vecRemoveRight_Symbol(symTable, symName, NULL);
    vecFree_char(symName);
  }
  vecFree_char(&entry.name);
  vecFree_Vector_char(&entry.symbolNames);
}

static bool symTablePush(Vector_ScopeEntry *scope, Vector_Symbol *symTable,
                         Symbol *sym, SemanticError *err) {
  size_t scopeLen = vecLength_ScopeEntry(scope);
  ScopeEntry *last = vecIndex_ScopeEntry(scope, scopeLen - 1);
  Vector_char *existing = vecLookup_Vector_char(&last->symbolNames, &sym->name);
  sym->scope = duplicateScope(scope);
  if (existing != NULL) {
    Vector_char errMsg =
        charVecFromCArray("símbolo declarado duas vezes no mesmo escopo: ");
    Vector_char symRepr = STRINGIFY_TO_CHAR_VEC(Symbol, *sym);
    vecExtend_char(&errMsg, &symRepr);
    CREATE_ERROR(err, charVecCreateCArray(&errMsg)); // memory leak
    vecFree_char(&symRepr);
    return false;
  }
  vecPushRight_Vector_char(&last->symbolNames, vecDuplicate_char(&sym->name));
  vecPushRight_Symbol(symTable, *sym);
  return true;
}

static bool parseTypeSpecNode(ASTNode *ast, TypeSpecifier *out) {
  assert(ast->kind == TYPE_SPECIFIER_NODE);
  Vector_char literalInt = charVecFromCArray("int");
  Vector_char literalVoid = charVecFromCArray("void");
  bool ok = false;

  if (charVecStrcmp(&ast->content, &literalVoid) == 0) {
    *out = VOID_TYPE_SPEC;
    ok = true;
  } else if (charVecStrcmp(&ast->content, &literalInt) == 0) {
    *out = INT_TYPE_SPEC;
    ok = true;
  }

  vecFree_char(&literalInt);
  vecFree_char(&literalVoid);
  return ok;
}

static bool requireIntTypeSpec(ASTNode *typeSpec, SemanticError *err) {

  TypeSpecifier ts;
  assert(parseTypeSpecNode(typeSpec, &ts));
  if (ts == INT_TYPE_SPEC) {
    return true;
  }
  CREATE_ERROR(err, "'int' é o único especificador válido para nesse contexto");
  err->sourceLine = typeSpec->line;
  return false;
}

static bool semanticizeVarDeclaration(ASTNode *ast, Vector_Symbol *symTable,
                                      Vector_ScopeEntry *scope,
                                      SemanticError *err);

static bool semanticizeCompoundStmt(ASTNode *ast, Vector_Symbol *symTable,
                                    SemanticError *err,
                                    Vector_ScopeEntry *scope);

static bool semanticizeLocalDeclarations(ASTNode *ast, Vector_Symbol *symTable,
                                         SemanticError *err,
                                         Vector_ScopeEntry *scope) {
  Vector_ASTNode *children = getChildren(ast);
  size_t len = vecLength_ASTNode(children);
  switch (ast->kind) {
  case EMPTY_NODE:
    return true;
  case LOCAL_DECLARATIONS_NODE:
    bool ok = true;
    for (size_t i = 0; i < len; i++) {
      ok = semanticizeLocalDeclarations(vecIndex_ASTNode(children, i), symTable,
                                        err, scope);
    }
    return ok;
  case VAR_DECLARATION_NODE:
    return semanticizeVarDeclaration(ast, symTable, scope, err);
  default:
    assert(0); // erro do analisador sintático
  }
}

static bool semanticizeSelectionStatement(ASTNode *ast, Vector_Symbol *symTable,
                                          SemanticError *err,
                                          Vector_ScopeEntry *scope) {}

static bool semanticizeExpression(ASTNode *ast, Vector_Symbol *symTable,
                                  SemanticError *err,
                                  Vector_ScopeEntry *scope) {
  Vector_ASTNode *children = getChildren(ast);
  ASTNode *simpleExpression =
      vecLookup_ASTNode(children, SIMPLE_EXPRESSION_NODE);
}

static bool semanticizeExpressionStatement(ASTNode *ast,
                                           Vector_Symbol *symTable,
                                           SemanticError *err,
                                           Vector_ScopeEntry *scope) {
  assert(ast->kind == EXPRESSION_STMT_NODE);
  Vector_ASTNode *children = getChildren(ast);
  ASTNode *expression = vecLookup_ASTNode(children, EXPRESSION_NODE);
  if (expression == NULL) {
    return true;
  }
  return semanticizeExpression(ast, symTable, err, scope);
}

static bool semanticizeIterationStatement(ASTNode *ast, Vector_Symbol *symTable,
                                          SemanticError *err,
                                          Vector_ScopeEntry *scope) {}

static bool semanticizeReturnStatement(ASTNode *ast, Vector_Symbol *symTable,
                                       SemanticError *err,
                                       Vector_ScopeEntry *scope) {}

static bool semanticizeStatement(ASTNode *ast, Vector_Symbol *symTable,
                                 SemanticError *err, Vector_ScopeEntry *scope) {
  assert(ast->kind == STATEMENT_NODE);
  ASTNode *child = getChild(ast);
  switch (child->kind) {
  case EXPRESSION_STMT_NODE:
    return semanticizeExpressionStatement(child, symTable, err, scope);
  case SELECTION_STMT_NODE:
    return semanticizeSelectionStatement(child, symTable, err, scope);
  case ITERATION_STMT_NODE:
    return semanticizeIterationStatement(child, symTable, err, scope);
  case RETURN_STMT_NODE:
    return semanticizeReturnStatement(child, symTable, err, scope);
  case COMPOUND_STMT_NODE:
    return semanticizeCompoundStmt(child, symTable, err, scope);
  default:
    assert(0); // erro do analisador sintático
  }
}

static bool semanticizeStatementList(ASTNode *ast, Vector_Symbol *symTable,
                                     SemanticError *err,
                                     Vector_ScopeEntry *scope) {

  Vector_ASTNode *children = getChildren(ast);
  size_t len = vecLength_ASTNode(children);
  switch (ast->kind) {
  case EMPTY_NODE:
    return true;
  case LOCAL_DECLARATIONS_NODE:
    bool ok = true;
    for (size_t i = 0; i < len; i++) {
      ok = semanticizeStatementList(vecIndex_ASTNode(children, i), symTable,
                                    err, scope);
      if (!ok)
        return false;
    }
    return true;
  case VAR_DECLARATION_NODE:
    return semanticizeStatement(ast, symTable, err, scope);
  default:
    assert(0); // erro do analisador sintático
  }
}

static bool semanticizeCompoundStmt(ASTNode *ast, Vector_Symbol *symTable,
                                    SemanticError *err,
                                    Vector_ScopeEntry *scope) {
  assert(ast->kind == COMPOUND_STMT_NODE);
  Vector_ASTNode *children = getChildren(ast);
  ASTNode *localDeclarations =
      vecKey_ASTNode(children, LOCAL_DECLARATIONS_NODE);
  ASTNode *statementList = vecKey_ASTNode(children, STATEMENT_LIST_NODE);
  semanticizeLocalDeclarations(localDeclarations, symTable, err, scope);
}

static bool semanticizeParams(ASTNode *ast, Vector_Symbol *out,
                              SemanticError *err, Vector_ScopeEntry *scope) {
  Vector_ASTNode *children = getChildren(ast);
  size_t len = vecLength_ASTNode(children);
  switch (ast->kind) {
  case PARAMS_NODE:
    assert(len == 0 || len == 1);
    bool ok = true;
    if (len == 1) {
      ok = semanticizeParams(getChild(ast), out, err, scope);
    }
    return ok;
  case PARAM_LIST_NODE:
    assert(len == 1 || len == 2);
    for (size_t i = 0; i < len; i++) {
      ASTNode *child = vecIndex_ASTNode(children, i);
      if (!semanticizeParams(child, out, err, scope))
        return false;
    }
    return true;
  case PARAM_NODE:
    assert(len == 2 || len == 3);

    DataType dt;
    dt.length = 0;
    ASTNode *typeSpec = vecKey_ASTNode(children, TYPE_SPECIFIER_NODE);
    ASTNode *identifier = vecKey_ASTNode(children, ID_NODE);
    ASTNode *emptyBrackets =
        vecLookup_ASTNode(children, EMPTY_SQR_BRACKETS_NODE);
    Symbol sym;
    sym.kind = VAR_SYMBOL;
    sym.isParameter = true;
    sym.line = ast->line;
    sym.node = ast;
    sym.name = vecDuplicate_char(&identifier->content);
    if (!requireIntTypeSpec(typeSpec, err))
      return false;
    if (emptyBrackets == NULL) {
      dt.kind = INT_VALUE_TYPE;
    } else {
      dt.kind = INT_ARRAY_TYPE;
    }
    sym.spec.var.dataType = dt;
    vecPushRight_Symbol(out, sym);
    return true;
  default:
    assert(0); // erro do analisador sintático
  }
}

static bool semanticizeFunDeclaration(ASTNode *ast, Vector_Symbol *symTable,
                                      Vector_ScopeEntry *scope,
                                      SemanticError *err) {

  assert(ast->kind == FUN_DECLARATION_NODE);

  Vector_ASTNode *children = getChildren(ast);
  assert(vecLength_ASTNode(children) == 4);
  ASTNode *typeSpec = vecKey_ASTNode(children, TYPE_SPECIFIER_NODE);
  ASTNode *identifier = vecKey_ASTNode(children, ID_NODE);
  ASTNode *params = vecKey_ASTNode(children, PARAMS_NODE);
  ASTNode *block = vecKey_ASTNode(children, COMPOUND_STMT_NODE);

  Symbol sym;
  sym.kind = FUN_SYMBOL;
  sym.line = ast->line;
  sym.name = vecDuplicate_char(&identifier->content);
  sym.node = ast;
  sym.isParameter = false;
  TypeSpecifier ts;
  assert(parseTypeSpecNode(typeSpec, &ts));
  if (ts == INT_TYPE_SPEC) {
    sym.spec.fun.returnDataTypeKind = INT_VALUE_TYPE;
  } else if (ts == VOID_TYPE_SPEC) {
    sym.spec.fun.returnDataTypeKind = VOID_TYPE;
  }

  Vector_char scopeName = vecDuplicate_char(&sym.name);
  charVecAppendCArray(&scopeName, "()");
  scopePush(scope, &scopeName);
  vecFree_char(&scopeName);

  Vector_Symbol parsedParams = vecCreateEmpty_Symbol();
  if (!semanticizeParams(params, &parsedParams, err, scope))
    return false;
  sym.spec.fun.params = parsedParams;

  if (!symTablePush(scope, symTable, &sym, err)) // necessário para recursão
    return false;

  for (size_t i = 0; i < vecLength_Symbol(&parsedParams); i++) {
    if (!symTablePush(scope, symTable, vecIndex_Symbol(&parsedParams, i), err))
      return false;
  }

  vecFree_Symbol(&parsedParams);
  scopePop(scope, symTable);

  return true;
}

static bool semanticizeVarDeclaration(ASTNode *ast, Vector_Symbol *symTable,
                                      Vector_ScopeEntry *scope,
                                      SemanticError *err) {
  assert(ast->kind == VAR_DECLARATION_NODE);

  Vector_ASTNode *children = getChildren(ast);
  size_t amountOfChildren = vecLength_ASTNode(children);
  assert(amountOfChildren == 2 || amountOfChildren == 3);
  ASTNode *typeSpec = vecKey_ASTNode(children, TYPE_SPECIFIER_NODE);

  if (!requireIntTypeSpec(typeSpec, err))
    return false;

  ASTNode *identifier = vecKey_ASTNode(children, ID_NODE);
  ASTNode *maybeSize = vecLookup_ASTNode(children, NUM_NODE);
  DataType dt;
  if (maybeSize == NULL) {
    dt.kind = INT_VALUE_TYPE;
  } else {
    dt.kind = INT_ARRAY_TYPE;
    assert(sscanf(vecBorrow_char(&maybeSize->content, NULL), "%lu",
                  &dt.length) == 1);
  }
  Symbol sym;
  sym.name = vecDuplicate_char(&identifier->content);
  sym.line = ast->line;
  sym.spec.var.dataType = dt;
  sym.node = ast;
  sym.kind = VAR_SYMBOL;
  sym.isParameter = false;
  symTablePush(scope, symTable, &sym, err);
  Attribute save;
  save.name = VAR_FUN_DECLARATION__SYMBOL;
  save.type = STRUCT_ATTRIBUTE;
  AttributeValue val;
  val.structure = vecCopiedFromArray_uint8_t((uint8_t *)&sym, sizeof(sym));
  save.value = val;
  vecPushRight_Attribute(&ast->attributes, save);

  return true;
}

static bool semanticizeDeclaration(ASTNode *ast, Vector_Symbol *symTable,
                                   Vector_ScopeEntry *scope,
                                   SemanticError *err) {
  assert(ast->kind == DECLARATION_NODE);

  ASTNode *childNode = getChild(ast);
  assert(childNode->kind == FUN_DECLARATION_NODE ||
         childNode->kind == VAR_DECLARATION_NODE);
  if (childNode->kind == FUN_DECLARATION_NODE) {
    return semanticizeFunDeclaration(childNode, symTable, scope, err);
  } else if (childNode->kind == VAR_DECLARATION_NODE) {
    return semanticizeVarDeclaration(childNode, symTable, scope, err);
  }

  return true;
}

static bool semanticizeDeclarationList(ASTNode *ast, Vector_Symbol *symTable,
                                       Vector_ScopeEntry *scope,
                                       SemanticError *err) {
  assert(ast->kind == DECLARATION_LIST_NODE);
  Vector_ASTNode *declarations = getChildren(ast);
  size_t len = vecLength_ASTNode(declarations);
  assert(len == 2 || len == 1);
  for (size_t i = 0; i < len; i++) {
    ASTNode *child = vecIndex_ASTNode(declarations, i);
    assert(child->kind == DECLARATION_LIST_NODE ||
           child->kind == DECLARATION_NODE);
    if (child->kind == DECLARATION_LIST_NODE) {
      bool res = semanticizeDeclarationList(child, symTable, scope, err);
      if (!res) {
        return false;
      }
    } else if (child->kind == DECLARATION_NODE) {
      bool res = semanticizeDeclaration(child, symTable, scope, err);
      if (!res) {
        return false;
      }
    }
  }

  return true;
}

bool semanticize(ASTNode *ast, Vector_Vector_Symbol *outSymbolTables,
                 SemanticError *err) {
  assert(ast->kind == PROGRAM_NODE);
  ASTNode *childNode = getChild(ast);
  Vector_Symbol programSymbolTable = vecCreateEmpty_Symbol();
  Vector_char strGlobal = charVecFromCArray("global");
  Vector_ScopeEntry scope = vecCreateEmpty_ScopeEntry();
  scopePush(&scope, &strGlobal);
  vecFree_char(&strGlobal);

  return semanticizeDeclarationList(childNode, &programSymbolTable, &scope,
                                    err);
}
