#include "semantic.h"
#include "ast_attribute_names.h"
#include "char_vector.h"
#include "error.h"
#include "stringify.h"
#include "syntactic.h"
#include "vector.h"
#include <assert.h>

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

Vector_char scopeSlashRepresentation(const Vector_Vector_char *scope) {
  Vector_char res = vecCreateEmpty_char();
  for (size_t i = 0; i < vecLength_Vector_char(scope); i++) {
    vecExtend_char(&res, vecIndex_Vector_char(scope, i));
    vecPushRight_char(&res, '/');
  }
  if (vecLength_char(&res) > 0) {
    vecPopRight_char(&res); // remove trailing slash
  }
  return res;
}

DECLARE_STRINGIFY_FUNCTION(Symbol, sym) {
  STRINGIFY_PUT("linha ");
  STRINGIFY_PUT_VALUE(uint32_t, sym.line);
  STRINGIFY_PUT(": ");
  STRINGIFY_PUT("escopo ");
  Vector_char repr = scopeSlashRepresentation(&sym.scope);
  STRINGIFY_PUT_VALUE(Vector_char, repr);
  vecFree_char(&repr);
  STRINGIFY_PUT(": ");
  STRINGIFY_PUT_VALUE(DataType, sym.dataType);
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
  Attribute *attr = vecKey_Attribute(&parent->attributes, &NODE__CHILDREN);
  assert(attr->type == (NODE_ATTRIBUTE | VEC_ATTRIBUTE));
  return &attr->value.nodes;
}

static ASTNode *getChild(ASTNode *parent) {
  Vector_ASTNode *children = getChildren(parent);
  assert(vecLength_ASTNode(children) == 1);
  return vecIndex_ASTNode(children, 0);
}

static Vector_Vector_char duplicateScope(const Vector_Vector_char *scope) {
  Vector_Vector_char out =
      vecCreateWithCapacity_Vector_char(vecLength_Vector_char(scope));
  for (size_t i = 0; i < vecLength_Vector_char(scope); i++) {
    vecPushRight_Vector_char(&out,
                             vecDuplicate_char(vecIndex_Vector_char(scope, i)));
  }
  return out;
}

static bool semanticizeFunDeclaration(ASTNode *ast, Vector_Symbol *symTable,
                                      Vector_Vector_char *scope,
                                      SemanticError *err) {
  Vector_char literalInt = charVecFromCArray("int");
  Vector_char literalVoid = charVecFromCArray("void");

  assert(ast->kind == FUN_DECLARATION_NODE);
  // TODO: parei aqui

  vecFree_char(&literalInt);
  vecFree_char(&literalVoid);
  return true;
}
static bool semanticizeVarDeclaration(ASTNode *ast, Vector_Symbol *symTable,
                                      Vector_Vector_char *scope,
                                      SemanticError *err) {
  Vector_char literalInt = charVecFromCArray("int");
  assert(ast->kind == VAR_DECLARATION_NODE);

  Vector_ASTNode *children = getChildren(ast);
  size_t amountOfChildren = vecLength_ASTNode(children);
  assert(amountOfChildren == 2 || amountOfChildren == 3);
  ASTNode *typeSpec = vecKey_ASTNode(children, TYPE_SPECIFIER_NODE);

  Vector_char trim = charVecStripWhitespace(&typeSpec->content);
  if (charVecStrcmp(&trim, &literalInt) != 0) {
    CREATE_ERROR(err, "'int' é o único especificador válido para "
                      "declaração de variável");
    err->sourceLine = typeSpec->line;
    vecFree_char(&trim);
    vecFree_char(&literalInt);
    return false;
  }

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
  sym.line = identifier->line;
  sym.scope = duplicateScope(scope);
  sym.dataType = dt;
  vecPushRight_Symbol(symTable, sym);
  Attribute save;
  save.name = VAR_FUN_DECLARATION__SYMBOL;
  save.type = STRUCT_ATTRIBUTE;
  AttributeValue val;
  val.structure = vecCopiedFromArray_uint8_t((uint8_t *)&sym, sizeof(sym));
  save.value = val;
  vecPushRight_Attribute(&ast->attributes, save);

  vecFree_char(&trim);
  vecFree_char(&literalInt);
  return true;
}

static bool semanticizeDeclaration(ASTNode *ast, Vector_Symbol *symTable,
                                   Vector_Vector_char *scope,
                                   SemanticError *err) {
  assert(ast->kind == DECLARATION_NODE);

  ASTNode *childNode = getChild(ast);
  switch (childNode->kind) {
  case FUN_DECLARATION_NODE:
    return semanticizeFunDeclaration(childNode, symTable, scope, err);
  case VAR_DECLARATION_NODE:
    return semanticizeVarDeclaration(childNode, symTable, scope, err);
  default:
    assert(0);
  }

  return true;
}

static bool semanticizeDeclarationList(ASTNode *ast, Vector_Symbol *symTable,
                                       Vector_Vector_char *scope,
                                       SemanticError *err) {
  assert(ast->kind == DECLARATION_LIST_NODE);
  Vector_ASTNode *declarations = getChildren(ast);
  for (size_t i = 0; i < vecLength_ASTNode(declarations); i++) {
    bool res = semanticizeDeclaration(vecIndex_ASTNode(declarations, i),
                                      symTable, scope, err);
    if (!res) {
      return false;
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
  Vector_Vector_char scope = vecCreateSingle_Vector_char(strGlobal);

  return semanticizeDeclarationList(childNode, &programSymbolTable, &scope,
                                    err);
}
