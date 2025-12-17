#include "semantic.h"
#include "char_vector.h"
#include "codegen.h"
#include "error.h"
#include "stringify.h"
#include "syntactic.h"
#include "vector.h"
#include <assert.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

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

static void setSemanticError(SemanticError *err, Vector_char *msg,
                             uint32_t line) {
  CREATE_ERROR(err, charVecCreateCArray(msg));
  err->sourceLine = line;
}

static void setSemanticErrorPrefix(SemanticError *err, char *prefix,
                                   Vector_char *suffix, uint32_t line) {
  Vector_char msg = charVecFromCArray(prefix);
  vecExtend_char(&msg, suffix);
  setSemanticError(err, &msg, line);
}

static void setSemanticErrorLiteral(SemanticError *err, char *literal,
                                    uint32_t line) {
  Vector_char msg = charVecFromCArray(literal);
  setSemanticError(err, &msg, line);
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
  }
}

static bool symTablePush(Vector_ScopeEntry *scope, Vector_Symbol *symTable,
                         Symbol *sym, SemanticError *err) {
  size_t scopeLen = vecLength_ScopeEntry(scope);
  ScopeEntry *last = vecIndex_ScopeEntry(scope, scopeLen - 1);
  Vector_char *existing = vecLookup_Vector_char(&last->symbolNames, &sym->name);
  sym->scope = duplicateScope(scope);
  if (existing != NULL) {
    setSemanticErrorPrefix(
        err,
        "símbolo declarado mais do que uma vez no mesmo escopo: ", &sym->name,
        sym->line);
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

  return ok;
}

static bool requireIntTypeSpec(ASTNode *typeSpec, SemanticError *err) {

  TypeSpecifier ts;
  assert(parseTypeSpecNode(typeSpec, &ts));
  if (ts == INT_TYPE_SPEC) {
    return true;
  }
  setSemanticErrorLiteral(
      err, "'int' é o único especificador válido para esse contexto",
      typeSpec->line);
  return false;
}

static bool isExpressionType(ASTNodeKind);

static ASTNode *findExpressionNodeLeft(Vector_ASTNode *children) {
  size_t len = vecLength_ASTNode(children);
  for (size_t i = 0; i < len; i++) {
    if (isExpressionType(vecIndex_ASTNode(children, i)->kind)) {
      return vecIndex_ASTNode(children, i);
    }
  }
  return NULL;
}

static ASTNode *findExpressionNodeRight(Vector_ASTNode *children) {
  size_t len = vecLength_ASTNode(children);
  for (int64_t i = (int64_t)len - 1; i >= 0; i--) {
    if (isExpressionType(vecIndex_ASTNode(children, (size_t)i)->kind)) {
      return vecIndex_ASTNode(children, (size_t)i);
    }
  }
  return NULL;
}
static bool semanticizeExpressionStatement(ASTNode *ast,
                                           Vector_Symbol *symTable,
                                           SemanticError *err,
                                           Vector_ScopeEntry *scope,
                                           Codegen *cg);

static bool semanticizeStatement(ASTNode *ast, Vector_Symbol *symTable,
                                 SemanticError *err, Vector_ScopeEntry *scope,
                                 Codegen *cgOut);

static bool semanticizeVarDeclaration(ASTNode *ast, Vector_Symbol *symTable,
                                      Vector_ScopeEntry *scope,
                                      SemanticError *err);

static bool semanticizeExpression(ASTNode *ast, Vector_Symbol *symTable,
                                  SemanticError *err, Vector_ScopeEntry *scope,
                                  Codegen *outCg);

static bool semanticizeCompoundStmt(ASTNode *ast, Vector_Symbol *symTable,
                                    SemanticError *err,
                                    Vector_ScopeEntry *scope, Codegen *outCg);

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

static bool mustGetSymbol(Vector_Symbol *symTable, Vector_char *name,
                          Symbol *out, SemanticError *err, uint32_t line) {
  Symbol *search = vecLookup_Symbol(symTable, name);
  if (search == NULL) {
    setSemanticErrorPrefix(err, "símbolo não encontrado: ", name, line);
    return false;
  }
  *out = *search;
  return true;
}

static void fakeVecExtend_Instruction(Vector_Instruction *dst,
                                      Vector_Instruction *src) {
  vecExtend_Instruction(dst, src);
}

static void fakeVecPushRight_Instruction(Vector_Instruction *dst,
                                         Instruction item) {
  vecPushRight_Instruction(dst, item);
}

static bool semanticizeSelectionStatement(ASTNode *ast, Vector_Symbol *symTable,
                                          SemanticError *err,
                                          Vector_ScopeEntry *scope,
                                          Codegen *cgOut) {
  Vector_ASTNode *children = getChildren(ast);
  ASTNode *conditionExpression = findExpressionNodeLeft(children);
  ASTNode *statementIf = vecSearchLeft_ASTNode(children, COMPOUND_STMT_NODE);
  ASTNode *statementElse = vecSearchRight_ASTNode(children, COMPOUND_STMT_NODE);
  assert(statementIf != NULL);
  if (statementIf == statementElse) {
    statementElse = NULL;
  }
  Codegen conditionCg = createCodegenObj();
  if (!semanticizeExpression(conditionExpression, symTable, err, scope,
                             &conditionCg))
    return false;
  if (conditionCg.resultType != INT_VALUE_TYPE) {
    setSemanticErrorPrefix(
        err, "condição do 'if' exige tipo int: ", &conditionExpression->content,
        conditionExpression->line);
    return false;
  }
  *cgOut = createCodegenObj();
  fakeVecExtend_Instruction(&cgOut->instructions, &conditionCg.instructions);
  Register *ifNotLabelReg, *endifLabelReg;
  Instruction ifNotLabelInst = createLabel(&ifNotLabelReg);
  Instruction endifLabelInst = createArithmeticInstruction(
      ifNotLabelReg, ifNotLabelReg, ADD_IOP, ifNotLabelReg);
  if (statementElse != NULL) {
    endifLabelInst = createLabel(&endifLabelReg);
  }
  fakeVecPushRight_Instruction(
      &cgOut->instructions,
      createIfNotInstruction(conditionCg.result, ifNotLabelReg));
  Codegen statementIfCg = createCodegenObj();
  if (!semanticizeStatement(statementIf, symTable, err, scope, &statementIfCg))
    return false;
  fakeVecExtend_Instruction(&cgOut->instructions, &statementIfCg.instructions);
  if (statementElse != NULL) {
    fakeVecPushRight_Instruction(&cgOut->instructions,
                                 createJumpInstruction(endifLabelReg));
  }
  fakeVecPushRight_Instruction(&cgOut->instructions, ifNotLabelInst);
  if (statementElse != NULL) {
    Codegen statementElseCg = createCodegenObj();
    if (!semanticizeStatement(statementElse, symTable, err, scope,
                              &statementElseCg))
      return false;
    fakeVecExtend_Instruction(&cgOut->instructions,
                              &statementElseCg.instructions);
    fakeVecPushRight_Instruction(&cgOut->instructions, endifLabelInst);
  }
  cgOut->result = NULL;
  cgOut->resultType = VOID_TYPE;
  return true;
}

static bool semanticizeArgs(ASTNode *ast, Vector_Symbol *symTable,
                            SemanticError *err, Vector_ScopeEntry *scope,
                            Vector_Codegen *outArgExprs) {
  assert(ast->kind == ARGS_NODE || ast->kind == ARG_LIST_NODE ||
         ast->kind == EMPTY_NODE || isExpressionType(ast->kind));
  if (ast->kind == EMPTY_NODE) {
    return true;
  }
  if (ast->kind == ARGS_NODE || ast->kind == ARG_LIST_NODE) {
    Vector_ASTNode *children = getChildren(ast);
    for (size_t i = 0; i < vecLength_ASTNode(children); i++) {
      if (!semanticizeArgs(vecIndex_ASTNode(children, i), symTable, err, scope,
                           outArgExprs))
        return false;
    }
    return true;
  }
  if (isExpressionType(ast->kind)) {
    Codegen cg = createCodegenObj();
    if (!semanticizeExpression(ast, symTable, err, scope, &cg))
      return false;
    vecPushRight_Codegen(outArgExprs, cg);
    return true;
  }
  assert(0);
}

static bool semanticizeCall(ASTNode *ast, Vector_Symbol *symTable,
                            SemanticError *err, Vector_ScopeEntry *scope,
                            Codegen *outCg) {
  assert(ast->kind == CALL_NODE);
  Vector_char literalInput = charVecFromCArray("input");
  Vector_char literalOutput = charVecFromCArray("output");
  Vector_ASTNode *children = getChildren(ast);
  ASTNode *identifier = vecKey_ASTNode(children, ID_NODE);
  ASTNode *args = vecKey_ASTNode(children, ARGS_NODE);
  Symbol sym;
  if (!mustGetSymbol(symTable, &identifier->content, &sym, err,
                     identifier->line))
    return false;
  if (sym.kind != FUN_SYMBOL) {
    setSemanticErrorPrefix(
        err, "esperava que o símbolo fosse uma função: ", &sym.name, sym.line);
    return false;
  }
  Vector_Codegen argsCgs = vecCreateEmpty_Codegen();
  Vector_Vector_char resultRegNames = vecCreateEmpty_Vector_char();
  if (!semanticizeArgs(args, symTable, err, scope, &argsCgs))
    return false;
  size_t argCount = vecLength_Codegen(&argsCgs);
  size_t paramCount = vecLength_Symbol(&sym.spec.fun.params);

  if (argCount != paramCount) {
    setSemanticErrorPrefix(
        err, "número incorreto de argumentos para a chamada de função: ",
        &ast->content, ast->line);
    return false;
  }
  *outCg = createCodegenObj();
  outCg->resultType = sym.spec.fun.returnDataTypeKind;
  for (size_t i = 0; i < argCount; i++) {
    Codegen *element = vecIndex_Codegen(&argsCgs, i);
    Symbol *paramSymbol = vecIndex_Symbol(&sym.spec.fun.params, i);
    if (element->resultType != paramSymbol->spec.var.dataType.kind) {
      setSemanticErrorPrefix(
          err, "chamada de função: tipo incorreto para o parâmetro ",
          &paramSymbol->name, ast->line);
    }
    fakeVecExtend_Instruction(&outCg->instructions, &element->instructions);
    vecPushRight_Vector_char(&resultRegNames, element->result->name);
  }
  for (size_t i = 0; i < argCount; i++) {
    fakeVecPushRight_Instruction(
        &outCg->instructions, createParamPushInstruction(getRegister(
                                  vecIndex_Vector_char(&resultRegNames, i))));
  }
  Register *resultStorer;
  if (sym.spec.fun.returnDataTypeKind == VOID_TYPE) {
    resultStorer = NULL;
  } else {
    resultStorer = getTemporaryRegister();
  }
  fakeVecPushRight_Instruction(
      &outCg->instructions, createCallInstruction(getRegisterNamedAfter(&sym)));
  fakeVecPushRight_Instruction(
      &outCg->instructions,
      createMoveInstruction(resultStorer, getRegisterCStr("$ret")));
  outCg->result = resultStorer;
  return true;
}

static bool semanticizeFactor(ASTNode *ast, Vector_Symbol *symTable,
                              SemanticError *err, Vector_ScopeEntry *scope,
                              Codegen *outCg) {
  assert(ast->kind == FACTOR_NODE);
  ASTNode *child = getChild(ast);
  ASTNodeKind childKind = child->kind;
  if (isExpressionType(childKind)) {
    return semanticizeExpression(child, symTable, err, scope, outCg);
  }
  if (childKind == CALL_NODE) {
    return semanticizeCall(child, symTable, err, scope, outCg);
  }
  if (childKind == NUM_NODE) {
    *outCg = createCodegenObj();
    Register *resultStorer = getTemporaryRegister();
    int64_t parsedNumber;
    assert(sscanf(charVecCreateCArray(&child->content), "%ld", &parsedNumber));
    fakeVecPushRight_Instruction(
        &outCg->instructions,
        createImmediateLoadInstruction(resultStorer, parsedNumber));
    outCg->result = resultStorer;
    outCg->resultType = INT_VALUE_TYPE;
    return true;
  }
  if (childKind == VAR_NODE) {
    Vector_ASTNode *children = getChildren(child);
    ASTNode *identifier = vecKey_ASTNode(children, ID_NODE);
    ASTNode *idxExpr = vecLookup_ASTNode(children, EXPRESSION_NODE);
    Symbol varSym;
    if (!mustGetSymbol(symTable, &identifier->content, &varSym, err,
                       identifier->line))
      return false;
    if (varSym.kind == FUN_SYMBOL) {
      setSemanticErrorPrefix(err,
                             "uso inválido de função em expressão (talvez "
                             "você tenha se esquecido de chamar ela?): ",
                             &child->content, child->line);
      return false;
    }
    Register *varReg = getRegisterNamedAfter(&varSym);
    if (idxExpr == NULL) {
      *outCg = createCodegenObj();
      // not necessary to add any instruction here
      outCg->result = varReg;
      outCg->resultType = varSym.spec.var.dataType.kind;
      return true;
    } else {
      if (varSym.spec.var.dataType.kind != INT_ARRAY_TYPE) {
        setSemanticErrorPrefix(
            err, "operador subscrito [] em símbolo que não é um vetor: ",
            &child->content, child->line);
        return false;
      }
      Codegen idxCg = createCodegenObj();
      if (!semanticizeExpression(idxExpr, symTable, err, scope, &idxCg))
        return false;
      if (idxCg.resultType != INT_VALUE_TYPE) {
        setSemanticErrorPrefix(
            err, "int é o único tipo de dado aceito como índice de array: ",
            &idxExpr->content, idxExpr->line);
        return false;
      }
      Register *resultStorer = getTemporaryRegister();
      *outCg = createCodegenObj();
      outCg->result = resultStorer;
      outCg->resultType = INT_VALUE_TYPE;
      fakeVecExtend_Instruction(&outCg->instructions, &idxCg.instructions);
      fakeVecPushRight_Instruction(
          &outCg->instructions,
          createArrayLoadInstruction(resultStorer, varReg, idxCg.result));
      return true;
    }
  }
  assert(0);
}

static bool semanticizeTerm(ASTNode *ast, Vector_Symbol *symTable,
                            SemanticError *err, Vector_ScopeEntry *scope,
                            Codegen *outCg) {
  assert(ast->kind == TERM_NODE);
  Vector_ASTNode *children = getChildren(ast);
  ASTNode *childTerm = vecLookup_ASTNode(children, TERM_NODE);
  ASTNode *mulop = vecLookup_ASTNode(children, MULOP_NODE);
  ASTNode *factor = vecKey_ASTNode(children, FACTOR_NODE);
  Codegen factorCg = createCodegenObj();
  if (!semanticizeFactor(factor, symTable, err, scope, &factorCg))
    return false;
  if (factorCg.resultType != INT_VALUE_TYPE) {
    setSemanticErrorPrefix(err, "esperava tipo int: ", &factor->content,
                           factor->line);
    return false;
  }
  if (childTerm == NULL) {
    *outCg = factorCg;
    return true;
  }
  Codegen out = createCodegenObj();
  Register *factorStorer = getTemporaryRegister();
  fakeVecExtend_Instruction(&out.instructions, &factorCg.instructions);
  fakeVecPushRight_Instruction(
      &out.instructions, createMoveInstruction(factorStorer, factorCg.result));
  Codegen childCg = createCodegenObj();
  if (!semanticizeTerm(childTerm, symTable, err, scope, &childCg))
    return false;
  if (childCg.resultType != INT_VALUE_TYPE) {
    setSemanticErrorPrefix(err, "esperava tipo int: ", &childTerm->content,
                           childTerm->line);
    return false;
  }
  fakeVecExtend_Instruction(&out.instructions, &childCg.instructions);
  InstructionOperator iop = iOperatorFromString(&mulop->content);
  Register *resultStorer = getTemporaryRegister();
  fakeVecPushRight_Instruction(
      &out.instructions, createArithmeticInstruction(
                             resultStorer, childCg.result, iop, factorStorer));
  out.result = resultStorer;
  out.resultType = INT_VALUE_TYPE;
  *outCg = out;
  return true;
}

static bool semanticizeAdditiveExpression(ASTNode *ast, Vector_Symbol *symTable,
                                          SemanticError *err,
                                          Vector_ScopeEntry *scope,
                                          Codegen *outCg) {
  assert(ast->kind == ADDITIVE_EXPRESSION_NODE);
  Codegen out = createCodegenObj();
  Vector_ASTNode *children = getChildren(ast);
  ASTNode *childAdditiveExpr = findExpressionNodeLeft(children);
  ASTNode *addop = vecLookup_ASTNode(children, ADDOP_NODE);
  ASTNode *term = findExpressionNodeRight(children);
  Codegen termCg = createCodegenObj();
  if (!semanticizeExpression(term, symTable, err, scope, &termCg))
    return false;
  if (termCg.resultType != INT_VALUE_TYPE) {
    setSemanticErrorPrefix(err, "esperava tipo int: ", &term->content,
                           term->line);
    return false;
  }
  if (childAdditiveExpr == NULL) {
    *outCg = termCg;
    return true;
  }
  Register *termStorer = getTemporaryRegister();
  fakeVecExtend_Instruction(&out.instructions, &termCg.instructions);
  fakeVecPushRight_Instruction(
      &out.instructions, createMoveInstruction(termStorer, termCg.result));
  Codegen childCg = createCodegenObj();
  if (!semanticizeExpression(childAdditiveExpr, symTable, err, scope, &childCg))
    return false;
  if (childCg.resultType != INT_VALUE_TYPE) {
    setSemanticErrorPrefix(err,
                           "esperava tipo int: ", &childAdditiveExpr->content,
                           childAdditiveExpr->line);
    return false;
  }
  fakeVecExtend_Instruction(&out.instructions, &childCg.instructions);
  InstructionOperator iop = iOperatorFromString(&addop->content);
  Register *resultStorer = getTemporaryRegister();
  fakeVecPushRight_Instruction(
      &out.instructions, createArithmeticInstruction(
                             resultStorer, childCg.result, iop, termStorer));
  out.result = resultStorer;
  out.resultType = INT_VALUE_TYPE;
  *outCg = out;
  return true;
}

static bool semanticizeSimpleExpression(ASTNode *ast, Vector_Symbol *symTable,
                                        SemanticError *err,
                                        Vector_ScopeEntry *scope,
                                        Codegen *outCg) {
  assert(ast->kind == SIMPLE_EXPRESSION_NODE);
  Vector_ASTNode *children = getChildren(ast);
  ASTNode *relopNode = vecLookup_ASTNode(children, RELOP_NODE);
  if (relopNode != NULL) {
    ASTNode *lhs = findExpressionNodeLeft(children);
    ASTNode *rhs = findExpressionNodeRight(children);
    assert(lhs != NULL && rhs != NULL);
    Codegen cgLhs = createCodegenObj(), cgRhs = createCodegenObj();
    InstructionOperator iop = iOperatorFromString(&relopNode->content);
    if (!semanticizeExpression(lhs, symTable, err, scope, &cgLhs))
      return false;
    if (!semanticizeExpression(rhs, symTable, err, scope, &cgRhs))
      return false;
    if (cgLhs.resultType != INT_VALUE_TYPE ||
        cgRhs.resultType != INT_VALUE_TYPE) {
      setSemanticErrorPrefix(
          err, "comparação só pode ser feita entre valores numéricos: ",
          &ast->content, ast->line);
      return false;
    }
    Register *lhsResult = getTemporaryRegister();
    Register *cmpResult = getTemporaryRegister();
    Codegen out = createCodegenObj();
    fakeVecExtend_Instruction(&out.instructions, &cgLhs.instructions);
    fakeVecPushRight_Instruction(
        &out.instructions, createMoveInstruction(lhsResult, cgLhs.result));
    fakeVecExtend_Instruction(&out.instructions, &cgRhs.instructions);
    fakeVecPushRight_Instruction(
        &out.instructions,
        createArithmeticInstruction(cmpResult, lhsResult, iop, cgRhs.result));
    out.result = cmpResult;
    out.resultType = INT_VALUE_TYPE;
    *outCg = out;
    return true;
  } else {
    return semanticizeAdditiveExpression(
        vecKey_ASTNode(children, ADDITIVE_EXPRESSION_NODE), symTable, err,
        scope, outCg);
  }
}

static bool isExpressionType(ASTNodeKind k) {
  return k == FACTOR_NODE || k == SIMPLE_EXPRESSION_NODE ||
         k == ADDITIVE_EXPRESSION_NODE || k == TERM_NODE ||
         k == EXPRESSION_STMT_NODE || k == EXPRESSION_NODE;
}

static bool semanticizeTheExpression(ASTNode *ast, Vector_Symbol *symTable,
                                     SemanticError *err,
                                     Vector_ScopeEntry *scope, Codegen *outCg) {
  assert(ast->kind == EXPRESSION_NODE);
  Vector_ASTNode *children = getChildren(ast);
  size_t childrenCount = vecLength_ASTNode(children);
  ASTNode *simpleExpression =
      vecLookup_ASTNode(children, SIMPLE_EXPRESSION_NODE);
  if (childrenCount > 1) {
    ASTNode *varNode = vecKey_ASTNode(children, VAR_NODE);
    Vector_ASTNode *varChildren = getChildren(varNode);
    ASTNode *varIdent = vecKey_ASTNode(varChildren, ID_NODE);
    ASTNode *varIndexExpr = findExpressionNodeRight(varChildren);
    ASTNode *expressionNode = findExpressionNodeRight(children);
    assert(expressionNode != NULL);
    Symbol varSymbol;
    if (!mustGetSymbol(symTable, &varIdent->content, &varSymbol, err,
                       ast->line))
      return false;
    if (varSymbol.kind != VAR_SYMBOL) {
      setSemanticErrorPrefix(
          err, "não se pode atribuir valores à função (constante): ",
          &varSymbol.name, varSymbol.line);
      return false;
    }
    Register *varReg = getRegisterNamedAfter(&varSymbol);
    Codegen cg = createCodegenObj(), cgIndex = createCodegenObj();
    bool addIndexStore = false;
    if (!semanticizeExpression(expressionNode, symTable, err, scope, &cg))
      return false;
    if (varSymbol.spec.var.dataType.kind == INT_ARRAY_TYPE) {
      if (varIndexExpr == NULL && cg.resultType != INT_ARRAY_TYPE) {
        setSemanticErrorPrefix(err,
                               "esperava uma expressão do tipo int[] (será que "
                               "você esqueceu de indexar o array?): ",
                               &ast->content, expressionNode->line);
        return false;
      } else if (varIndexExpr != NULL) {
        Codegen cgIndex = createCodegenObj();
        if (!semanticizeExpression(varIndexExpr, symTable, err, scope,
                                   &cgIndex))
          return false;
        if (cgIndex.resultType != INT_VALUE_TYPE) {
          setSemanticErrorPrefix(
              err, "índices de arrays precisam ser expressões do tipo int",
              &varIndexExpr->content, varIndexExpr->line);
          return false;
        }
        addIndexStore = true;
      }
    }
    Codegen out = createCodegenObj();
    fakeVecExtend_Instruction(&out.instructions, &cg.instructions);
    if (addIndexStore) {
      Register *whatToStore = getTemporaryRegister();
      fakeVecPushRight_Instruction(
          &out.instructions, createMoveInstruction(whatToStore, cg.result));
      fakeVecExtend_Instruction(&out.instructions, &cgIndex.instructions);
      fakeVecPushRight_Instruction(
          &out.instructions,
          createArrayStoreInstruction(varReg, cgIndex.result, whatToStore));
    } else {
      fakeVecPushRight_Instruction(&out.instructions,
                                   createMoveInstruction(varReg, cg.result));
    }
    *outCg = out;
    out.resultType = cg.resultType;
    out.result = varReg;
    return true;
  } else {
    return semanticizeSimpleExpression(ast, symTable, err, scope, outCg);
  }
}

static bool semanticizeExpression(ASTNode *ast, Vector_Symbol *symTable,
                                  SemanticError *err, Vector_ScopeEntry *scope,
                                  Codegen *outCg) {
  switch (ast->kind) {
  case FACTOR_NODE:
    return semanticizeFactor(ast, symTable, err, scope, outCg);
  case SIMPLE_EXPRESSION_NODE:
    return semanticizeSimpleExpression(ast, symTable, err, scope, outCg);
  case ADDITIVE_EXPRESSION_NODE:
    return semanticizeAdditiveExpression(ast, symTable, err, scope, outCg);
  case TERM_NODE:
    return semanticizeTerm(ast, symTable, err, scope, outCg);
  case EXPRESSION_STMT_NODE:
    return semanticizeExpressionStatement(ast, symTable, err, scope, outCg);
  case EXPRESSION_NODE:
    return semanticizeTheExpression(ast, symTable, err, scope, outCg);

  default:
    assert(0);
  }
}

static bool semanticizeExpressionStatement(ASTNode *ast,
                                           Vector_Symbol *symTable,
                                           SemanticError *err,
                                           Vector_ScopeEntry *scope,
                                           Codegen *cg) {
  assert(ast->kind == EXPRESSION_STMT_NODE);
  Vector_ASTNode *children = getChildren(ast);
  ASTNode *expression = findExpressionNodeLeft(children);
  if (expression == NULL) {
    return true;
  }
  return semanticizeExpression(expression, symTable, err, scope, cg);
}

static bool semanticizeIterationStatement(ASTNode *ast, Vector_Symbol *symTable,
                                          SemanticError *err,
                                          Vector_ScopeEntry *scope,
                                          Codegen *cgOut) {
  assert(ast->kind == ITERATION_STMT_NODE);
  Vector_ASTNode *children = getChildren(ast);
  ASTNode *conditionExpression = vecKey_ASTNode(children, EXPRESSION_NODE);
  ASTNode *loopingStatement = vecKey_ASTNode(children, STATEMENT_NODE);
  Register *loopStartReg, *loopEndReg;
  Instruction loopStartLabel = createLabel(&loopStartReg);
  Instruction loopEndLabel = createLabel(&loopEndReg);
  *cgOut = createCodegenObj();
  Codegen cgInsideLoop, cgCondition;
  if (!semanticizeExpression(conditionExpression, symTable, err, scope,
                             &cgCondition))
    return false;
  if (cgCondition.resultType != INT_VALUE_TYPE) {
    setSemanticErrorPrefix(err, "condição do 'while' exige tipo int: ",
                           &conditionExpression->content,
                           conditionExpression->line);
    return false;
  }
  if (!semanticizeStatement(loopingStatement, symTable, err, scope,
                            &cgInsideLoop))
    return false;
  fakeVecPushRight_Instruction(&cgOut->instructions, loopStartLabel);
  fakeVecExtend_Instruction(&cgOut->instructions, &cgCondition.instructions);
  fakeVecPushRight_Instruction(
      &cgOut->instructions,
      createIfNotInstruction(cgCondition.result, loopEndReg));
  fakeVecExtend_Instruction(&cgOut->instructions, &cgInsideLoop.instructions);
  fakeVecPushRight_Instruction(&cgOut->instructions,
                               createJumpInstruction(loopStartReg));
  fakeVecPushRight_Instruction(&cgOut->instructions, loopEndLabel);
  cgOut->result = NULL;
  cgOut->resultType = VOID_TYPE;
  return true;
}

static bool semanticizeReturnStatement(ASTNode *ast, Vector_Symbol *symTable,
                                       SemanticError *err,
                                       Vector_ScopeEntry *scope,
                                       Codegen *cgOut) {
  assert(ast->kind == RETURN_STMT_NODE);
  Register *retRegister = getRegisterCStr("$ret");
  Vector_ASTNode *children = getChildren(ast);
  ASTNode *expr = findExpressionNodeLeft(children);
  ScopeEntry *functionScopeElement;
  size_t scopeSize = vecLength_ScopeEntry(scope);
  bool foundFunction = false;
  for (size_t i = scopeSize - 1; i > 0; i--) {
    functionScopeElement = vecIndex_ScopeEntry(scope, i);
    Vector_char *fn = &functionScopeElement->name;
    if (*vecIndex_char(fn, vecLength_char(fn) - 1) == ')') {
      foundFunction = true;
      break;
    }
  }
  if (!foundFunction) {
    setSemanticErrorLiteral(err, "return fora de uma função", ast->line);
    return false;
  }
  Vector_char symNameActual = vecDuplicate_char(&functionScopeElement->name);
  vecPopRight_char(&symNameActual);
  vecPopRight_char(&symNameActual);
  Symbol funcSym;
  STRINGIFY_DBG(Vector_Symbol, *symTable);
  STRINGIFY_DBG(ScopeEntry, *functionScopeElement);
  assert(mustGetSymbol(symTable, &symNameActual, &funcSym, err, ast->line));
  if (expr == NULL && funcSym.spec.fun.returnDataTypeKind != VOID_TYPE) {
    setSemanticErrorLiteral(
        err, "return vazio em função com tipo de retorno diferente de 'void'",
        ast->line);
    return false;
  }
  if (expr != NULL && funcSym.spec.fun.returnDataTypeKind == VOID_TYPE) {
    setSemanticErrorLiteral(
        err, "return com valor em função com tipo de retorno 'void'",
        ast->line);
    return false;
  }
  Codegen exprCg = createCodegenObj();
  if (!semanticizeExpression(expr, symTable, err, scope, &exprCg))
    return false;
  if (exprCg.resultType != funcSym.spec.fun.returnDataTypeKind) {
    Vector_char msgerr = STRINGIFY_TO_CHAR_VEC(DataTypeKind, exprCg.resultType);
    setSemanticErrorPrefix(
        err,
        "return com tipo de dado diferente do retornado pela função: ", &msgerr,
        ast->line);
    return false;
  }
  *cgOut = createCodegenObj();
  fakeVecPushRight_Instruction(
      &cgOut->instructions, createMoveInstruction(retRegister, exprCg.result));
  fakeVecPushRight_Instruction(&cgOut->instructions, createReturnInstruction());
  return true;
}

static bool isAStatement(ASTNodeKind kind) {
  return kind == EXPRESSION_STMT_NODE || kind == SELECTION_STMT_NODE ||
         kind == ITERATION_STMT_NODE || kind == RETURN_STMT_NODE ||
         kind == COMPOUND_STMT_NODE;
}

static bool semanticizeStatement(ASTNode *ast, Vector_Symbol *symTable,
                                 SemanticError *err, Vector_ScopeEntry *scope,
                                 Codegen *cgOut) {
  switch (ast->kind) {
  case EXPRESSION_STMT_NODE:
    return semanticizeExpressionStatement(ast, symTable, err, scope, cgOut);
  case SELECTION_STMT_NODE:
    return semanticizeSelectionStatement(ast, symTable, err, scope, cgOut);
  case ITERATION_STMT_NODE:
    return semanticizeIterationStatement(ast, symTable, err, scope, cgOut);
  case RETURN_STMT_NODE:
    return semanticizeReturnStatement(ast, symTable, err, scope, cgOut);
  case COMPOUND_STMT_NODE:
    return semanticizeCompoundStmt(ast, symTable, err, scope, cgOut);
  default:
    assert(0); // erro do analisador sintático
  }
}

static bool semanticizeTheStatement(ASTNode *ast, Vector_Symbol *symTable,
                                    SemanticError *err,
                                    Vector_ScopeEntry *scope, Codegen *cgOut) {
  if (ast->kind == STATEMENT_NODE) {
    ASTNode *child = getChild(ast);
    return semanticizeStatement(child, symTable, err, scope, cgOut);
  }
  return semanticizeStatement(ast, symTable, err, scope, cgOut);
}

static bool semanticizeStatementList(ASTNode *ast, Vector_Symbol *symTable,
                                     SemanticError *err,
                                     Vector_ScopeEntry *scope, Codegen *cgOut) {

  Vector_ASTNode *children = getChildren(ast);
  *cgOut = createCodegenObj();
  size_t len = vecLength_ASTNode(children);
  switch (ast->kind) {
  case EMPTY_NODE:
    return true;
  case LOCAL_DECLARATIONS_NODE:
  case STATEMENT_LIST_NODE:
    bool ok = true;
    Codegen cgCat = createCodegenObj();
    for (size_t i = 0; i < len; i++) {
      ok = semanticizeStatementList(vecIndex_ASTNode(children, i), symTable,
                                    err, scope, &cgCat);
      if (!ok)
        return false;
      vecExtend_Instruction(&cgOut->instructions, &cgCat.instructions);
    }
    return true;
  case VAR_DECLARATION_NODE:
    return semanticizeVarDeclaration(ast, symTable, scope, err);
  case STATEMENT_NODE:
  case EXPRESSION_STMT_NODE:
  case ITERATION_STMT_NODE:
  case SELECTION_STMT_NODE:
  case RETURN_STMT_NODE:
    return semanticizeStatement(ast, symTable, err, scope, cgOut);
  default:
    assert(0); // erro do analisador sintático
  }
}

static bool semanticizeCompoundStmt(ASTNode *ast, Vector_Symbol *symTable,
                                    SemanticError *err,
                                    Vector_ScopeEntry *scope, Codegen *cgOut) {
  assert(ast->kind == COMPOUND_STMT_NODE);
  Vector_ASTNode *children = getChildren(ast);
  assert(children->collection != NULL);
  ASTNode *localDeclarations =
      vecKey_ASTNode(children, LOCAL_DECLARATIONS_NODE);
  ASTNode *statementList = vecKey_ASTNode(children, STATEMENT_LIST_NODE);
  Vector_char scopeName = charVecFromCArray("#block");
  scopePush(scope, &scopeName);
  if (!semanticizeLocalDeclarations(localDeclarations, symTable, err, scope))
    return false;
  if (!semanticizeStatementList(statementList, symTable, err, scope, cgOut))
    return false;
  scopePop(scope, symTable);
  return true;
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
  case TYPE_SPECIFIER_NODE:
    return true;
  default:
    assert(0); // erro do analisador sintático
  }
}

static bool semanticizeFunDeclaration(ASTNode *ast, Vector_Symbol *symTable,
                                      Vector_ScopeEntry *scope,
                                      SemanticError *err, Codegen *cgOut) {

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
  Vector_Symbol parsedParams = vecCreateEmpty_Symbol();
  if (!semanticizeParams(params, &parsedParams, err, scope))
    return false;

  sym.spec.fun.params = parsedParams;
  if (ts == INT_TYPE_SPEC) {
    sym.spec.fun.returnDataTypeKind = INT_VALUE_TYPE;
  } else if (ts == VOID_TYPE_SPEC) {
    sym.spec.fun.returnDataTypeKind = VOID_TYPE;
  }
  if (!symTablePush(scope, symTable, &sym, err))
    return false;

  Vector_char scopeName = vecDuplicate_char(&sym.name);
  charVecAppendCArray(&scopeName, "()");
  scopePush(scope, &scopeName);

  for (size_t i = 0; i < vecLength_Symbol(&parsedParams); i++) {
    if (!symTablePush(scope, symTable, vecIndex_Symbol(&parsedParams, i), err))
      return false;
  }

  Codegen fnBody = createCodegenObj();
  if (!semanticizeCompoundStmt(block, symTable, err, scope, &fnBody))
    return false;
  for (size_t i = 0; i < vecLength_Symbol(&parsedParams); i++) {
    vecPushLeft_Instruction(&fnBody.instructions,
                            createParamPopInstruction(getRegisterNamedAfter(
                                vecIndex_Symbol(&parsedParams, i))));
  }
  vecPushLeft_Instruction(&fnBody.instructions,
                          createLabelInstruction(getRegisterNamedAfter(&sym)));
  *cgOut = createCodegenObj();
  fakeVecExtend_Instruction(&cgOut->instructions, &fnBody.instructions);
  cgOut->result = NULL;
  cgOut->resultType = VOID_TYPE;

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
    assert(sscanf(charVecCreateCArray(&maybeSize->content), "%lu",
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
  return true;
}

static bool semanticizeDeclaration(ASTNode *ast, Vector_Symbol *symTable,
                                   Vector_ScopeEntry *scope, SemanticError *err,
                                   Codegen *cgOut) {
  assert(ast->kind == DECLARATION_NODE);

  ASTNode *childNode = getChild(ast);
  assert(childNode->kind == FUN_DECLARATION_NODE ||
         childNode->kind == VAR_DECLARATION_NODE);
  if (childNode->kind == FUN_DECLARATION_NODE) {
    return semanticizeFunDeclaration(childNode, symTable, scope, err, cgOut);
  } else if (childNode->kind == VAR_DECLARATION_NODE) {
    return semanticizeVarDeclaration(childNode, symTable, scope, err);
  }

  return true;
}

static bool semanticizeDeclarationList(ASTNode *ast, Vector_Symbol *symTable,
                                       Vector_ScopeEntry *scope,
                                       SemanticError *err, Codegen *cgOut) {
  assert(ast->kind == DECLARATION_LIST_NODE);
  Vector_ASTNode *declarations = getChildren(ast);
  size_t len = vecLength_ASTNode(declarations);
  *cgOut = createCodegenObj();
  for (size_t i = 0; i < len; i++) {
    Codegen cgCat = createCodegenObj();
    cgCat.result = NULL;
    cgCat.resultType = VOID_TYPE;
    ASTNode *child = vecIndex_ASTNode(declarations, i);
    assert(child->kind == DECLARATION_LIST_NODE ||
           child->kind == DECLARATION_NODE ||
           child->kind == VAR_DECLARATION_NODE ||
           child->kind == FUN_DECLARATION_NODE);
    if (child->kind == DECLARATION_LIST_NODE) {
      bool res =
          semanticizeDeclarationList(child, symTable, scope, err, &cgCat);
      if (!res) {
        return false;
      }
      fakeVecExtend_Instruction(&cgOut->instructions, &cgCat.instructions);
    } else if (child->kind == DECLARATION_NODE) {
      bool res = semanticizeDeclaration(child, symTable, scope, err, &cgCat);
      if (!res) {
        return false;
      }
      fakeVecExtend_Instruction(&cgOut->instructions, &cgCat.instructions);

    } else if (child->kind == VAR_DECLARATION_NODE) {
      bool res = semanticizeVarDeclaration(child, symTable, scope, err);
      if (!res) {
        return false;
      }
      fakeVecExtend_Instruction(&cgOut->instructions, &cgCat.instructions);
    } else if (child->kind == FUN_DECLARATION_NODE) {
      bool res = semanticizeFunDeclaration(child, symTable, scope, err, &cgCat);
      if (!res) {
        return false;
      }
      fakeVecExtend_Instruction(&cgOut->instructions, &cgCat.instructions);
    }
  }

  return true;
}

bool semanticize(ASTNode *ast, Vector_Vector_Symbol *outSymbolTables,
                 SemanticError *err, void *outCode) {
  assert(ast->kind == PROGRAM_NODE);
  codegenInit();
  ASTNode *childNode = getChild(ast);
  Vector_Symbol programSymbolTable = vecCreateEmpty_Symbol();
  Vector_char strGlobal = charVecFromCArray("global");
  Vector_ScopeEntry scope = vecCreateEmpty_ScopeEntry();
  scopePush(&scope, &strGlobal);
  Symbol inputsym;
  inputsym.kind = FUN_SYMBOL;
  inputsym.line = 0;
  inputsym.name = charVecFromCArray("input");
  inputsym.node = ast;
  inputsym.spec.fun.params = vecCreateEmpty_Symbol();
  inputsym.spec.fun.returnDataTypeKind = INT_VALUE_TYPE;
  inputsym.isParameter = false;
  Symbol outputarg;
  outputarg.kind = VAR_SYMBOL;
  outputarg.line = 0;
  outputarg.isParameter = true;
  outputarg.name = charVecFromCArray("val");
  outputarg.spec.var.dataType.kind = INT_VALUE_TYPE;
  outputarg.node = ast;
  outputarg.scope = scope;
  Symbol outputsym;
  outputsym.kind = FUN_SYMBOL;
  outputsym.line = 0;
  outputsym.name = charVecFromCArray("output");
  outputsym.node = ast;
  outputsym.spec.fun.params = vecCreateSingle_Symbol(outputarg);
  outputsym.spec.fun.returnDataTypeKind = INT_VALUE_TYPE;
  outputsym.isParameter = false;
  symTablePush(&scope, &programSymbolTable, &inputsym, err);
  symTablePush(&scope, &programSymbolTable, &outputsym, err);

  return semanticizeDeclarationList(childNode, &programSymbolTable, &scope, err,
                                    outCode);
}
