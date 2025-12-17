#include "char_vector.h"
#include "codegen.h"
#include "lexer.h"
#include "semantic.h"
#include "stringify.h"
#include "syntactic.h"
#include <stdio.h>

int main(int argc, char *argv[]) {
  Vector_Token toks;
  LexError theMistake;
  FILE *f = fopen(argv[1], "rb");
  bool res = tokenize(f, &toks, &theMistake);
  STRINGIFY_DBG(Vector_Token, toks);
  if (!res) {
    STRINGIFY_DBG(LexError, theMistake);
    printf("ERRO LEXICO: %s - LINHA %u\n",
           vecBorrow_char(&theMistake.token.content, NULL),
           theMistake.token.line);
    STRINGIFY_DBG(Token, theMistake.token);
  }
  SyntaxError synError;
  ASTNode ast;
  bool synRes = generateAST(toks, &ast, &synError);
  if (!synRes) {
    printf("ERRO SINTATICO: %s - LINHA %u\n", synError.message,
           synError.node.line);
    return 1;
  }
  printASTTree(&ast, 0);
  Codegen cg = createCodegenObj();
  Vector_Vector_Symbol st = vecCreateEmpty_Vector_Symbol();
  SemanticError semError;
  bool semRes = semanticize(&ast, &st, &semError, &cg);
  if (!semRes) {
    STRINGIFY_DBG(SemanticError, semError);
    printf("ERRO SEMANTICO: %s - LINHA %lu\n", semError.message,
           semError.sourceLine);
  }

  FILE *out = fopen("output.asm", "wb");
  Vector_char outBuf =
      STRINGIFY_TO_CHAR_VEC(Vector_Instruction, cg.instructions);

  fputs(charVecCreateCArray(&outBuf), out);
  fclose(out);

  return 0;
}
