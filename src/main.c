#include "lexer.h"
#include "stringify.h"
#include "syntactic.h"

int main(int argc, char *argv[]) {
  Vector_Token toks;
  LexError theMistake;
  bool res = tokenize(stdin, &toks, &theMistake);
  STRINGIFY_DBG(Vector_Token, toks);
  if (!res) {
    STRINGIFY_DBG(LexError, theMistake);
    STRINGIFY_DBG(Token, theMistake.token);
  }
  SyntaxError synError;
  ASTNode ast;
  bool synRes = generateAST(toks, &ast, &synError);
  if (!synRes) {
    STRINGIFY_DBG(SyntaxError, synError);
    return 1;
  }
  printASTTree(&ast, 0);
  return 0;
}
