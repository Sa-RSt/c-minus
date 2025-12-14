#include "lexer.h"
#include "stringify.h"

int main(int argc, char *argv[]) {
  Vector_Token toks;
  LexError theMistake;
  bool res = tokenize(stdin, &toks, &theMistake);
  STRINGIFY_DBG(Vector_Token, toks);
  if (!res) {
    STRINGIFY_DBG(LexError, theMistake);
    STRINGIFY_DBG(Token, theMistake.token);
  }
}
