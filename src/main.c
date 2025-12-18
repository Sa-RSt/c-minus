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
  puts("#### [ LEXER ] ####");
  STRINGIFY_DBG(Vector_Token, toks);
  if (!res) {
    STRINGIFY_DBG(LexError, theMistake);
    printf("ERRO LEXICO: %s - LINHA %u\n",
           vecBorrow_char(&theMistake.token.content, NULL),
           theMistake.token.line);
    STRINGIFY_DBG(Token, theMistake.token);
    return 0;
  }
  puts("#### [ SINTÁTICO ] ####");
  SyntaxError synError;
  ASTNode ast;
  bool synRes = generateAST(toks, &ast, &synError);
  if (!synRes) {
    printf("ERRO SINTATICO: %s - LINHA %u\n", synError.message,
           synError.sourceLine);
    return 1;
  }
  printASTTree(&ast, 0);
  puts("#### [ SEMÂNTICO - OUTPUT ] ####");
  Codegen cg = createCodegenObj();
  Vector_Symbol st = vecCreateEmpty_Symbol();
  SemanticError semError;
  bool semRes = semanticize(&ast, &st, &semError, &cg);
  if (!semRes) {
    printf("ERRO SEMANTICO: %s - LINHA %lu\n", semError.message,
           semError.sourceLine);
  }
  puts("#### [ SEMÂNTICO - TABELA DE SÍMBOLOS ] ####");
  puts("!!!OBSERVAÇÃO IMPORTANTE!!! Os escopos estão escritos em um estilo "
       "parecido com um caminho em um sistema de arquivos. Então, por exemplo, "
       "uma variável local a uma função chamada F() terá escopo 'global/F()', "
       "já que o escopo de F() está contido no global. Consequentemente, "
       "qualquer escopo começa com a palavra 'global', mesmo que não seja um "
       "escopo global.\n");
  for (size_t i = 0; i < vecLength_Symbol(&st); i++) {
    Symbol *s = vecIndex_Symbol(&st, i);
    if (s->line != 0) { // omitir símbolos implícitos "input" e "output"
      STRINGIFY_DBG(Symbol, *s);
    }
  }

  puts("#### [ 3 ENDEREÇOS ] ####");
  if (semRes) {
    FILE *out = stdout; // fopen("output.asm", "wb");
    Vector_char outBuf =
        STRINGIFY_TO_CHAR_VEC(Vector_Instruction, cg.instructions);

    fputs(charVecCreateCArray(&outBuf), out);
    fclose(out);
  } else {
    puts("Sem saída, pois houve um erro no código fonte C-.");
  }

  return 0;
}
