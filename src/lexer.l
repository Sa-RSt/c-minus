%option noyywrap
%{
#include "lexer.h"
#include "char_vector.h"
#include "stringify.h"
#include "vector.h"
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>

Vector_Token *generatedTokens;
LexError errorThrown;
bool errorExists = false;
bool insideComment = false;
bool wantsWhitespace = false;

#define TOKEN_CONTENT_GETTER(x) (&x.content)
DECLARE_VECTOR_TYPE(Token, Vector_char*, TOKEN_CONTENT_GETTER, charVecStrcmp)
DECLARE_VECTOR_STRINGIFY_FUNCTION(Token)
DECLARE_ERROR_TYPE_FUNCTIONS(LexError)


static void putToken(TokenKind kind);
static bool errorWantsWhitespace();
%}

%%

, {
  putToken(COMMA_TOKEN);
  wantsWhitespace = false;
}

[+-/*<>=]|<=|>=|==|!= {
  putToken(SPECIAL_TOKEN);
  wantsWhitespace = false;
}

; {
  putToken(SEMICOLON_TOKEN);
  wantsWhitespace = false;
}

"/*" {
  putToken(LEFT_COMMENT_TOKEN);
  insideComment = true;
  wantsWhitespace = false;
}

"*/" {
  insideComment = false;
  putToken(RIGHT_COMMENT_TOKEN);
  wantsWhitespace = false;
}

(int|void|while|if|else|return) {
  if (wantsWhitespace && errorWantsWhitespace()) return 1;
  putToken(KEYWORD_TOKEN);
  wantsWhitespace = true;
}

[a-zA-Z]+ {
  if (wantsWhitespace && errorWantsWhitespace()) return 1;
  putToken(IDENTIFIER_TOKEN);
  wantsWhitespace = true;
}

[0-9]+ {
  if (wantsWhitespace && errorWantsWhitespace()) return 1;
  putToken(NUMBER_TOKEN);
  wantsWhitespace = true;
}

\( {
  putToken(LEFT_PAREN_TOKEN);
  wantsWhitespace = false;
}

\) {
  putToken(RIGHT_PAREN_TOKEN);
  wantsWhitespace = false;
}

\[ {
  putToken(LEFT_SQ_BRACKET_TOKEN);
  wantsWhitespace = false;
}

\] {
  putToken(RIGHT_SQ_BRACKET_TOKEN);
  wantsWhitespace = false;
}
 
\{ {
  putToken(LEFT_CURLY_BRACE_TOKEN);
  wantsWhitespace = false;
}

\} {
  putToken(RIGHT_CURLY_BRACE_TOKEN);
  wantsWhitespace = false;
}

[[:space:]]+ {
  wantsWhitespace = false;
}


. {
  if (!insideComment) {
    errorExists = true;
    char *errorText = malloc(1024);
    strcat(errorText, "token não reconhecido: ");
    strcat(errorText, yytext);
    CREATE_ERROR(&errorThrown, errorText);
    Token tok;
    tok.content = charVecFromCArray(yytext);
    tok.line = yylineno;
    tok.kind = (TokenKind)-1;
    errorThrown.token = tok;
    return 1;
  }
}

%%

extern FILE *yyin;

DECLARE_STRINGIFY_FUNCTION(TokenKind, tk) {
  switch (tk) {
  case KEYWORD_TOKEN:
    STRINGIFY_PUT("KEYWORD_TOKEN");
    break;
  case NUMBER_TOKEN:
    STRINGIFY_PUT("NUMBER_TOKEN");
    break;
  case IDENTIFIER_TOKEN:
    STRINGIFY_PUT("IDENTIFIER_TOKEN");
    break;
  case LEFT_PAREN_TOKEN:
    STRINGIFY_PUT("LEFT_PAREN_TOKEN");
    break;
  case RIGHT_PAREN_TOKEN:
    STRINGIFY_PUT("RIGHT_PAREN_TOKEN");
    break;
  case LEFT_SQ_BRACKET_TOKEN:
    STRINGIFY_PUT("LEFT_SQ_BRACKET_TOKEN");
    break;
  case RIGHT_SQ_BRACKET_TOKEN:
    STRINGIFY_PUT("RIGHT_SQ_BRACKET_TOKEN");
    break;
  case LEFT_CURLY_BRACE_TOKEN:
    STRINGIFY_PUT("LEFT_CURLY_BRACE_TOKEN");
    break;
  case RIGHT_CURLY_BRACE_TOKEN:
    STRINGIFY_PUT("RIGHT_CURLY_BRACE_TOKEN");
    break;
  case LEFT_COMMENT_TOKEN:
    STRINGIFY_PUT("LEFT_COMMENT_TOKEN");
    break;
  case RIGHT_COMMENT_TOKEN:
    STRINGIFY_PUT("RIGHT_COMMENT_TOKEN");
    break;
  case SEMICOLON_TOKEN:
    STRINGIFY_PUT("SEMICOLON_TOKEN");
    break;
  case SPECIAL_TOKEN:
    STRINGIFY_PUT("SPECIAL_TOKEN");
    break;
  case COMMA_TOKEN:
    STRINGIFY_PUT("COMMA_TOKEN");
    break;
  default:
    STRINGIFY_PUT("<UNKNOWN>");
    break;
  }
}

DECLARE_STRINGIFY_FUNCTION(Token, tok) {
  STRINGIFY_PUT("content: ");
  STRINGIFY_PUT_VALUE(Vector_char, tok.content);
  STRINGIFY_PUT(" line: ");
  STRINGIFY_PUT_VALUE(uint32_t, tok.line);
  STRINGIFY_PUT(" kind: ");
  STRINGIFY_PUT_VALUE(TokenKind, tok.kind);
}

static void putToken(TokenKind kind) {
  if (insideComment) {
    return;
  }
  Token tok;
  tok.kind = kind;
  tok.line = (uint32_t)yylineno;
  tok.content = charVecFromCArray(yytext);
  vecPushRight_Token(generatedTokens, tok);
}

static bool errorWantsWhitespace() {
  if (insideComment) {
    return false;
  }
  errorExists = true;
  char *errorText = malloc(1024);
  strcpy(errorText, "esperava whitespace antes de: ");
  strcat(errorText, yytext);
  CREATE_ERROR(&errorThrown, errorText);
  errorThrown.token = *vecIndex_Token(generatedTokens, vecLength_Token(generatedTokens)-1);
  return true;
}

bool tokenize(FILE *input, Vector_Token *out, LexError *err) {
  *out = vecCreateEmpty_Token();
  generatedTokens = out;
  errorExists = false;
  insideComment = false;
  wantsWhitespace = false;
  yyin = input;
  yylex();
  *err = errorThrown;
  return !errorExists;
}

