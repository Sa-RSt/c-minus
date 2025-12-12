#pragma once

#include "char_vector.h"
#include "error.h"
#include "vector.h"

typedef enum TokenKind {
  KEYWORD_TOKEN,
  NUMBER_TOKEN,
  IDENTIFIER_TOKEN,
  LEFT_PAREN_TOKEN,
  RIGHT_PAREN_TOKEN,
  LEFT_SQ_BRACKET_TOKEN,
  RIGHT_SQ_BRACKET_TOKEN,
  LEFT_CURLY_BRACE_TOKEN,
  RIGHT_CURLY_BRACE_TOKEN,
  LEFT_COMMENT_TOKEN,
  RIGHT_COMMENT_TOKEN,
  SEMICOLON_TOKEN,
  SPECIAL_TOKEN
} TokenKind;

typedef struct Token {
  Vector_char content;
  TokenKind kind;
  uint32_t line;
} Token;

typedef struct LexError {
  ERROR_TYPE_FIELDS
  Token token;
} LexError;

HEADER_ERROR_TYPE_FUNCTIONS(LexError)

HEADER_VECTOR_TYPE(Token, Vector_char)

bool tokenize(FILE *input, Vector_Token *out, LexError *err);
