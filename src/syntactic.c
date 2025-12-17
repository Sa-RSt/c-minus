#include "syntactic.h"
#include "char_vector.h"
#include <stdio.h>
#include <stdlib.h>

#pragma region DECLARE_VECTOR_TYPES
// Funções 'get' necessárias para DECLARE_VECTOR_TYPE
static ASTNodeKind getASTNodeKind(ASTNode node) { return node.kind; }

// Funções de comparação necessárias para DECLARE_VECTOR_TYPE
static int astNodeKindComparer(ASTNodeKind a, ASTNodeKind b) {
  return (int)a - (int)b;
}

DECLARE_VECTOR_TYPE(ASTNode, ASTNodeKind, getASTNodeKind, astNodeKindComparer)
#pragma endregion

#pragma region DECLARE_STRINGIFY_FUNCTIONS

// Função de stringify para retorno de erros de sintaxe
DECLARE_STRINGIFY_FUNCTION(SyntaxError, err) {
  STRINGIFY_PUT("Syntax error in line: ");
  STRINGIFY_PUT_VALUE(uint32_t, err.line);
  STRINGIFY_PUT(", message: ");
  STRINGIFY_PUT(err.message);
  STRINGIFY_PUT(")");
}

DECLARE_STRINGIFY_FUNCTION(ASTNodeKind, kind) {
  switch (kind) {
  case PROGRAM_NODE:
    STRINGIFY_PUT("PROGRAM");
    break;
  case DECLARATION_LIST_NODE:
    STRINGIFY_PUT("DECL_LIST");
    break;
  case DECLARATION_NODE:
    STRINGIFY_PUT("DECL");
    break;
  case VAR_DECLARATION_NODE:
    STRINGIFY_PUT("VAR_DECL");
    break;
  case FUN_DECLARATION_NODE:
    STRINGIFY_PUT("FUN_DECL");
    break;
  case TYPE_SPECIFIER_NODE:
    STRINGIFY_PUT("TYPE_SPEC");
    break;
  case ID_NODE:
    STRINGIFY_PUT("ID");
    break;
  case NUM_NODE:
    STRINGIFY_PUT("NUM");
    break;
  case PARAMS_NODE:
    STRINGIFY_PUT("PARAMS");
    break;
  case PARAM_LIST_NODE:
    STRINGIFY_PUT("PARAM_LIST");
    break;
  case PARAM_NODE:
    STRINGIFY_PUT("PARAM");
    break;
  case EMPTY_SQR_BRACKETS_NODE:
    STRINGIFY_PUT("EMPTY_SQR");
    break;
  case COMPOUND_STMT_NODE:
    STRINGIFY_PUT("COMPOUND ");
    break;
  case LOCAL_DECLARATIONS_NODE:
    STRINGIFY_PUT("LOCAL_DECLS");
    break;
  case STATEMENT_LIST_NODE:
    STRINGIFY_PUT("STMT_LIST");
    break;
  case STATEMENT_NODE:
    STRINGIFY_PUT("STMT");
    break;
  case EXPRESSION_STMT_NODE:
    STRINGIFY_PUT("EXPR_STMT");
    break;
  case SELECTION_STMT_NODE:
    STRINGIFY_PUT("IF_STMT");
    break;
  case ITERATION_STMT_NODE:
    STRINGIFY_PUT("WHILE_STMT");
    break;
  case RETURN_STMT_NODE:
    STRINGIFY_PUT("RETURN_STMT");
    break;
  case EXPRESSION_NODE:
    STRINGIFY_PUT("EXPR");
    break;
  case VAR_NODE:
    STRINGIFY_PUT("VAR");
    break;
  case SIMPLE_EXPRESSION_NODE:
    STRINGIFY_PUT("SIMPLE_EXPR");
    break;
  case ADDITIVE_EXPRESSION_NODE:
    STRINGIFY_PUT("ADD_EXPR");
    break;
  case RELOP_NODE:
    STRINGIFY_PUT("RELOP");
    break;
  case TERM_NODE:
    STRINGIFY_PUT("TERM");
    break;
  case ADDOP_NODE:
    STRINGIFY_PUT("ADDOP");
    break;
  case MULOP_NODE:
    STRINGIFY_PUT("MULOP");
    break;
  case FACTOR_NODE:
    STRINGIFY_PUT("FACTOR");
    break;
  case CALL_NODE:
    STRINGIFY_PUT("CALL");
    break;
  case ARGS_NODE:
    STRINGIFY_PUT("ARGS");
    break;
  case ARG_LIST_NODE:
    STRINGIFY_PUT("ARG_LIST");
    break;
  case EMPTY_NODE:
    STRINGIFY_PUT("EMPTY");
    break;
  default:
    STRINGIFY_PUT("UNKNOWN");
    break;
  }
}
#pragma endregion

void printASTTree(const ASTNode *node, size_t indent) {
  if (node == NULL)
    return;

  // Indentação
  for (size_t i = 0; i < indent; i++)
    printf("  ");

  // Encontra o tipo de nó
  staticStringifyBuffer[0] = 0;
  stringify_ASTNodeKind(node->kind, staticStringifyBuffer);
  printf("[%s] (line %u)", staticStringifyBuffer, node->line);

  // Imprime conteúdo, se houver
  if (vecLength_char((Vector_char *)&node->content) > 0) 
      {
    char *text = charVecCreateCArray((Vector_char *)&node->content);
    printf(" content=\"%s\"", text);
    free(text);
  }
  printf("\n");

  // Recursão para os filhos
  size_t num_children = vecLength_ASTNode((Vector_ASTNode *)&node->children);
  for (size_t i = 0; i < num_children; i++) {
    ASTNode *child = vecIndex_ASTNode((Vector_ASTNode *)&node->children, i);
    printASTTree(child, indent + 1);
  }
}

#pragma region GRAMATICA_C_MINUS
/*
CFG:
program -> declaration_list
declaration_list -> declaration_list declaration | declaration
declaration -> var_declaration | fun_declaration
var_declaration -> type_specifier ID ';' | type_specifier ID '[' NUM ']' ';'
type_specifier -> 'int' | 'void'
fun_declaration -> type_specifier ID '(' params ')' compound_stmt
params -> param_list | 'void'
param_list -> param_list ',' param | param
param -> type_specifier ID | type_specifier ID '[' ']'
compound_stmt -> '{' local_declarations statement_list '}'
local_declarations -> local_declarations var_declaration | empty
statement_list -> statement_list statement | empty
statement -> expression_stmt | compound_stmt | selection_stmt | iteration_stmt |
return_stmt expression_stmt -> expression ';' | ';' selection_stmt -> 'if' '('
expression ')' statement | 'if' '(' expression ')' statement 'else' statement
iteration_stmt -> 'while' '(' expression ')' statement
return_stmt -> 'return' ';' | 'return' expression ';'
expression -> var '=' expression | simple_expression
var -> ID | ID '[' expression ']'
simple_expression -> additive_expression relop additive_expression |
additive_expression relop -> '<=' | '<' | '>' | '>=' | '==' | '!='
additive_expression -> additive_expression addop term | term
addop -> '+' | '-'
term -> term mulop factor | factor
mulop -> '*' | '/'
factor -> '(' expression ')' | var | call | NUM
call -> ID '(' args ')'
args -> arg_list | empty
arg_list -> arg_list ',' expression | expression
*/
#pragma endregion

#pragma region PARSER_DECLARATIONS

// Declarações
static ASTNode *parse_program(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_declaration_list(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_declaration(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_var_declaration(Vector_Token *tokens, size_t *pos, 
ASTNode *type_spec, Token *id_token, SyntaxError *err);

static ASTNode *parse_fun_declaration(Vector_Token *tokens, size_t *pos, 
ASTNode *type_spec, Token *id_token, SyntaxError *err);

static ASTNode *parse_type_specifier(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_params(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_param_list(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_param(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_compound_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_local_declarations(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_statement_list(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_statement(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_expression_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_selection_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_iteration_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_return_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_expression(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_var(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_simple_expression(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_additive_expression(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_term(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_factor(Vector_Token *tokens, size_t *pos, SyntaxError *err);
                             
static ASTNode *parse_call(Vector_Token *tokens, size_t *pos, 
Token *id_token, SyntaxError *err);

static ASTNode *parse_args(Vector_Token *tokens, size_t *pos, SyntaxError *err);

static ASTNode *parse_arg_list(Vector_Token *tokens, size_t *pos, SyntaxError *err);
#pragma endregion

#pragma region UTILITY_FUNCTIONS

// Verifica o token atual (impede overflow)
static Token *peek(Vector_Token *tokens, size_t pos) {
  if (pos >= vecLength_Token(tokens)) {
    return NULL;
  }
  return vecIndex_Token(tokens, pos);
}

// Avança o ponteiro de tokens
static Token *advance(Vector_Token *tokens, size_t *pos) {
  Token *token = peek(tokens, *pos);
  if (token != NULL) {
    (*pos)++;
  }
  return token;
}

// Compara Vector_char com C string (para keywords e símbolos especiais)
static bool charvec_equals_cstr(const Vector_char *vec, const char *cstr) {
  Vector_char tmp = charVecFromCArray(cstr);
  bool eq = (charVecStrcmp(vec, &tmp) == 0);
  vecFree_char(&tmp);
  return eq;
}

// Wrapper da função de cima, checa se o token é uma keyword específica
static bool is_keyword(Token *token, const char *keyword) {
  return token != NULL && token->kind == KEYWORD_TOKEN &&
         charvec_equals_cstr(&token->content, keyword);
}

// Wrapper que usa o outro pra verificar símbolos especiais
static bool is_special(Token *token, const char *special) {
  return token != NULL && token->kind == SPECIAL_TOKEN &&
         charvec_equals_cstr(&token->content, special);
}

// Helper functions for specific special characters
static bool is_semicolon(Token *token) {
  return token != NULL && token->kind == SEMICOLON_TOKEN;
}

static bool is_comma(Token *token) {
  return token != NULL && token->kind == COMMA_TOKEN;
}

static bool is_lparen(Token *token) {
  return token != NULL && token->kind == LEFT_PAREN_TOKEN;
}

static bool is_rparen(Token *token) {
  return token != NULL && token->kind == RIGHT_PAREN_TOKEN;
}

static bool is_lbracket(Token *token) {
  return token != NULL && token->kind == LEFT_SQ_BRACKET_TOKEN;
}

static bool is_rbracket(Token *token) {
  return token != NULL && token->kind == RIGHT_SQ_BRACKET_TOKEN;
}

static bool is_lbrace(Token *token) {
  return token != NULL && token->kind == LEFT_CURLY_BRACE_TOKEN;
}

static bool is_rbrace(Token *token) {
  return token != NULL && token->kind == RIGHT_CURLY_BRACE_TOKEN;
}

// Inicializa uma ASTNode
static ASTNode *create_node(ASTNode *node, ASTNodeKind kind, uint32_t line) {
  if (node == NULL) {
    node = malloc(sizeof(ASTNode));
  }
  node->content = vecCreateEmpty_char(); // String vazia
  node->kind = kind;
  node->line = line;
  node->children = vecCreateEmpty_ASTNode(); // Vetor de filhos vazio
  return node;
}

// Cria node a partir de um token
static ASTNode *create_node_from_token(ASTNodeKind kind, Token *token) {
  ASTNode *node = create_node(NULL, kind, token->line);
  node->content = vecDuplicate_char(&token->content);
  return node;
}

// Envia um erro
static void set_error(SyntaxError *err, Token *token, const char *message) {
  if (token) {
    err->line = token->line;
  } else {
    err->line = 0;
  }
  err->filename = __FILE__;
  err->message = (char *)message;
  err->caused = NULL;
}

// Avança se o token for do tipo esperado, senão seta erro (Bom para 'consumir' tokens)
static Token *expect(Vector_Token *tokens, size_t *pos, TokenKind kind,
SyntaxError *err) {
  Token *token = advance(tokens, pos);
  if (token == NULL || token->kind != kind) {
    set_error(err, token, "Unexpected token.");
    return NULL;
  }
  return token;
}

#pragma endregion

#pragma region PARSER_IMPLEMENTATION

// program -> declaration_list
static ASTNode *parse_program(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Esperado: Programa, encontrado fim da entrada.");
    return NULL;
  }
  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, PROGRAM_NODE, first->line);

  ASTNode *decl_list = parse_declaration_list(tokens, pos, err);
  if (decl_list == NULL) {
    return NULL; // O próprio declaration_list já envia erro
  }
  vecPushRight_ASTNode(&node->children, *decl_list);
  return node;
}

// declaration_list -> declaration_list declaration | declaration
static ASTNode *parse_declaration_list(Vector_Token *tokens, size_t *pos,
                                       SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Esperado: Declaração, encontrado fim da entrada.");
    return NULL;
  }
  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, DECLARATION_LIST_NODE, first->line);

  // Faz o parse da primeira declaração
  ASTNode *first_decl = parse_declaration(tokens, pos, err);
  if (first_decl == NULL) {
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *first_decl);
  free(first_decl);

  // Itera para pegar declarações seguintes
  Token *current = peek(tokens, *pos);
  while (current != NULL &&
         (is_keyword(current, "int") || is_keyword(current, "void"))) { 
    ASTNode *decl = parse_declaration(tokens, pos, err);
    if (decl == NULL) {
      return NULL;
    }
    vecPushRight_ASTNode(&node->children, *decl);
    free(decl);
    current = peek(tokens, *pos);
  }

  ASTNode *result = malloc(sizeof(ASTNode));
  *result = *node;
  return result;
}

// declaration -> var_declaration | fun_declaration
static ASTNode *parse_declaration(Vector_Token *tokens, size_t *pos,
                                  SyntaxError *err) {
  ASTNode *type_spec = parse_type_specifier(tokens, pos, err);
  if (type_spec == NULL) {
    return NULL;
  }
  Token *id_token = expect(tokens, pos, IDENTIFIER_TOKEN, err);
  if (id_token == NULL) {
    free(type_spec);
    return NULL;
  }
  Token *next_token = peek(tokens, *pos);
  if (next_token != NULL && is_lparen(next_token)) {
    return parse_fun_declaration(tokens, pos, type_spec, id_token, err);
  } else {
    return parse_var_declaration(tokens, pos, type_spec, id_token, err);
  }
}

// var_declaration -> type_specifier ID ';' | type_specifier ID '[' NUM ']' ';'
static ASTNode *parse_var_declaration(Vector_Token *tokens, size_t *pos,
                                      ASTNode *type_spec, Token *id_token,
                                      SyntaxError *err) {
  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, VAR_DECLARATION_NODE, id_token->line);

  vecPushRight_ASTNode(&node->children, *type_spec);
  free(type_spec);

  ASTNode *id_node = create_node_from_token(ID_NODE, id_token);
  vecPushRight_ASTNode(&node->children, *id_node);
  free(id_node);

  Token *next = peek(tokens, *pos);
  if (next != NULL && is_lbracket(next)) {
    advance(tokens, pos); // consome '['
    Token *num_token = expect(tokens, pos, NUMBER_TOKEN, err);
    if (num_token == NULL) {
      free(node);
      return NULL;
    }
    ASTNode *num_node = create_node_from_token(NUM_NODE, num_token);
    vecPushRight_ASTNode(&node->children, *num_node);
    free(num_node);

    Token *rbracket = peek(tokens, *pos);
    if (rbracket == NULL || !is_rbracket(rbracket)) {
      set_error(err, rbracket, "Expected ']' after array size.");
      free(node);
      return NULL;
    }
    advance(tokens, pos); // consome ']'
  }

  Token *semicolon = peek(tokens, *pos);
  if (semicolon == NULL || !is_semicolon(semicolon)) {
    set_error(err, semicolon, "Expected ';' after variable declaration.");
    free(node);
    return NULL;
  }
  advance(tokens, pos); // consome ';'

  return node;
}

// fun_declaration -> type_specifier ID '(' params ')' compound_stmt
static ASTNode *parse_fun_declaration(Vector_Token *tokens, size_t *pos,
                                      ASTNode *type_spec, Token *id_token,
                                      SyntaxError *err) {
  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, FUN_DECLARATION_NODE, id_token->line);

  vecPushRight_ASTNode(&node->children, *type_spec);
  free(type_spec);

  ASTNode *id_node = create_node_from_token(ID_NODE, id_token);
  vecPushRight_ASTNode(&node->children, *id_node);
  free(id_node);

  Token *lparen = peek(tokens, *pos);
  if (lparen == NULL || !is_lparen(lparen)) {
    set_error(err, lparen, "Expected '(' after function name.");
    free(node);
    return NULL;
  }
  advance(tokens, pos); // consome '('

  ASTNode *params = parse_params(tokens, pos, err);
  if (params == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *params);
  free(params);

  Token *rparen = peek(tokens, *pos);
  if (rparen == NULL || !is_rparen(rparen)) {
    set_error(err, rparen, "Expected ')' after parameters.");
    free(node);
    return NULL;
  }
  advance(tokens, pos); // consome ')'

  ASTNode *compound = parse_compound_stmt(tokens, pos, err);
  if (compound == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *compound);
  free(compound);

  return node;
}

// type_specifier -> 'int' | 'void'
static ASTNode *parse_type_specifier(Vector_Token *tokens, size_t *pos,
                                     SyntaxError *err) {
  Token *token = advance(tokens, pos);
  if (token == NULL || token->kind != KEYWORD_TOKEN) {
    set_error(err, token, "Expected type specifier ('int' or 'void').");
    return NULL;
  }
  if (!is_keyword(token, "int") && !is_keyword(token, "void")) {
    set_error(err, token, "Expected 'int' or 'void'.");
    return NULL;
  }
  return create_node_from_token(TYPE_SPECIFIER_NODE, token);
}

// params -> param_list | 'void'
static ASTNode *parse_params(Vector_Token *tokens, size_t *pos,
                             SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Expected parameters.");
    return NULL;
  }

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, PARAMS_NODE, first->line);

  // Verifica se é só 'void'
  if (is_keyword(first, "void")) {
    Token *next = peek(tokens, *pos + 1);
    if (next != NULL && is_rparen(next)) {
      // Se é só 'void'
      advance(tokens, pos); // consome 'void'
      ASTNode *void_node = create_node_from_token(TYPE_SPECIFIER_NODE, first);
      vecPushRight_ASTNode(&node->children, *void_node);
      free(void_node);
      return node;
    }
  }

  // Caso contrário, parse da param_list
  ASTNode *param_list = parse_param_list(tokens, pos, err);
  if (param_list == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *param_list);
  free(param_list);

  return node;
}

// param_list -> param_list ',' param | param
static ASTNode *parse_param_list(Vector_Token *tokens, size_t *pos,
                                 SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Expected parameter.");
    return NULL;
  }

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, PARAM_LIST_NODE, first->line);

  ASTNode *param = parse_param(tokens, pos, err);
  if (param == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *param);
  free(param);

  while (true) {
    Token *comma = peek(tokens, *pos);
    if (comma == NULL || !is_comma(comma)) {
      break;
    }
    advance(tokens, pos); // consome ','

    param = parse_param(tokens, pos, err);
    if (param == NULL) {
      free(node);
      return NULL;
    }
    vecPushRight_ASTNode(&node->children, *param);
    free(param);
  }

  return node;
}

// param -> type_specifier ID | type_specifier ID '[' ']'
static ASTNode *parse_param(Vector_Token *tokens, size_t *pos,
                            SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Expected parameter.");
    return NULL;
  }

  ASTNode *type_spec = parse_type_specifier(tokens, pos, err);
  if (type_spec == NULL) {
    return NULL;
  }

  Token *id_token = expect(tokens, pos, IDENTIFIER_TOKEN, err);
  if (id_token == NULL) {
    free(type_spec);
    return NULL;
  }

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, PARAM_NODE, first->line);
  vecPushRight_ASTNode(&node->children, *type_spec);
  free(type_spec);

  ASTNode *id_node = create_node_from_token(ID_NODE, id_token);
  vecPushRight_ASTNode(&node->children, *id_node);
  free(id_node);

  Token *lbracket = peek(tokens, *pos);
  if (lbracket != NULL && is_lbracket(lbracket)) {
    advance(tokens, pos); // consome '['

    Token *rbracket = peek(tokens, *pos);
    if (rbracket == NULL || !is_rbracket(rbracket)) {
      set_error(err, rbracket, "Expected ']' in array parameter.");
      free(node);
      return NULL;
    }
    advance(tokens, pos); // consome ']'

    ASTNode *empty_brackets = malloc(sizeof(ASTNode));
    create_node(empty_brackets, EMPTY_SQR_BRACKETS_NODE, lbracket->line);
    vecPushRight_ASTNode(&node->children, *empty_brackets);
    free(empty_brackets);
  }

  return node;
}

// compound_stmt -> '{' local_declarations statement_list '}'
static ASTNode *parse_compound_stmt(Vector_Token *tokens, size_t *pos,
                                    SyntaxError *err) {
  Token *lbrace = peek(tokens, *pos);
  if (lbrace == NULL || !is_lbrace(lbrace)) {
    set_error(err, lbrace, "Expected '{'.");
    return NULL;
  }
  advance(tokens, pos); // consome '{'

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, COMPOUND_STMT_NODE, lbrace->line);

  ASTNode *local_decls = parse_local_declarations(tokens, pos, err);
  if (local_decls == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *local_decls);
  free(local_decls);

  ASTNode *stmt_list = parse_statement_list(tokens, pos, err);
  if (stmt_list == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *stmt_list);
  free(stmt_list);

  Token *rbrace = peek(tokens, *pos);
  if (rbrace == NULL || !is_rbrace(rbrace)) {
    set_error(err, rbrace, "Expected '}'.");
    free(node);
    return NULL;
  }
  advance(tokens, pos); // consome '}'

  return node;
}

// local_declarations -> local_declarations var_declaration | empty
static ASTNode *parse_local_declarations(Vector_Token *tokens, size_t *pos,
                                         SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Unexpected end of input.");
    return NULL;
  }

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, LOCAL_DECLARATIONS_NODE, first->line);

  while (true) {
    Token *token = peek(tokens, *pos);
    if (token == NULL ||
        !is_keyword(token, "int") && !is_keyword(token, "void")) {
      break;
    }

    size_t saved_pos = *pos;
    ASTNode *type_spec = parse_type_specifier(tokens, pos, err);
    if (type_spec == NULL) {
      free(node);
      return NULL;
    }

    Token *id_token = expect(tokens, pos, IDENTIFIER_TOKEN, err);
    if (id_token == NULL) {
      free(type_spec);
      free(node);
      return NULL;
    }

    // Check if this is a variable declaration (not a statement)
    Token *next = peek(tokens, *pos);
    if (next != NULL && (is_semicolon(next) || is_lbracket(next))) {
      // It's a variable declaration
      ASTNode *var_decl =
          parse_var_declaration(tokens, pos, type_spec, id_token, err);
      if (var_decl == NULL) {
        free(node);
        return NULL;
      }
      vecPushRight_ASTNode(&node->children, *var_decl);
      free(var_decl);
    } else {
      // Not a var declaration, restore position and break
      *pos = saved_pos;
      free(type_spec);
      break;
    }
  }

  return node;
}

// statement_list -> statement_list statement | empty
// Left recursion eliminated: statement_list -> {statement}
static ASTNode *parse_statement_list(Vector_Token *tokens, size_t *pos,
                                     SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Unexpected end of input.");
    return NULL;
  }

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, STATEMENT_LIST_NODE, first->line);

  while (true) {
    Token *token = peek(tokens, *pos);
    if (token == NULL || is_rbrace(token)) {
      break;
    }

    ASTNode *stmt = parse_statement(tokens, pos, err);
    if (stmt == NULL) {
      free(node);
      return NULL;
    }
    vecPushRight_ASTNode(&node->children, *stmt);
    free(stmt);
  }

  return node;
}

// statement -> expression_stmt | compound_stmt | selection_stmt |
// iteration_stmt | return_stmt
static ASTNode *parse_statement(Vector_Token *tokens, size_t *pos,
                                SyntaxError *err) {
  Token *token = peek(tokens, *pos);
  if (token == NULL) {
    set_error(err, NULL, "Expected statement.");
    return NULL;
  }

  if (is_lbrace(token)) {
    return parse_compound_stmt(tokens, pos, err);
  } else if (is_keyword(token, "if")) {
    return parse_selection_stmt(tokens, pos, err);
  } else if (is_keyword(token, "while")) {
    return parse_iteration_stmt(tokens, pos, err);
  } else if (is_keyword(token, "return")) {
    return parse_return_stmt(tokens, pos, err);
  } else {
    return parse_expression_stmt(tokens, pos, err);
  }
}

// expression_stmt -> expression ';' | ';'
static ASTNode *parse_expression_stmt(Vector_Token *tokens, size_t *pos,
                                      SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Expected expression statement.");
    return NULL;
  }

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, EXPRESSION_STMT_NODE, first->line);

  if (is_semicolon(first)) {
    advance(tokens, pos); // consome ';'
    return node;
  }

  ASTNode *expr = parse_expression(tokens, pos, err);
  if (expr == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *expr);
  free(expr);

  Token *semicolon = peek(tokens, *pos);
  if (semicolon == NULL || !is_semicolon(semicolon)) {
    set_error(err, semicolon, "Expected ';' after expression.");
    free(node);
    return NULL;
  }
  advance(tokens, pos); // consome ';'

  return node;
}

// selection_stmt -> 'if' '(' expression ')' statement | 'if' '(' expression ')'
// statement 'else' statement
static ASTNode *parse_selection_stmt(Vector_Token *tokens, size_t *pos,
                                     SyntaxError *err) {
  Token *if_token = peek(tokens, *pos);
  if (if_token == NULL || !is_keyword(if_token, "if")) {
    set_error(err, if_token, "Expected 'if'.");
    return NULL;
  }
  advance(tokens, pos); // consome 'if'

  Token *lparen = peek(tokens, *pos);
  if (lparen == NULL || !is_lparen(lparen)) {
    set_error(err, lparen, "Expected '(' after 'if'.");
    return NULL;
  }
  advance(tokens, pos); // consome '('

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, SELECTION_STMT_NODE, if_token->line);

  ASTNode *condition = parse_expression(tokens, pos, err);
  if (condition == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *condition);
  free(condition);

  Token *rparen = peek(tokens, *pos);
  if (rparen == NULL || !is_rparen(rparen)) {
    set_error(err, rparen, "Expected ')' after condition.");
    free(node);
    return NULL;
  }
  advance(tokens, pos); // consome ')'

  ASTNode *then_stmt = parse_statement(tokens, pos, err);
  if (then_stmt == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *then_stmt);
  free(then_stmt);

  Token *else_token = peek(tokens, *pos);
  if (else_token != NULL && is_keyword(else_token, "else")) {
    advance(tokens, pos); // consome 'else'

    ASTNode *else_stmt = parse_statement(tokens, pos, err);
    if (else_stmt == NULL) {
      free(node);
      return NULL;
    }
    vecPushRight_ASTNode(&node->children, *else_stmt);
    free(else_stmt);
  }

  return node;
}

// iteration_stmt -> 'while' '(' expression ')' statement
static ASTNode *parse_iteration_stmt(Vector_Token *tokens, size_t *pos,
                                     SyntaxError *err) {
  Token *while_token = peek(tokens, *pos);
  if (while_token == NULL || !is_keyword(while_token, "while")) {
    set_error(err, while_token, "Expected 'while'.");
    return NULL;
  }
  advance(tokens, pos); // consome 'while'

  Token *lparen = peek(tokens, *pos);
  if (lparen == NULL || !is_lparen(lparen)) {
    set_error(err, lparen, "Expected '(' after 'while'.");
    return NULL;
  }
  advance(tokens, pos); // consome '('

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, ITERATION_STMT_NODE, while_token->line);

  ASTNode *condition = parse_expression(tokens, pos, err);
  if (condition == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *condition);
  free(condition);

  Token *rparen = peek(tokens, *pos);
  if (rparen == NULL || !is_rparen(rparen)) {
    set_error(err, rparen, "Expected ')' after condition.");
    free(node);
    return NULL;
  }
  advance(tokens, pos); // consome ')'

  ASTNode *body = parse_statement(tokens, pos, err);
  if (body == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *body);
  free(body);

  return node;
}

// return_stmt -> 'return' ';' | 'return' expression ';'
static ASTNode *parse_return_stmt(Vector_Token *tokens, size_t *pos,
                                  SyntaxError *err) {
  Token *return_token = peek(tokens, *pos);
  if (return_token == NULL || !is_keyword(return_token, "return")) {
    set_error(err, return_token, "Expected 'return'.");
    return NULL;
  }
  advance(tokens, pos); // consome 'return'

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, RETURN_STMT_NODE, return_token->line);

  Token *next = peek(tokens, *pos);
  if (next != NULL && !is_semicolon(next)) {
    ASTNode *expr = parse_expression(tokens, pos, err);
    if (expr == NULL) {
      free(node);
      return NULL;
    }
    vecPushRight_ASTNode(&node->children, *expr);
    free(expr);
  }

  Token *semicolon = peek(tokens, *pos);
  if (semicolon == NULL || !is_semicolon(semicolon)) {
    set_error(err, semicolon, "Expected ';' after return.");
    free(node);
    return NULL;
  }
  advance(tokens, pos); // consome ';'

  return node;
}

// expression -> var '=' expression | simple_expression
static ASTNode *parse_expression(Vector_Token *tokens, size_t *pos,
                                 SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Expected expression.");
    return NULL;
  }

  size_t saved_pos = *pos;

  // Try to parse as var '=' expression
  if (first->kind == IDENTIFIER_TOKEN) {
    ASTNode *var = parse_var(tokens, pos, err);
    if (var != NULL) {
      Token *eq = peek(tokens, *pos);
      if (eq != NULL && is_special(eq, "=")) {
        advance(tokens, pos); // consome '='

        ASTNode *node = malloc(sizeof(ASTNode));
        create_node(node, EXPRESSION_NODE, first->line);
        vecPushRight_ASTNode(&node->children, *var);
        free(var);

        ASTNode *rhs = parse_expression(tokens, pos, err);
        if (rhs == NULL) {
          free(node);
          return NULL;
        }
        vecPushRight_ASTNode(&node->children, *rhs);
        free(rhs);

        return node;
      } else {
        // Not an assignment, restore and parse as simple_expression
        free(var);
        *pos = saved_pos;
      }
    } else {
      *pos = saved_pos;
    }
  }

  return parse_simple_expression(tokens, pos, err);
}

// var -> ID | ID '[' expression ']'
static ASTNode *parse_var(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  Token *id_token = expect(tokens, pos, IDENTIFIER_TOKEN, err);
  if (id_token == NULL) {
    return NULL;
  }

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, VAR_NODE, id_token->line);

  ASTNode *id_node = create_node_from_token(ID_NODE, id_token);
  vecPushRight_ASTNode(&node->children, *id_node);
  free(id_node);

  Token *lbracket = peek(tokens, *pos);
  if (lbracket != NULL && is_lbracket(lbracket)) {
    advance(tokens, pos); // consome '['

    ASTNode *index_expr = parse_expression(tokens, pos, err);
    if (index_expr == NULL) {
      free(node);
      return NULL;
    }
    vecPushRight_ASTNode(&node->children, *index_expr);
    free(index_expr);

    Token *rbracket = peek(tokens, *pos);
    if (rbracket == NULL || !is_rbracket(rbracket)) {
      set_error(err, rbracket, "Expected ']' after array index.");
      free(node);
      return NULL;
    }
    advance(tokens, pos); // consome ']'
  }

  return node;
}

// simple_expression -> additive_expression relop additive_expression |
// additive_expression
static ASTNode *parse_simple_expression(Vector_Token *tokens, size_t *pos,
                                        SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Expected expression.");
    return NULL;
  }

  ASTNode *left = parse_additive_expression(tokens, pos, err);
  if (left == NULL) {
    return NULL;
  }

  Token *op = peek(tokens, *pos);
  if (op != NULL && op->kind == SPECIAL_TOKEN &&
      (is_special(op, "<=") || is_special(op, "<") || is_special(op, ">") ||
       is_special(op, ">=") || is_special(op, "==") || is_special(op, "!="))) {
    advance(tokens, pos); // consome relop

    ASTNode *node = malloc(sizeof(ASTNode));
    create_node(node, SIMPLE_EXPRESSION_NODE, first->line);
    vecPushRight_ASTNode(&node->children, *left);
    free(left);

    ASTNode *op_node = create_node_from_token(RELOP_NODE, op);
    vecPushRight_ASTNode(&node->children, *op_node);
    free(op_node);

    ASTNode *right = parse_additive_expression(tokens, pos, err);
    if (right == NULL) {
      free(node);
      return NULL;
    }
    vecPushRight_ASTNode(&node->children, *right);
    free(right);

    return node;
  }

  return left;
}

// additive_expression -> additive_expression addop term | term
static ASTNode *parse_additive_expression(Vector_Token *tokens, size_t *pos,
                                          SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Expected term.");
    return NULL;
  }

  ASTNode *left = parse_term(tokens, pos, err);
  if (left == NULL) {
    return NULL;
  }

  while (true) {
    Token *op = peek(tokens, *pos);
    if (op == NULL || !is_special(op, "+") && !is_special(op, "-")) {
      break;
    }
    advance(tokens, pos); // consome addop

    ASTNode *node = malloc(sizeof(ASTNode));
    create_node(node, ADDITIVE_EXPRESSION_NODE, first->line);
    vecPushRight_ASTNode(&node->children, *left);
    free(left);

    ASTNode *op_node = create_node_from_token(ADDOP_NODE, op);
    vecPushRight_ASTNode(&node->children, *op_node);
    free(op_node);

    ASTNode *right = parse_term(tokens, pos, err);
    if (right == NULL) {
      free(node);
      return NULL;
    }
    vecPushRight_ASTNode(&node->children, *right);
    free(right);

    left = node;
  }

  return left;
}

// term -> term mulop factor | factor
static ASTNode *parse_term(Vector_Token *tokens, size_t *pos,
                           SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Expected factor.");
    return NULL;
  }

  ASTNode *left = parse_factor(tokens, pos, err);
  if (left == NULL) {
    return NULL;
  }

  while (true) {
    Token *op = peek(tokens, *pos);
    if (op == NULL || !is_special(op, "*") && !is_special(op, "/")) {
      break;
    }
    advance(tokens, pos); // consome mulop

    ASTNode *node = malloc(sizeof(ASTNode));
    create_node(node, TERM_NODE, first->line);
    vecPushRight_ASTNode(&node->children, *left);
    free(left);

    ASTNode *op_node = create_node_from_token(MULOP_NODE, op);
    vecPushRight_ASTNode(&node->children, *op_node);
    free(op_node);

    ASTNode *right = parse_factor(tokens, pos, err);
    if (right == NULL) {
      free(node);
      return NULL;
    }
    vecPushRight_ASTNode(&node->children, *right);
    free(right);

    left = node;
  }

  return left;
}

// factor -> '(' expression ')' | var | call | NUM
static ASTNode *parse_factor(Vector_Token *tokens, size_t *pos,
                             SyntaxError *err) {
  Token *token = peek(tokens, *pos);
  if (token == NULL) {
    set_error(err, NULL, "Expected factor.");
    return NULL;
  }

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, FACTOR_NODE, token->line);

  if (is_lparen(token)) {
    advance(tokens, pos); // consome '('

    ASTNode *expr = parse_expression(tokens, pos, err);
    if (expr == NULL) {
      free(node);
      return NULL;
    }
    vecPushRight_ASTNode(&node->children, *expr);
    free(expr);

    Token *rparen = peek(tokens, *pos);
    if (rparen == NULL || !is_rparen(rparen)) {
      set_error(err, rparen, "Expected ')' after expression.");
      free(node);
      return NULL;
    }
    advance(tokens, pos); // consome ')'

    return node;
  } else if (token->kind == NUMBER_TOKEN) {
    advance(tokens, pos);
    ASTNode *num_node = create_node_from_token(NUM_NODE, token);
    vecPushRight_ASTNode(&node->children, *num_node);
    free(num_node);
    return node;
  } else if (token->kind == IDENTIFIER_TOKEN) {
    Token *next = peek(tokens, *pos + 1);
    if (next != NULL && is_lparen(next)) {
      advance(tokens, pos); // consome ID
      ASTNode *call = parse_call(tokens, pos, token, err);
      if (call == NULL) {
        free(node);
        return NULL;
      }
      vecPushRight_ASTNode(&node->children, *call);
      free(call);
      return node;
    } else {
      ASTNode *var = parse_var(tokens, pos, err);
      if (var == NULL) {
        free(node);
        return NULL;
      }
      vecPushRight_ASTNode(&node->children, *var);
      free(var);
      return node;
    }
  } else {
    set_error(err, token, "Expected factor (number, variable, or call).");
    free(node);
    return NULL;
  }
}

// call -> ID '(' args ')'
static ASTNode *parse_call(Vector_Token *tokens, size_t *pos, Token *id_token,
                           SyntaxError *err) {
  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, CALL_NODE, id_token->line);

  ASTNode *id_node = create_node_from_token(ID_NODE, id_token);
  vecPushRight_ASTNode(&node->children, *id_node);
  free(id_node);

  Token *lparen = peek(tokens, *pos);
  if (lparen == NULL || !is_lparen(lparen)) {
    set_error(err, lparen, "Expected '(' for function call.");
    free(node);
    return NULL;
  }
  advance(tokens, pos); // consome '('

  ASTNode *args = parse_args(tokens, pos, err);
  if (args == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *args);
  free(args);

  Token *rparen = peek(tokens, *pos);
  if (rparen == NULL || !is_rparen(rparen)) {
    set_error(err, rparen, "Expected ')' after arguments.");
    free(node);
    return NULL;
  }
  advance(tokens, pos); // consome ')'

  return node;
}

// args -> arg_list | empty
static ASTNode *parse_args(Vector_Token *tokens, size_t *pos,
                           SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Expected arguments or ')'.");
    return NULL;
  }

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, ARGS_NODE, first->line);

  if (is_rparen(first)) {
    // Empty args
    return node;
  }

  ASTNode *arg_list = parse_arg_list(tokens, pos, err);
  if (arg_list == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *arg_list);
  free(arg_list);

  return node;
}

// arg_list -> arg_list ',' expression | expression
// Left recursion eliminated: arg_list -> expression {',' expression}
static ASTNode *parse_arg_list(Vector_Token *tokens, size_t *pos,
                               SyntaxError *err) {
  Token *first = peek(tokens, *pos);
  if (first == NULL) {
    set_error(err, NULL, "Expected expression.");
    return NULL;
  }

  ASTNode *node = malloc(sizeof(ASTNode));
  create_node(node, ARG_LIST_NODE, first->line);

  ASTNode *expr = parse_expression(tokens, pos, err);
  if (expr == NULL) {
    free(node);
    return NULL;
  }
  vecPushRight_ASTNode(&node->children, *expr);
  free(expr);

  while (true) {
    Token *comma = peek(tokens, *pos);
    if (comma == NULL || !is_comma(comma)) {
      break;
    }
    advance(tokens, pos); // consome ','

    expr = parse_expression(tokens, pos, err);
    if (expr == NULL) {
      free(node);
      return NULL;
    }
    vecPushRight_ASTNode(&node->children, *expr);
    free(expr);
  }

  return node;
}

#pragma endregion

#pragma region ANALISADOR_SINTATICO

// Função principal para gerar a AST a partir dos tokens
bool generateAST(Vector_Token tokens, ASTNode *out, SyntaxError *err) {
  // Filtra comentários para evitar interferência na análise sintática
  Vector_Token filtered = vecCreateEmpty_Token();
  for (size_t i = 0; i < vecLength_Token(&tokens); i++) {
    Token *t = vecIndex_Token(&tokens, i);
    if (t->kind == LEFT_COMMENT_TOKEN || t->kind == RIGHT_COMMENT_TOKEN) {
      continue;
    }
    Token copy = *t;
    copy.content = vecDuplicate_char(&t->content);
    vecPushRight_Token(&filtered, copy);
  }

  size_t pos = 0;
  ASTNode *root = parse_program(&filtered, &pos, err); // Começa a análise aqui
  if (root == NULL) {
    vecFree_Token(&filtered); // Se houver erro, libera memória e retorna falso
    return false;
  }

  // Verifica que não há tokens restantes após o programa.
  Token *remaining = peek(&filtered, pos);
  if (remaining != NULL) {
    set_error(err, remaining, "Tokens inesperados após o fim do programa.");
    free(root);
    vecFree_Token(&filtered);
    return false;
  }

  *out = *root;
  free(root);
  vecFree_Token(&filtered);
  return true; // Análise retorna verdadeira se não houver erros.
}

#pragma endregion
