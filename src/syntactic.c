#include "syntactic.h"
#include <stdlib.h>
#include <string.h>

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
statement -> expression_stmt | compound_stmt | selection_stmt | iteration_stmt | return_stmt
expression_stmt -> expression ';' | ';'
selection_stmt -> 'if' '(' expression ')' statement | 'if' '(' expression ')' statement 'else' statement
iteration_stmt -> 'while' '(' expression ')' statement
return_stmt -> 'return' ';' | 'return' expression ';'
expression -> var '=' expression | simple_expression
var -> ID | ID '[' expression ']'
simple_expression -> additive_expression relop additive_expression | additive_expression
relop -> '<=' | '<' | '>' | '>=' | '==' | '!='
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
static ASTNode* parse_program(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_declaration_list(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_declaration(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_var_declaration(Vector_Token *tokens, size_t *pos, ASTNode *type_spec, Token *id_token, SyntaxError *err);
static ASTNode* parse_fun_declaration(Vector_Token *tokens, size_t *pos, ASTNode *type_spec, Token *id_token, SyntaxError *err);
static ASTNode* parse_type_specifier(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_params(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_param_list(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_param(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_compound_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_local_declarations(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_statement_list(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_statement(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_expression_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_selection_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_iteration_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_return_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_expression(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_var(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_simple_expression(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_additive_expression(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_term(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_factor(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_call(Vector_Token *tokens, size_t *pos, Token *id_token, SyntaxError *err);
static ASTNode* parse_args(Vector_Token *tokens, size_t *pos, SyntaxError *err);
static ASTNode* parse_arg_list(Vector_Token *tokens, size_t *pos, SyntaxError *err);
#pragma endregion

#pragma region UTILITY_FUNCTIONS

// Verifica o token atual (impede overflow)
static Token* peek(Vector_Token *tokens, size_t pos) {
  if (pos >= vecLength_Token(tokens)) {
    return NULL;
  }
  return vecIndex_Token(tokens, pos);
}

static bool charvec_equals_cstr(const Vector_char *vec, const char *cstr) {
  Vector_char tmp = charVecFromCArray(cstr);
  bool eq = (charVecStrcmp(vec, &tmp) == 0);
  vecFree_char(&tmp);
  return eq;
}

// Verifica se token corresponde a keyword
static bool is_keyword(Token *token, const char *keyword) {
  return token != NULL && token->kind == KEYWORD_TOKEN &&
         charvec_equals_cstr(&token->content, keyword);
}

// Helper to check if token is a special character
static bool is_special(Token *token, const char *special) {
  return token != NULL && token->kind == SPECIAL_TOKEN &&
         charvec_equals_cstr(&token->content, special);
}

// Helper to create an AST node
static ASTNode* create_node(ASTNodeKind kind, uint32_t line) {
  ASTNode *node = (ASTNode*)malloc(sizeof(ASTNode));
  node->kind = kind;
  node->line = line;
  node->content = vecCreateEmpty_char();
  node->attributes = vecCreateEmpty_Attribute();
  node->children = vecCreateEmpty_ASTNode();
  return node;
}

// Helper to create node with content from token
static ASTNode* create_node_from_token(ASTNodeKind kind, Token *token) {
  ASTNode *node = create_node(kind, token->line);
  node->content = vecDuplicate_char(&token->content);
  return node;
}

// Helper to set error
static void set_error(SyntaxError *err, Token *token, const char *message) {
  if (token) {
    err->line = token->line;
  } else {
    err->line = 0;
  }
  err->filename = __FILE__;
  err->message = (char*)message;
  err->caused = NULL;
}

#pragma endregion

#pragma region PARSER_IMPLEMENTATION

// program -> declaration_list
static ASTNode* parse_program(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse the whole program starting at *pos and return a PROGRAM_NODE root.
  // Should parse a declaration_list and attach it as the single child of the root.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// declaration_list -> declaration_list declaration | declaration
static ASTNode* parse_declaration_list(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse one or more declarations, producing a DECLARATION_LIST_NODE whose
  // children are the individual declaration nodes parsed in sequence.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// declaration -> var_declaration | fun_declaration
// Both start with type_specifier ID, so we parse that first then decide
static ASTNode* parse_declaration(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse a single declaration. Decide between var_declaration and fun_declaration
  // after parsing type_specifier and identifier lookahead.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// var_declaration -> type_specifier ID ';' | type_specifier ID '[' NUM ']' ';'
static ASTNode* parse_var_declaration(Vector_Token *tokens, size_t *pos, ASTNode *type_spec, Token *id_token, SyntaxError *err) {
  // TODO: Build a VAR_DECLARATION_NODE with children: type_spec, ID, optional array size, then consume ';'.
  (void)tokens;
  (void)pos;
  (void)type_spec;
  (void)id_token;
  (void)err;
  return NULL;
}

// fun_declaration -> type_specifier ID '(' params ')' compound_stmt
static ASTNode* parse_fun_declaration(Vector_Token *tokens, size_t *pos, ASTNode *type_spec, Token *id_token, SyntaxError *err) {
  // TODO: Build a FUN_DECLARATION_NODE with children: type_spec, ID, params, compound_stmt. Consume surrounding parentheses.
  (void)tokens;
  (void)pos;
  (void)type_spec;
  (void)id_token;
  (void)err;
  return NULL;
}

// type_specifier -> 'int' | 'void'
static ASTNode* parse_type_specifier(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Consume 'int' or 'void' and return a TYPE_SPECIFIER_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// params -> param_list | 'void'
static ASTNode* parse_params(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Handle params as either 'void' (possibly empty) or a param_list. Return a PARAMS_NODE with appropriate child.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// param_list -> param_list ',' param | param
// Left recursion eliminated: param_list -> param {',' param}
static ASTNode* parse_param_list(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse one or more params separated by commas, building a PARAM_LIST_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// param -> type_specifier ID | type_specifier ID '[' ']'
static ASTNode* parse_param(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse a parameter (type_specifier + ID, with optional []), returning a PARAM_NODE with children.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// compound_stmt -> '{' local_declarations statement_list '}'
static ASTNode* parse_compound_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse a compound statement: '{' local_declarations statement_list '}'. Return COMPOUND_STMT_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// local_declarations -> local_declarations var_declaration | empty
// Left recursion eliminated: local_declarations -> {var_declaration}
static ASTNode* parse_local_declarations(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse zero or more variable declarations that appear inside a compound statement.
  // Stop when the next tokens do not form a var_declaration. Return LOCAL_DECLARATIONS_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// statement_list -> statement_list statement | empty
// Left recursion eliminated: statement_list -> {statement}
static ASTNode* parse_statement_list(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse zero or more statements until a closing '}' or end of input. Return STATEMENT_LIST_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// statement -> expression_stmt | compound_stmt | selection_stmt | iteration_stmt | return_stmt
static ASTNode* parse_statement(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Dispatch to the correct statement parser based on the current token.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// expression_stmt -> expression ';' | ';'
static ASTNode* parse_expression_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse an expression followed by ';' or a lone ';'. Return EXPRESSION_STMT_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// selection_stmt -> 'if' '(' expression ')' statement | 'if' '(' expression ')' statement 'else' statement
static ASTNode* parse_selection_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse an if/else statement with condition in parentheses and then/else branches. Return SELECTION_STMT_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// iteration_stmt -> 'while' '(' expression ')' statement
static ASTNode* parse_iteration_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse a while loop: 'while' '(' expression ')' statement. Return ITERATION_STMT_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// return_stmt -> 'return' ';' | 'return' expression ';'
static ASTNode* parse_return_stmt(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse 'return' with optional expression followed by ';'. Return RETURN_STMT_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// expression -> var '=' expression | simple_expression
static ASTNode* parse_expression(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse assignment (var '=' expression) or fallback to simple_expression. Return EXPRESSION_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// var -> ID | ID '[' expression ']'
static ASTNode* parse_var(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse a variable reference: ID with optional '[' expression ']'. Return VAR_NODE with children.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// simple_expression -> additive_expression relop additive_expression | additive_expression
static ASTNode* parse_simple_expression(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse additive_expression with optional relop additive_expression. Return SIMPLE_EXPRESSION_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// additive_expression -> additive_expression addop term | term
// Left recursion eliminated: additive_expression -> term {addop term}
static ASTNode* parse_additive_expression(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse term { (+|-) term } and return ADDITIVE_EXPRESSION_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// term -> term mulop factor | factor
// Left recursion eliminated: term -> factor {mulop factor}
static ASTNode* parse_term(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse factor { (*|/) factor } and return TERM_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// factor -> '(' expression ')' | var | call | NUM
static ASTNode* parse_factor(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse factor: '(' expression ')', var, call, or NUM. Return FACTOR_NODE with appropriate child.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// call -> ID '(' args ')'
static ASTNode* parse_call(Vector_Token *tokens, size_t *pos, Token *id_token, SyntaxError *err) {
  // TODO: Parse a function call: ID '(' args ')'. Return CALL_NODE with ID and args children.
  (void)tokens;
  (void)pos;
  (void)id_token;
  (void)err;
  return NULL;
}

// args -> arg_list | empty
static ASTNode* parse_args(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse args: either empty or an arg_list. Return ARGS_NODE with child.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

// arg_list -> arg_list ',' expression | expression
// Left recursion eliminated: arg_list -> expression {',' expression}
static ASTNode* parse_arg_list(Vector_Token *tokens, size_t *pos, SyntaxError *err) {
  // TODO: Parse one or more expressions separated by commas, returning ARG_LIST_NODE.
  (void)tokens;
  (void)pos;
  (void)err;
  return NULL;
}

#pragma endregion

#pragma region ANALISADOR_SINTATICO

// Main entry point for syntactic analysis
bool generateAST(Vector_Token tokens, ASTNode *out, SyntaxError *err) {
  // TODO: Entry point: initialize position, call parse_program, ensure all tokens consumed,
  // and write the resulting AST root into *out. Return true on success, false on error.
  (void)tokens;
  (void)out;
  (void)err;
  return false;
}

#pragma endregion