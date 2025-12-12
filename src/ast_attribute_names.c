#include "ast_attribute_names.h"
#include "char_vector.h"

Vector_char VAR_FUN_DECLARATION__SYMBOL;

void initializeAstAttributes() {
  VAR_FUN_DECLARATION__SYMBOL = charVecFromCArray("symbol");
}
