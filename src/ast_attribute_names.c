#include "ast_attribute_names.h"
#include "char_vector.h"

void initializeAstAttributes() {
  NODE__CHILDREN = charVecFromCArray("children");
  VAR_FUN_DECLARATION__SYMBOL = charVecFromCArray("symbol");
}
