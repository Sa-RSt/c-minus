#include "syntactic.h"
#include "char_vector.h"
#include "vector.h"

#define NODEKINDGETTER(x) x.kind
#define ATTRNAMEGETTER(x) (&x.name)
#define SELFGETTER(x) (x)
#define DIFF(x, y) (x - y)
DECLARE_VECTOR_TYPE(ASTNode, ASTNodeKind, NODEKINDGETTER, DIFF)

DECLARE_VECTOR_TYPE(Attribute, Vector_char *, ATTRNAMEGETTER, charVecStrcmp)

DECLARE_VECTOR_TYPE(uint8_t, uint8_t, SELFGETTER, DIFF)
