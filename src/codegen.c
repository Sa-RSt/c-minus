#include "codegen.h"
#include "char_vector.h"
#include "vector.h"
#include <stdlib.h>

#define REGISTER_NAME_GETTER(x) (&x.name)

RegisterPool globalRegisterPool;

DECLARE_VECTOR_TYPE(Register, Vector_char *, REGISTER_NAME_GETTER,
                    charVecStrcmp)

void codegenInit() { globalRegisterPool.registers = vecCreateEmpty_Register(); }

Register *getRegister(Vector_char *name) {
  Register *existing = vecLookup_Register(&globalRegisterPool.registers, name);
  if (existing != NULL) {
    return existing;
  }
  Register reg;
  reg.name = vecDuplicate_char(name);
  vecPushRight_Register(&globalRegisterPool.registers, reg);
  return getRegister(name);
}

Register *getRegisterCStr(const char *name) {
  Vector_char v = charVecFromCArray(name);
  Register *reg = getRegister(&v);
  vecFree_char(&v);
  return reg;
}

Register *getTemporaryRegister(Codegen *cg) {
  Vector_char v = STRINGIFY_TO_CHAR_VEC(uint32_t, cg->temporaryRegisters);
  vecPushLeft_char(&v, 't');
  cg->temporaryRegisters++;
  Register *reg = getRegister(&v);
  vecFree_char(&v);
  return reg;
}
