#include "codegen.h"
#include "char_vector.h"
#include "semantic.h"
#include "vector.h"
#include <assert.h>
#include <stdlib.h>

#define REGISTER_NAME_GETTER(x) (&x.name)

RegisterPool globalRegisterPool;
Vector_char literalAdd, literalSub, literalMul, literalDiv, literalRem,
    literalGt, literalGte, literalLt, literalLte, literalEq, literalNe;

DECLARE_VECTOR_TYPE(Register, Vector_char *, REGISTER_NAME_GETTER,
                    charVecStrcmp)

#define CODEGEN_RESULT_GETTER(x) (x.result)
#define DIFF(x, y) (x - y)
DECLARE_VECTOR_TYPE(Codegen, Register *, CODEGEN_RESULT_GETTER, DIFF)

void codegenInit() {
  globalRegisterPool.registers = vecCreateEmpty_Register();
  literalAdd = charVecFromCArray("+");
  literalSub = charVecFromCArray("-");
  literalMul = charVecFromCArray("*");
  literalDiv = charVecFromCArray("/");
  literalRem = charVecFromCArray("%");
  literalGt = charVecFromCArray(">");
  literalGte = charVecFromCArray(">=");
  literalLt = charVecFromCArray("<");
  literalLte = charVecFromCArray("<=");
  literalEq = charVecFromCArray("==");
  literalNe = charVecFromCArray("!=");
}

InstructionOperator iOperatorFromString(Vector_char *text) {
  if (charVecStrcmp(text, &literalAdd) == 0) {
    return ADD_IOP;
  }
  if (charVecStrcmp(text, &literalSub) == 0) {
    return SUB_IOP;
  }
  if (charVecStrcmp(text, &literalMul) == 0) {
    return MUL_IOP;
  }
  if (charVecStrcmp(text, &literalDiv) == 0) {
    return DIV_IOP;
  }
  if (charVecStrcmp(text, &literalRem) == 0) {
    return REM_IOP;
  }
  if (charVecStrcmp(text, &literalGt) == 0) {
    return GT_IOP;
  }
  if (charVecStrcmp(text, &literalGte) == 0) {
    return GTE_IOP;
  }
  if (charVecStrcmp(text, &literalLt) == 0) {
    return LT_IOP;
  }
  if (charVecStrcmp(text, &literalLte) == 0) {
    return LTE_IOP;
  }
  if (charVecStrcmp(text, &literalEq) == 0) {
    return EQ_IOP;
  }
  if (charVecStrcmp(text, &literalNe) == 0) {
    return NE_IOP;
  }
  assert(0); // erro do analisador sintático
}

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

Register *getTemporaryRegisterWithPrefix(char prefix) {

  Vector_char v =
      STRINGIFY_TO_CHAR_VEC(uint32_t, globalRegisterPool.temporaryRegisters);
  vecPushLeft_char(&v, prefix);
  globalRegisterPool.temporaryRegisters++;
  Register *reg = getRegister(&v);
  vecFree_char(&v);
  return reg;
}

Register *getTemporaryRegister() { return getTemporaryRegisterWithPrefix('t'); }

Register *getRegisterNamedAfter(Symbol *sym) {
  Vector_char regName = vecCreateEmpty_char();
  for (size_t i = 0; i < vecLength_ScopeEntry(&sym->scope); i++) {
    ScopeEntry *se = vecIndex_ScopeEntry(&sym->scope, i);
    vecExtend_char(&regName, &se->name);
    charVecAppendCArray(&regName, "__");
  }
  vecExtend_char(&regName, &sym->name);
  Register *reg = getRegister(&regName);
  vecFree_char(&regName);
  return reg;
}

Codegen createCodegenObj() {
  Codegen cg;
  cg.instructions = vecCreateEmpty_Instruction();
  cg.result = NULL;
  return cg;
}

Instruction createArithmeticInstruction(Register *dst, Register *lhs,
                                        InstructionOperator op, Register *rhs) {
  Instruction i;
  i.destination = dst;
  i.leftOperand = lhs;
  i.op = op;
  i.rightOperand = rhs;
  i.kind = ARITH_INSTRUCTION;
  i.immediate = 0;
  return i;
}

Instruction createMoveInstruction(Register *dst, Register *src) {
  Instruction i;
  i.kind = ARITH_INSTRUCTION;
  i.op = NO_OPERATOR_IOP;
  i.leftOperand = src;
  i.rightOperand = NULL;
  i.destination = dst;
  i.immediate = 0;
  return i;
}

Instruction createArrayLoadInstruction(Register *dst, Register *array,
                                       Register *index) {
  Instruction i;
  i.kind = ARRAY_LOAD_INSTRUCTION;
  i.op = NO_OPERATOR_IOP;
  i.leftOperand = array;
  i.rightOperand = index;
  i.destination = dst;
  i.immediate = 0;
  return i;
}

Instruction createArrayStoreInstruction(Register *array, Register *index,
                                        Register *src) {
  Instruction i;
  i.kind = ARRAY_STORE_INSTRUCTION;
  i.op = NO_OPERATOR_IOP;
  i.destination = array;
  i.leftOperand = index;
  i.rightOperand = src;
  i.immediate = 0;
  return i;
}

Instruction createImmediateLoadInstruction(Register *dst, int64_t imm) {
  Instruction i;
  i.kind = IMMEDIATE_LOAD_INSTRUCTION;
  i.op = NO_OPERATOR_IOP;
  i.destination = dst;
  i.immediate = imm;
  i.leftOperand = NULL;
  i.rightOperand = NULL;
  return i;
}

Instruction createParamPushInstruction(Register *src) {
  Instruction i;
  i.kind = PARAM_PUSH_INSTRUCTION;
  i.op = NO_OPERATOR_IOP;
  i.destination = NULL;
  i.immediate = 0;
  i.leftOperand = src;
  i.rightOperand = NULL;
  return i;
}

Instruction createCallInstruction(Register *dest, Register *fun) {
  Instruction i;
  i.kind = CALL_INSTRUCTION;
  i.op = NO_OPERATOR_IOP;
  i.destination = dest;
  i.immediate = 0;
  i.leftOperand = fun;
  i.rightOperand = NULL;
  return i;
}

Instruction createLabel(Register **rOut) {
  *rOut = getTemporaryRegisterWithPrefix('L');
  Instruction i;
  i.kind = LABEL_INSTRUCTION;
  i.op = NO_OPERATOR_IOP;
  i.immediate = 0;
  i.leftOperand = i.rightOperand = NULL;
  i.destination = *rOut;
  return i;
}

Instruction createIfNotInstruction(Register *cond, Register *label) {
  Instruction i;
  i.kind = IF_NOT_INSTRUCTION;
  i.destination = NULL;
  i.leftOperand = cond;
  i.rightOperand = label;
  i.op = NO_OPERATOR_IOP;
  i.immediate = 0;
  return i;
}

Instruction createIfInstruction(Register *cond, Register *label) {
  Instruction i;
  i.kind = IF_NOT_INSTRUCTION;
  i.destination = NULL;
  i.leftOperand = cond;
  i.rightOperand = label;
  i.op = NO_OPERATOR_IOP;
  i.immediate = 0;
  return i;
}

Instruction createJumpInstruction(Register *label) {
  Instruction i;
  i.kind = JUMP_INSTRUCTION;
  i.destination = i.leftOperand = NULL;
  i.rightOperand = label;
  i.op = NO_OPERATOR_IOP;
  i.immediate = 0;
  return i;
}
