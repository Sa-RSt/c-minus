#include "codegen.h"
#include "char_vector.h"
#include "semantic.h"
#include "stringify.h"
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

DECLARE_STRINGIFY_FUNCTION(InstructionOperator, iop) {
  switch (iop) {
  case ADD_IOP:
    STRINGIFY_PUT("+");
    break;
  case SUB_IOP:
    STRINGIFY_PUT("-");
    break;
  case MUL_IOP:
    STRINGIFY_PUT("*");
    break;
  case DIV_IOP:
    STRINGIFY_PUT("/");
    break;
  case REM_IOP:
    STRINGIFY_PUT("%");
    break;
  case GT_IOP:
    STRINGIFY_PUT(">");
    break;
  case GTE_IOP:
    STRINGIFY_PUT(">=");
    break;
  case LT_IOP:
    STRINGIFY_PUT("<");
    break;
  case LTE_IOP:
    STRINGIFY_PUT(">=");
    break;
  case EQ_IOP:
    STRINGIFY_PUT("==");
    break;
  case NE_IOP:
    STRINGIFY_PUT("!=");
    break;
  case NO_OPERATOR_IOP:
    STRINGIFY_PUT("NO_OPERATOR_IOP");
    break;
  }
}

DECLARE_STRINGIFY_FUNCTION(Instruction, ins) {
  switch (ins.kind) {
  case ARITH_INSTRUCTION:
    STRINGIFY_PUT("  ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.destination->name);
    STRINGIFY_PUT(" = ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.leftOperand->name);
    if (ins.op != NO_OPERATOR_IOP) {
      STRINGIFY_PUT(" ");
      STRINGIFY_PUT_VALUE(InstructionOperator, ins.op);
      STRINGIFY_PUT(" ");
      STRINGIFY_PUT_VALUE(Vector_char, ins.rightOperand->name);
    }
    break;
  case LABEL_INSTRUCTION:
    STRINGIFY_PUT_VALUE(Vector_char, ins.destination->name);
    STRINGIFY_PUT(":");
    break;
  case JUMP_INSTRUCTION:
    STRINGIFY_PUT("  ");
    STRINGIFY_PUT("goto ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.rightOperand->name);
    break;
  case IF_INSTRUCTION:
    STRINGIFY_PUT("  ");
    STRINGIFY_PUT("if ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.leftOperand->name);
    STRINGIFY_PUT(" goto ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.rightOperand->name);
    break;
  case IF_NOT_INSTRUCTION:
    STRINGIFY_PUT("  ");
    STRINGIFY_PUT("if_not ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.leftOperand->name);
    STRINGIFY_PUT(" goto ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.rightOperand->name);
    break;
  case RETURN_INSTRUCTION:
    STRINGIFY_PUT("  ");
    STRINGIFY_PUT("return");
    break;
  case ARRAY_LOAD_INSTRUCTION:
    STRINGIFY_PUT("  ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.destination->name);
    STRINGIFY_PUT(" = ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.leftOperand->name);
    STRINGIFY_PUT("[");
    STRINGIFY_PUT_VALUE(Vector_char, ins.rightOperand->name);
    STRINGIFY_PUT("]");
    break;
  case ARRAY_STORE_INSTRUCTION:
    STRINGIFY_PUT("  ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.destination->name);
    STRINGIFY_PUT("[");
    STRINGIFY_PUT_VALUE(Vector_char, ins.leftOperand->name);
    STRINGIFY_PUT("]");
    STRINGIFY_PUT(" = ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.rightOperand->name);
    break;
  case IMMEDIATE_LOAD_INSTRUCTION:
    STRINGIFY_PUT("  ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.destination->name);
    STRINGIFY_PUT(" = ");
    STRINGIFY_PUT_VALUE(int64_t, ins.immediate);
    break;
  case PARAM_PUSH_INSTRUCTION:
    STRINGIFY_PUT("  ");
    STRINGIFY_PUT("param_push ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.leftOperand->name);
    break;
  case PARAM_POP_INSTRUCTION:
    STRINGIFY_PUT("  ");
    STRINGIFY_PUT("param_pop ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.destination->name);
    break;
  case CALL_INSTRUCTION:
    STRINGIFY_PUT("  ");
    STRINGIFY_PUT("call ");
    STRINGIFY_PUT_VALUE(Vector_char, ins.leftOperand->name);
    break;
  }
}

DECLARE_STRINGIFY_FUNCTION(Vector_Instruction, vins) {
  size_t n = vecLength_Instruction(&vins);
  for (size_t i = 0; i < n; i++) {
    STRINGIFY_PUT_VALUE(Instruction, *vecIndex_Instruction(&vins, i));
    STRINGIFY_PUT("\n");
  }
}

#define KIND_GETTER(X) (X.kind)
DECLARE_VECTOR_TYPE(Instruction, InstructionKind, KIND_GETTER, DIFF)

void codegenInit() {
  globalRegisterPool.registers = NULL;
  globalRegisterPool.temporaryRegisters = 0;
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

static Register *regLookupByName(Vector_char *name) {
  Register *it = globalRegisterPool.registers;
  while (it != NULL) {
    if (charVecStrcmp(&it->name, name) == 0) {
      return it;
    }
    it = it->next;
  }
  return NULL;
}

Register *getRegister(Vector_char *name) {
  Register *existing = regLookupByName(name);
  if (existing != NULL) {
    return existing;
  }
  Register *reg = malloc(sizeof(Register));
  reg->name = vecDuplicate_char(name);
  reg->next = globalRegisterPool.registers;
  globalRegisterPool.registers = reg;
  return getRegister(name);
}

Register *getRegisterCStr(const char *name) {
  Vector_char v = charVecFromCArray(name);
  Register *reg = getRegister(&v);
  return reg;
}

Register *getTemporaryRegisterWithPrefix(char prefix) {

  Vector_char v =
      STRINGIFY_TO_CHAR_VEC(uint32_t, globalRegisterPool.temporaryRegisters);
  vecPushLeft_char(&v, prefix);
  globalRegisterPool.temporaryRegisters++;
  Register *reg = getRegister(&v);
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

Instruction createParamPopInstruction(Register *dst) {
  Instruction i;
  i.kind = PARAM_POP_INSTRUCTION;
  i.op = NO_OPERATOR_IOP;
  i.destination = dst;
  i.immediate = 0;
  i.leftOperand = NULL;
  i.rightOperand = NULL;
  return i;
}

Instruction createCallInstruction(Register *fun) {
  Instruction i;
  i.kind = CALL_INSTRUCTION;
  i.op = NO_OPERATOR_IOP;
  i.destination = NULL;
  i.immediate = 0;
  i.leftOperand = fun;
  i.rightOperand = NULL;
  return i;
}

Instruction createLabel(Register **rOut) {
  *rOut = getTemporaryRegisterWithPrefix('L');
  return createLabelInstruction(*rOut);
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
  i.kind = IF_INSTRUCTION;
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

Instruction createReturnInstruction() {
  Instruction i;
  i.kind = RETURN_INSTRUCTION;
  i.leftOperand = i.rightOperand = NULL;
  i.op = NO_OPERATOR_IOP;
  i.immediate = 0;
  return i;
}

Instruction createLabelInstruction(Register *labelReg) {
  Instruction i;
  i.kind = LABEL_INSTRUCTION;
  i.op = NO_OPERATOR_IOP;
  i.immediate = 0;
  i.leftOperand = i.rightOperand = NULL;
  i.destination = labelReg;
  return i;
}
