#pragma once
#include "char_vector.h"
#include "semantic.h"
#include "vector.h"

typedef enum InstructionKind {
  ARITH_INSTRUCTION,
  LABEL_INSTRUCTION,
  JUMP_INSTRUCTION,
  IF_INSTRUCTION,
  IF_NOT_INSTRUCTION,
  READ_INSTRUCTION,
  WRITE_INSTRUCTION,
  ENTRY_INSTRUCTION,
  RETURN_INSTRUCTION,
  ARRAY_STORE_INSTRUCTION,
  ARRAY_LOAD_INSTRUCTION,
  IMMEDIATE_LOAD_INSTRUCTION,
  PARAM_PUSH_INSTRUCTION,
  PARAM_POP_INSTRUCTION,
  CALL_INSTRUCTION
} InstructionKind;

typedef enum InstructionOperator {
  ADD_IOP,
  SUB_IOP,
  MUL_IOP,
  DIV_IOP,
  REM_IOP,
  GT_IOP,
  GTE_IOP,
  LT_IOP,
  LTE_IOP,
  EQ_IOP,
  NE_IOP,
  NO_OPERATOR_IOP
} InstructionOperator;

InstructionOperator iOperatorFromString(Vector_char *);

typedef struct Register {
  Vector_char name;
} Register;

HEADER_VECTOR_TYPE(Register, Vector_char *)

typedef struct RegisterPool {
  Vector_Register registers;
  uint32_t temporaryRegisters;
} RegisterPool;

typedef struct Instruction {
  InstructionKind kind;
  Register *destination;
  Register *leftOperand;
  InstructionOperator op;
  Register *rightOperand;
  int64_t immediate;
} Instruction;

HEADER_VECTOR_TYPE(Instruction, InstructionKind)

typedef struct Codegen {
  Vector_Instruction instructions;
  Register *result;
  DataTypeKind resultType;
} Codegen;

HEADER_VECTOR_TYPE(Codegen, Register *);

extern RegisterPool globalRegisterPool;

void codegenInit();

Register *getRegisterCStr(const char *);

Register *getRegister(Vector_char *);

Register *getTemporaryRegister();

Register *getRegisterNamedAfter(Symbol *);

Codegen createCodegenObj();

Instruction createArithmeticInstruction(Register *dst, Register *lhs,
                                        InstructionOperator op, Register *rhs);

Instruction createMoveInstruction(Register *dst, Register *src);

Instruction createArrayLoadInstruction(Register *dst, Register *array,
                                       Register *index);

Instruction createArrayStoreInstruction(Register *array, Register *index,
                                        Register *src);

Instruction createImmediateLoadInstruction(Register *dst, int64_t imm);

Instruction createParamPushInstruction(Register *src);

Instruction createCallInstruction(Register *dest, Register *fun);

Instruction createLabel(Register **rOut);

Instruction createIfNotInstruction(Register *cond, Register *label);

Instruction createIfInstruction(Register *cond, Register *label);

Instruction createJumpInstruction(Register *label);
