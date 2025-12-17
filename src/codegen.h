#pragma once
#include "char_vector.h"
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
  RETURN_INSTRUCTION
} InstructionKind;

typedef enum InstructionOperator {
  ADD_IOP = '+',
  SUB_IOP = '-',
  MUL_IOP = '*',
  DIV_IOP = '/',
  REM_IOP = '%'
} InstructionOperator;

typedef struct Register {
  Vector_char name;
} Register;

HEADER_VECTOR_TYPE(Register, Vector_char *)

typedef struct RegisterPool {
  Vector_Register registers;
} RegisterPool;

typedef struct Instruction {
  InstructionKind kind;
  Register *destination;
  Register *leftOperand;
  InstructionOperator op;
  Register *rightOperand;
} Instruction;

HEADER_VECTOR_TYPE(Instruction, InstructionKind)

typedef struct Codegen {
  Vector_Instruction instructions;
  Register result;
  uint32_t temporaryRegisters;
} Codegen;

extern RegisterPool globalRegisterPool;

void codegenInit();
