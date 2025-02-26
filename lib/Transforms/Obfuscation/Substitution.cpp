//===- Substitution.cpp - Substitution Obfuscation
// pass-------------------------===//
//
//                     The LLVM Compiler Infrastructure
//
// This file is distributed under the University of Illinois Open Source
// License. See LICENSE.TXT for details.
//
//===----------------------------------------------------------------------===//
//
// This file implements operators substitution's pass
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Obfuscation/Substitution.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Obfuscation/Utils.h"

#define DEBUG_TYPE "substitution"

#define NUMBER_ADD_SUBST 6
#define NUMBER_SUB_SUBST 7 // modify by chq 2024.3.10
#define NUMBER_AND_SUBST 2
#define NUMBER_OR_SUBST 2
#define NUMBER_XOR_SUBST 2

static cl::opt<int>
    ObfTimes("sub_loop",
             cl::desc("Choose how many time the -sub pass loops on a function"),
             cl::value_desc("number of times"), cl::init(1), cl::Optional);

// Stats
STATISTIC(Add, "Add substitued");
STATISTIC(Sub, "Sub substitued");
// STATISTIC(Mul,  "Mul substitued");
// STATISTIC(Div,  "Div substitued");
// STATISTIC(Rem,  "Rem substitued");
// STATISTIC(Shi,  "Shift substitued");
STATISTIC(And, "And substitued");
STATISTIC(Or, "Or substitued");
STATISTIC(Xor, "Xor substitued");

namespace {

struct Substitution : public FunctionPass {
  static char ID; // Pass identification, replacement for typeid
  void (Substitution::*funcAdd[NUMBER_ADD_SUBST])(BinaryOperator *bo);
  void (Substitution::*funcSub[NUMBER_SUB_SUBST])(BinaryOperator *bo);
  void (Substitution::*funcAnd[NUMBER_AND_SUBST])(BinaryOperator *bo);
  void (Substitution::*funcOr[NUMBER_OR_SUBST])(BinaryOperator *bo);
  void (Substitution::*funcXor[NUMBER_XOR_SUBST])(BinaryOperator *bo);
  bool flag;

  Substitution() : FunctionPass(ID) {}

  Substitution(bool flag) : FunctionPass(ID) {
    this->flag = flag;
    funcAdd[0] = &Substitution::addNeg;
    funcAdd[1] = &Substitution::addDoubleNeg;
    funcAdd[2] = &Substitution::addRand;
    funcAdd[3] = &Substitution::addRand2;
    funcAdd[4] = &Substitution::addComplex1;
    funcAdd[5] = &Substitution::addComplex2;

    funcSub[0] = &Substitution::subNeg;
    funcSub[1] = &Substitution::subRand;
    funcSub[2] = &Substitution::subRand2;
    funcSub[3] = &Substitution::subComplex; // add by chq 2024.3.10
    funcSub[4] = &Substitution::subComplex1;
    funcSub[5] = &Substitution::subComplex2;
    funcSub[6] = &Substitution::subComplex3;

    funcAnd[0] = &Substitution::andSubstitution;
    funcAnd[1] = &Substitution::andSubstitutionRand;

    funcOr[0] = &Substitution::orSubstitution;
    funcOr[1] = &Substitution::orSubstitutionRand;

    funcXor[0] = &Substitution::xorSubstitution;
    funcXor[1] = &Substitution::xorSubstitutionRand;
  }

  bool runOnFunction(Function &F);
  bool substitute(Function *f);

  void addNeg(BinaryOperator *bo);
  void addDoubleNeg(BinaryOperator *bo);
  void addRand(BinaryOperator *bo);
  void addRand2(BinaryOperator *bo);
  void addComplex1(BinaryOperator *bo);
  void addComplex2(BinaryOperator *bo);

  void subNeg(BinaryOperator *bo);
  void subRand(BinaryOperator *bo);
  void subRand2(BinaryOperator *bo);
  void subComplex(BinaryOperator *bo); // add by chq 2024.3.10
  void subComplex1(BinaryOperator *bo);
  void subComplex2(BinaryOperator *bo);
  void subComplex3(BinaryOperator *bo);

  void andSubstitution(BinaryOperator *bo);
  void andSubstitutionRand(BinaryOperator *bo);

  void orSubstitution(BinaryOperator *bo);
  void orSubstitutionRand(BinaryOperator *bo);

  void xorSubstitution(BinaryOperator *bo);
  void xorSubstitutionRand(BinaryOperator *bo);
};
} // namespace

char Substitution::ID = 0;
static RegisterPass<Substitution> X("substitution", "operators substitution");
Pass *llvm::createSubstitution(bool flag) { return new Substitution(flag); }

bool Substitution::runOnFunction(Function &F) {
  // Check if the percentage is correct
  if (ObfTimes <= 0) {
    errs() << "Substitution application number -sub_loop=x must be x > 0";
    return false;
  }

  Function *tmp = &F;
  // Do we obfuscate
  if (toObfuscate(flag, tmp, "sub")) {
    substitute(tmp);
    return true;
  }

  return false;
}

bool Substitution::substitute(Function *f) {
  Function *tmp = f;

  // Loop for the number of time we run the pass on the function
  int times = ObfTimes;
  do {
    for (Function::iterator bb = tmp->begin(); bb != tmp->end(); ++bb) {
      for (BasicBlock::iterator inst = bb->begin(); inst != bb->end(); ++inst) {
        if (inst->isBinaryOp()) {
          switch (inst->getOpcode()) {
          case BinaryOperator::Add:
            // case BinaryOperator::FAdd:
            // Substitute with random add operation
            (this->*funcAdd[llvm::cryptoutils->get_range(NUMBER_ADD_SUBST)])(
                cast<BinaryOperator>(inst));
            ++Add;
            break;
          case BinaryOperator::Sub:
            // case BinaryOperator::FSub:
            // Substitute with random sub operation
            (this->*funcSub[llvm::cryptoutils->get_range(NUMBER_SUB_SUBST)])(
                cast<BinaryOperator>(inst));
            ++Sub;
            break;
          case BinaryOperator::Mul:
          case BinaryOperator::FMul:
            //++Mul;
            break;
          case BinaryOperator::UDiv:
          case BinaryOperator::SDiv:
          case BinaryOperator::FDiv:
            //++Div;
            break;
          case BinaryOperator::URem:
          case BinaryOperator::SRem:
          case BinaryOperator::FRem:
            //++Rem;
            break;
          case Instruction::Shl:
            //++Shi;
            break;
          case Instruction::LShr:
            //++Shi;
            break;
          case Instruction::AShr:
            //++Shi;
            break;
          case Instruction::And:
            (this->*funcAnd[llvm::cryptoutils->get_range(2)])(
                cast<BinaryOperator>(inst));
            ++And;
            break;
          case Instruction::Or:
            (this->*funcOr[llvm::cryptoutils->get_range(2)])(
                cast<BinaryOperator>(inst));
            ++Or;
            break;
          case Instruction::Xor:
            (this->*funcXor[llvm::cryptoutils->get_range(2)])(
                cast<BinaryOperator>(inst));
            ++Xor;
            break;
          default:
            break;
          } // End switch
        } // End isBinaryOp
      } // End for basickblock
    } // End for Function
  } while (--times > 0); // for times
  return false;
}

// Implementation of a = b - (-c)
void Substitution::addNeg(BinaryOperator *bo) {
  BinaryOperator *op = NULL;

  // Create sub
  if (bo->getOpcode() == Instruction::Add) {
    op = BinaryOperator::CreateNeg(bo->getOperand(1), "", bo);
    op =
        BinaryOperator::Create(Instruction::Sub, bo->getOperand(0), op, "", bo);

    // Check signed wrap
    // op->setHasNoSignedWrap(bo->hasNoSignedWrap());
    // op->setHasNoUnsignedWrap(bo->hasNoUnsignedWrap());

    bo->replaceAllUsesWith(op);
  } /* else {
     op = BinaryOperator::CreateFNeg(bo->getOperand(1), "", bo);
     op = BinaryOperator::Create(Instruction::FSub, bo->getOperand(0), op, "",
                                 bo);
   }*/
}

// Implementation of a = -(-b + (-c))
void Substitution::addDoubleNeg(BinaryOperator *bo) {
  BinaryOperator *op, *op2 = NULL;

  if (bo->getOpcode() == Instruction::Add) {
    op = BinaryOperator::CreateNeg(bo->getOperand(0), "", bo);
    op2 = BinaryOperator::CreateNeg(bo->getOperand(1), "", bo);
    op = BinaryOperator::Create(Instruction::Add, op, op2, "", bo);
    op = BinaryOperator::CreateNeg(op, "", bo);

    // Check signed wrap
    // op->setHasNoSignedWrap(bo->hasNoSignedWrap());
    // op->setHasNoUnsignedWrap(bo->hasNoUnsignedWrap());
  } else {
    op = BinaryOperator::CreateFNeg(bo->getOperand(0), "", bo);
    op2 = BinaryOperator::CreateFNeg(bo->getOperand(1), "", bo);
    op = BinaryOperator::Create(Instruction::FAdd, op, op2, "", bo);
    op = BinaryOperator::CreateFNeg(op, "", bo);
  }

  bo->replaceAllUsesWith(op);
}

// Implementation of  r = rand (); a = b + r; a = a + c; a = a - r
void Substitution::addRand(BinaryOperator *bo) {
  BinaryOperator *op = NULL;

  if (bo->getOpcode() == Instruction::Add) {
    Type *ty = bo->getType();
    ConstantInt *co =
        (ConstantInt *)ConstantInt::get(ty, llvm::cryptoutils->get_uint64_t());
    op =
        BinaryOperator::Create(Instruction::Add, bo->getOperand(0), co, "", bo);
    op =
        BinaryOperator::Create(Instruction::Add, op, bo->getOperand(1), "", bo);
    op = BinaryOperator::Create(Instruction::Sub, op, co, "", bo);

    // Check signed wrap
    // op->setHasNoSignedWrap(bo->hasNoSignedWrap());
    // op->setHasNoUnsignedWrap(bo->hasNoUnsignedWrap());

    bo->replaceAllUsesWith(op);
  }
  /* else {
      Type *ty = bo->getType();
      ConstantFP *co =
  (ConstantFP*)ConstantFP::get(ty,(float)llvm::cryptoutils->get_uint64_t());
      op = BinaryOperator::Create(Instruction::FAdd,bo->getOperand(0),co,"",bo);
      op = BinaryOperator::Create(Instruction::FAdd,op,bo->getOperand(1),"",bo);
      op = BinaryOperator::Create(Instruction::FSub,op,co,"",bo);
  } */
}

// Implementation of r = rand (); a = b - r; a = a + b; a = a + r
void Substitution::addRand2(BinaryOperator *bo) {
  BinaryOperator *op = NULL;

  if (bo->getOpcode() == Instruction::Add) {
    Type *ty = bo->getType();
    ConstantInt *co =
        (ConstantInt *)ConstantInt::get(ty, llvm::cryptoutils->get_uint64_t());
    op =
        BinaryOperator::Create(Instruction::Sub, bo->getOperand(0), co, "", bo);
    op =
        BinaryOperator::Create(Instruction::Add, op, bo->getOperand(1), "", bo);
    op = BinaryOperator::Create(Instruction::Add, op, co, "", bo);

    // Check signed wrap
    // op->setHasNoSignedWrap(bo->hasNoSignedWrap());
    // op->setHasNoUnsignedWrap(bo->hasNoUnsignedWrap());

    bo->replaceAllUsesWith(op);
  }
  /* else {
      Type *ty = bo->getType();
      ConstantFP *co =
  (ConstantFP*)ConstantFP::get(ty,(float)llvm::cryptoutils->get_uint64_t());
      op = BinaryOperator::Create(Instruction::FAdd,bo->getOperand(0),co,"",bo);
      op = BinaryOperator::Create(Instruction::FAdd,op,bo->getOperand(1),"",bo);
      op = BinaryOperator::Create(Instruction::FSub,op,co,"",bo);
  } */
}

//+2
// Implementation of a = b * (b + 1) - b^2 + c = b + c
void Substitution::addComplex1(BinaryOperator *bo) {
  if (bo->getOpcode() == Instruction::Add) {
    // 计算 b + 1
    BinaryOperator *op1 =
        BinaryOperator::Create(Instruction::Add, bo->getOperand(0),
                               ConstantInt::get(bo->getType(), 1), "", bo);
    // 计算 b * (b + 1)
    BinaryOperator *op2 = BinaryOperator::Create(
        Instruction::Mul, bo->getOperand(0), op1, "", bo);
    // 计算 b^2
    BinaryOperator *op3 = BinaryOperator::Create(
        Instruction::Mul, bo->getOperand(0), bo->getOperand(0), "", bo);
    // 计算 b * (b + 1) - b^2
    BinaryOperator *op4 =
        BinaryOperator::Create(Instruction::Sub, op2, op3, "", bo);
    // 计算 b * (b + 1) - b^2 + c
    BinaryOperator *op5 = BinaryOperator::Create(Instruction::Add, op4,
                                                 bo->getOperand(1), "", bo);
    // 替换原操作符
    bo->replaceAllUsesWith(op5);
  }
}

// Implementation of a = b + 3 * c * (c - 2) - 3 * c^2 + 7 * c
void Substitution::addComplex2(BinaryOperator *bo) {
  if (bo->getOpcode() == Instruction::Add) {
    // 计算 c - 2
    BinaryOperator *op1 =
        BinaryOperator::Create(Instruction::Sub, bo->getOperand(1),
                               ConstantInt::get(bo->getType(), 2), "", bo);
    // 计算 3 * c * (c - 2)
    BinaryOperator *op2 = BinaryOperator::Create(
        Instruction::Mul, bo->getOperand(1), op1, "", bo);
    op2 = BinaryOperator::Create(
        Instruction::Mul, ConstantInt::get(bo->getType(), 3), op2, "", bo);
    // 计算 3 * c^2
    BinaryOperator *op3 = BinaryOperator::Create(
        Instruction::Mul, bo->getOperand(1), bo->getOperand(1), "", bo);
    op3 = BinaryOperator::Create(
        Instruction::Mul, ConstantInt::get(bo->getType(), 3), op3, "", bo);
    // 计算 7 * c
    BinaryOperator *op4 =
        BinaryOperator::Create(Instruction::Mul, bo->getOperand(1),
                               ConstantInt::get(bo->getType(), 7), "", bo);
    // 计算 b + 3 * c * (c - 2) - 3 * c^2 + 7 * c
    BinaryOperator *op5 = BinaryOperator::Create(
        Instruction::Add, bo->getOperand(0), op2, "", bo);
    op5 = BinaryOperator::Create(Instruction::Sub, op5, op3, "", bo);
    op5 = BinaryOperator::Create(Instruction::Add, op5, op4, "", bo);
    // 替换原操作符
    bo->replaceAllUsesWith(op5);
  }
}

// Implementation of a = b + (-c)
void Substitution::subNeg(BinaryOperator *bo) {
  BinaryOperator *op = NULL;

  if (bo->getOpcode() == Instruction::Sub) {
    op = BinaryOperator::CreateNeg(bo->getOperand(1), "", bo);
    op =
        BinaryOperator::Create(Instruction::Add, bo->getOperand(0), op, "", bo);

    // Check signed wrap
    // op->setHasNoSignedWrap(bo->hasNoSignedWrap());
    // op->setHasNoUnsignedWrap(bo->hasNoUnsignedWrap());
  } else {
    op = BinaryOperator::CreateFNeg(bo->getOperand(1), "", bo);
    op = BinaryOperator::Create(Instruction::FAdd, bo->getOperand(0), op, "",
                                bo);
  }

  bo->replaceAllUsesWith(op);
}

// Implementation of  r = rand (); a = b + r; a = a - c; a = a - r
void Substitution::subRand(BinaryOperator *bo) {
  BinaryOperator *op = NULL;

  if (bo->getOpcode() == Instruction::Sub) {
    Type *ty = bo->getType();
    ConstantInt *co =
        (ConstantInt *)ConstantInt::get(ty, llvm::cryptoutils->get_uint64_t());
    op =
        BinaryOperator::Create(Instruction::Add, bo->getOperand(0), co, "", bo);
    op =
        BinaryOperator::Create(Instruction::Sub, op, bo->getOperand(1), "", bo);
    op = BinaryOperator::Create(Instruction::Sub, op, co, "", bo);

    // Check signed wrap
    // op->setHasNoSignedWrap(bo->hasNoSignedWrap());
    // op->setHasNoUnsignedWrap(bo->hasNoUnsignedWrap());

    bo->replaceAllUsesWith(op);
  }
  /* else {
      Type *ty = bo->getType();
      ConstantFP *co =
  (ConstantFP*)ConstantFP::get(ty,(float)llvm::cryptoutils->get_uint64_t());
      op = BinaryOperator::Create(Instruction::FAdd,bo->getOperand(0),co,"",bo);
      op = BinaryOperator::Create(Instruction::FSub,op,bo->getOperand(1),"",bo);
      op = BinaryOperator::Create(Instruction::FSub,op,co,"",bo);
  } */
}

// Implementation of  r = rand (); a = b - r; a = a - c; a = a + r
void Substitution::subRand2(BinaryOperator *bo) {
  BinaryOperator *op = NULL;

  if (bo->getOpcode() == Instruction::Sub) {
    Type *ty = bo->getType();
    ConstantInt *co =
        (ConstantInt *)ConstantInt::get(ty, llvm::cryptoutils->get_uint64_t());
    op =
        BinaryOperator::Create(Instruction::Sub, bo->getOperand(0), co, "", bo);
    op =
        BinaryOperator::Create(Instruction::Sub, op, bo->getOperand(1), "", bo);
    op = BinaryOperator::Create(Instruction::Add, op, co, "", bo);

    // Check signed wrap
    // op->setHasNoSignedWrap(bo->hasNoSignedWrap());
    // op->setHasNoUnsignedWrap(bo->hasNoUnsignedWrap());

    bo->replaceAllUsesWith(op);
  }
  /* else {
      Type *ty = bo->getType();
      ConstantFP *co =
  (ConstantFP*)ConstantFP::get(ty,(float)llvm::cryptoutils->get_uint64_t());
      op = BinaryOperator::Create(Instruction::FSub,bo->getOperand(0),co,"",bo);
      op = BinaryOperator::Create(Instruction::FSub,op,bo->getOperand(1),"",bo);
      op = BinaryOperator::Create(Instruction::FAdd,op,co,"",bo);
  } */
}

// add by chq 2024.3.10
//  Implementation of r1 = rand(); r2 = rand(); a = ((b + r1) - (c + r2)) + (r1
//  - r2)
void Substitution::subComplex(BinaryOperator *bo) {
  BinaryOperator *op = NULL;

  if (bo->getOpcode() == Instruction::Add) {
    Type *ty = bo->getType();
    ConstantInt *co1 =
        (ConstantInt *)ConstantInt::get(ty, llvm::cryptoutils->get_uint64_t());
    ConstantInt *co2 =
        (ConstantInt *)ConstantInt::get(ty, llvm::cryptoutils->get_uint64_t());
    op = BinaryOperator::Create(Instruction::Add, bo->getOperand(0), co1, "",
                                bo); // a+r1
    BinaryOperator *op2 = BinaryOperator::Create(
        Instruction::Add, bo->getOperand(1), co2, "", bo); // b+r2
    op = BinaryOperator::Create(Instruction::Sub, op, op2, "",
                                bo); // a+r1) - (b+r2
    BinaryOperator *op3 =
        BinaryOperator::Create(Instruction::Sub, co2, co1, "", bo); // r2-r1
    op = BinaryOperator::Create(Instruction::Add, op, op3, "",
                                bo); // ((a+r1) - (b+r2)) + (r2-r1)

    // Check signed wrap
    // op->setHasNoSignedWrap(bo->hasNoSignedWrap());
    // op->setHasNoUnsignedWrap(bo->hasNoUnsignedWrap());

    bo->replaceAllUsesWith(op);
  }
}
// add by chq 2024.3.10

// Implementation of a = b - 3 * c * (c - 2) + 3 * c^2 - 7 * c
void Substitution::subComplex1(BinaryOperator *bo) {
  if (bo->getOpcode() == Instruction::Sub) {
    // 计算 c - 2
    BinaryOperator *op1 =
        BinaryOperator::Create(Instruction::Sub, bo->getOperand(1),
                               ConstantInt::get(bo->getType(), 2), "", bo);
    // 计算 3 * c * (c - 2)
    BinaryOperator *op2 = BinaryOperator::Create(
        Instruction::Mul, bo->getOperand(1), op1, "", bo);
    op2 = BinaryOperator::Create(
        Instruction::Mul, ConstantInt::get(bo->getType(), 3), op2, "", bo);
    // 计算 3 * c^2
    BinaryOperator *op3 = BinaryOperator::Create(
        Instruction::Mul, bo->getOperand(1), bo->getOperand(1), "", bo);
    op3 = BinaryOperator::Create(
        Instruction::Mul, ConstantInt::get(bo->getType(), 3), op3, "", bo);
    // 计算 7 * c
    BinaryOperator *op4 =
        BinaryOperator::Create(Instruction::Mul, bo->getOperand(1),
                               ConstantInt::get(bo->getType(), 7), "", bo);
    // 计算 b - 3 * c * (c - 2) + 3 * c^2 - 7 * c
    BinaryOperator *op5 = BinaryOperator::Create(
        Instruction::Sub, bo->getOperand(0), op2, "", bo);
    op5 = BinaryOperator::Create(Instruction::Add, op5, op3, "", bo);
    op5 = BinaryOperator::Create(Instruction::Sub, op5, op4, "", bo);
    // 替换原操作符
    bo->replaceAllUsesWith(op5);
  }
}

// Implementation of a = (b - 1) * (b + 3) - (b + 4) * (b - 3) – 9 – c=b-c
void Substitution::subComplex2(BinaryOperator *bo) {
  if (bo->getOpcode() == Instruction::Sub) {
    // 计算 b - 1
    BinaryOperator *op1 =
        BinaryOperator::Create(Instruction::Sub, bo->getOperand(0),
                               ConstantInt::get(bo->getType(), 1), "", bo);
    // 计算 b + 3
    BinaryOperator *op2 =
        BinaryOperator::Create(Instruction::Add, bo->getOperand(0),
                               ConstantInt::get(bo->getType(), 3), "", bo);
    // 计算 (b - 1) * (b + 3)
    BinaryOperator *op3 =
        BinaryOperator::Create(Instruction::Mul, op1, op2, "", bo);
    // 计算 b + 4
    BinaryOperator *op4 =
        BinaryOperator::Create(Instruction::Add, bo->getOperand(0),
                               ConstantInt::get(bo->getType(), 4), "", bo);
    // 计算 b - 3
    BinaryOperator *op5 =
        BinaryOperator::Create(Instruction::Sub, bo->getOperand(0),
                               ConstantInt::get(bo->getType(), 3), "", bo);
    // 计算 (b + 4) * (b - 3)
    BinaryOperator *op6 =
        BinaryOperator::Create(Instruction::Mul, op4, op5, "", bo);
    // 计算 (b - 1) * (b + 3) - (b + 4) * (b - 3)
    BinaryOperator *op7 =
        BinaryOperator::Create(Instruction::Sub, op3, op6, "", bo);
    // 计算 (b - 1) * (b + 3) - (b + 4) * (b - 3) – 9
    BinaryOperator *op8 = BinaryOperator::Create(
        Instruction::Sub, op7, ConstantInt::get(bo->getType(), 9), "", bo);
    // 计算 (b - 1) * (b + 3) - (b + 4) * (b - 3) – 9 – c
    BinaryOperator *op9 = BinaryOperator::Create(Instruction::Sub, op8,
                                                 bo->getOperand(1), "", bo);
    // 替换原操作符
    bo->replaceAllUsesWith(op9);
  }
}

// Implementation of a = b * (b + 1) - b^2 - (c - 1) * (c + 3) + (c + 4) * (c -
// 3) + 9
void Substitution::subComplex3(BinaryOperator *bo) {
  if (bo->getOpcode() == Instruction::Sub) {
    // 计算 b * (b + 1)
    BinaryOperator *op1 = BinaryOperator::Create(
        Instruction::Mul, bo->getOperand(0),
        BinaryOperator::Create(Instruction::Add, bo->getOperand(0),
                               ConstantInt::get(bo->getType(), 1), "", bo),
        "", bo);
    // 计算 b^2
    BinaryOperator *op2 = BinaryOperator::Create(
        Instruction::Mul, bo->getOperand(0), bo->getOperand(0), "", bo);
    // 计算 c - 1
    BinaryOperator *op3 =
        BinaryOperator::Create(Instruction::Sub, bo->getOperand(1),
                               ConstantInt::get(bo->getType(), 1), "", bo);
    // 计算 c + 3
    BinaryOperator *op4 =
        BinaryOperator::Create(Instruction::Add, bo->getOperand(1),
                               ConstantInt::get(bo->getType(), 3), "", bo);
    // 计算 (c - 1) * (c + 3)
    BinaryOperator *op5 =
        BinaryOperator::Create(Instruction::Mul, op3, op4, "", bo);
    // 计算 c + 4
    BinaryOperator *op6 =
        BinaryOperator::Create(Instruction::Add, bo->getOperand(1),
                               ConstantInt::get(bo->getType(), 4), "", bo);
    // 计算 c - 3
    BinaryOperator *op7 =
        BinaryOperator::Create(Instruction::Sub, bo->getOperand(1),
                               ConstantInt::get(bo->getType(), 3), "", bo);
    // 计算 (c + 4) * (c - 3)
    BinaryOperator *op8 =
        BinaryOperator::Create(Instruction::Mul, op6, op7, "", bo);
    // 计算 b * (b + 1) - b^2
    BinaryOperator *op9 =
        BinaryOperator::Create(Instruction::Sub, op1, op2, "", bo);
    // 计算 b * (b + 1) - b^2 - (c - 1) * (c + 3)
    BinaryOperator *op10 =
        BinaryOperator::Create(Instruction::Sub, op9, op5, "", bo);
    // 计算 b * (b + 1) - b^2 - (c - 1) * (c + 3) + (c + 4) * (c - 3)
    BinaryOperator *op11 =
        BinaryOperator::Create(Instruction::Add, op10, op8, "", bo);
    // 计算 b * (b + 1) - b^2 - (c - 1) * (c + 3) + (c + 4) * (c - 3) + 9
    BinaryOperator *op12 = BinaryOperator::Create(
        Instruction::Add, op11, ConstantInt::get(bo->getType(), 9), "", bo);

    // 替换原操作符
    bo->replaceAllUsesWith(op12);
  }
}

// Implementation of a = b & c => a = (b^~c)& b
void Substitution::andSubstitution(BinaryOperator *bo) {
  BinaryOperator *op = NULL;

  // Create NOT on second operand => ~c
  op = BinaryOperator::CreateNot(bo->getOperand(1), "", bo);

  // Create XOR => (b^~c)
  BinaryOperator *op1 =
      BinaryOperator::Create(Instruction::Xor, bo->getOperand(0), op, "", bo);

  // Create AND => (b^~c) & b
  op = BinaryOperator::Create(Instruction::And, op1, bo->getOperand(0), "", bo);
  bo->replaceAllUsesWith(op);
}

// Implementation of a = a && b <=> !(!a | !b) && (r | !r)
void Substitution::andSubstitutionRand(BinaryOperator *bo) {
  // Copy of the BinaryOperator type to create the random number with the
  // same type of the operands
  Type *ty = bo->getType();

  // r (Random number)
  ConstantInt *co =
      (ConstantInt *)ConstantInt::get(ty, llvm::cryptoutils->get_uint64_t());

  // !a
  BinaryOperator *op = BinaryOperator::CreateNot(bo->getOperand(0), "", bo);

  // !b
  BinaryOperator *op1 = BinaryOperator::CreateNot(bo->getOperand(1), "", bo);

  // !r
  BinaryOperator *opr = BinaryOperator::CreateNot(co, "", bo);

  // (!a | !b)
  BinaryOperator *opa =
      BinaryOperator::Create(Instruction::Or, op, op1, "", bo);

  // (r | !r)
  opr = BinaryOperator::Create(Instruction::Or, co, opr, "", bo);

  // !(!a | !b)
  op = BinaryOperator::CreateNot(opa, "", bo);

  // !(!a | !b) && (r | !r)
  op = BinaryOperator::Create(Instruction::And, op, opr, "", bo);

  // We replace all the old AND operators with the new one transformed
  bo->replaceAllUsesWith(op);
}

// Implementation of a = b | c => a = (b & c) | (b ^ c)
void Substitution::orSubstitutionRand(BinaryOperator *bo) {

  Type *ty = bo->getType();
  ConstantInt *co =
      (ConstantInt *)ConstantInt::get(ty, llvm::cryptoutils->get_uint64_t());

  // !a
  BinaryOperator *op = BinaryOperator::CreateNot(bo->getOperand(0), "", bo);

  // !b
  BinaryOperator *op1 = BinaryOperator::CreateNot(bo->getOperand(1), "", bo);

  // !r
  BinaryOperator *op2 = BinaryOperator::CreateNot(co, "", bo);

  // !a && r
  BinaryOperator *op3 =
      BinaryOperator::Create(Instruction::And, op, co, "", bo);

  // a && !r
  BinaryOperator *op4 =
      BinaryOperator::Create(Instruction::And, bo->getOperand(0), op2, "", bo);

  // !b && r
  BinaryOperator *op5 =
      BinaryOperator::Create(Instruction::And, op1, co, "", bo);

  // b && !r
  BinaryOperator *op6 =
      BinaryOperator::Create(Instruction::And, bo->getOperand(1), op2, "", bo);

  // (!a && r) || (a && !r)
  op3 = BinaryOperator::Create(Instruction::Or, op3, op4, "", bo);

  // (!b && r) ||(b && !r)
  op4 = BinaryOperator::Create(Instruction::Or, op5, op6, "", bo);

  // (!a && r) || (a && !r) ^ (!b && r) ||(b && !r)
  op5 = BinaryOperator::Create(Instruction::Xor, op3, op4, "", bo);

  // !a || !b
  op3 = BinaryOperator::Create(Instruction::Or, op, op1, "", bo);

  // !(!a || !b)
  op3 = BinaryOperator::CreateNot(op3, "", bo);

  // r || !r
  op4 = BinaryOperator::Create(Instruction::Or, co, op2, "", bo);

  // !(!a || !b) && (r || !r)
  op4 = BinaryOperator::Create(Instruction::And, op3, op4, "", bo);

  // [(!a && r) || (a && !r) ^ (!b && r) ||(b && !r) ] || [!(!a || !b) && (r ||
  // !r)]
  op = BinaryOperator::Create(Instruction::Or, op5, op4, "", bo);
  bo->replaceAllUsesWith(op);
}

void Substitution::orSubstitution(BinaryOperator *bo) {
  BinaryOperator *op = NULL;

  // Creating first operand (b & c)
  op = BinaryOperator::Create(Instruction::And, bo->getOperand(0),
                              bo->getOperand(1), "", bo);

  // Creating second operand (b ^ c)
  BinaryOperator *op1 = BinaryOperator::Create(
      Instruction::Xor, bo->getOperand(0), bo->getOperand(1), "", bo);

  // final op
  op = BinaryOperator::Create(Instruction::Or, op, op1, "", bo);
  bo->replaceAllUsesWith(op);
}

// Implementation of a = a ~ b => a = (!a && b) || (a && !b)
void Substitution::xorSubstitution(BinaryOperator *bo) {
  BinaryOperator *op = NULL;

  // Create NOT on first operand
  op = BinaryOperator::CreateNot(bo->getOperand(0), "", bo); // !a

  // Create AND
  op = BinaryOperator::Create(Instruction::And, bo->getOperand(1), op, "",
                              bo); // !a && b

  // Create NOT on second operand
  BinaryOperator *op1 =
      BinaryOperator::CreateNot(bo->getOperand(1), "", bo); // !b

  // Create AND
  op1 = BinaryOperator::Create(Instruction::And, bo->getOperand(0), op1, "",
                               bo); // a && !b

  // Create OR
  op = BinaryOperator::Create(Instruction::Or, op, op1, "",
                              bo); // (!a && b) || (a && !b)
  bo->replaceAllUsesWith(op);
}

// implementation of a = a ^ b <=> (a ^ r) ^ (b ^ r) <=> (!a && r || a && !r) ^
// (!b && r || b && !r)
// note : r is a random number
void Substitution::xorSubstitutionRand(BinaryOperator *bo) {
  BinaryOperator *op = NULL;

  Type *ty = bo->getType();
  ConstantInt *co =
      (ConstantInt *)ConstantInt::get(ty, llvm::cryptoutils->get_uint64_t());

  // !a
  op = BinaryOperator::CreateNot(bo->getOperand(0), "", bo);

  // !a && r
  op = BinaryOperator::Create(Instruction::And, co, op, "", bo);

  // !r
  BinaryOperator *opr = BinaryOperator::CreateNot(co, "", bo);

  // a && !r
  BinaryOperator *op1 =
      BinaryOperator::Create(Instruction::And, bo->getOperand(0), opr, "", bo);

  // !b
  BinaryOperator *op2 = BinaryOperator::CreateNot(bo->getOperand(1), "", bo);

  // !b && r
  op2 = BinaryOperator::Create(Instruction::And, op2, co, "", bo);

  // b && !r
  BinaryOperator *op3 =
      BinaryOperator::Create(Instruction::And, bo->getOperand(1), opr, "", bo);

  // (!a && r) || (a && !r)
  op = BinaryOperator::Create(Instruction::Or, op, op1, "", bo);

  // (!b && r) || (b && !r)
  op1 = BinaryOperator::Create(Instruction::Or, op2, op3, "", bo);

  // (!a && r) || (a && !r) ^ (!b && r) || (b && !r)
  op = BinaryOperator::Create(Instruction::Xor, op, op1, "", bo);
  bo->replaceAllUsesWith(op);
}
