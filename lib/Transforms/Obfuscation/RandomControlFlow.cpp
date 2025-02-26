//===- RandomControlFlow.cpp - RandomControlFlow Obfuscation
//pass-------------------------===//
/**
 * @file RandomControlFlow.cpp
 * @brief Implementation of the RandomControlFlow obfuscation pass.
 *
 * This pass applies random control flow obfuscation to a function. It selects
 * basic blocks based on a given probability rate and adds random flow to those
 * selected blocks. The pass can be configured with options for the probability
 * rate and the number of times it loops on a function.
 *
 * Usage:
 *   -randcf_prob=<probability rate> : Choose the probability rate [%] each
 * basic block will be obfuscated -randcf_loop=<number of times> : Choose how
 * many times the pass loops on a function
 *
 */

#include "llvm/Transforms/Obfuscation/RandomControlFlow.h"
#include "llvm/Transforms/Obfuscation/Utils.h"

// Define CONST_I32 constant
#define CONST_I32(value)                                                       \
  ConstantInt::get(Type::getInt32Ty(insertAfter->getContext()), value)

// Stats
#define DEBUG_TYPE "RandomControlFlow"
STATISTIC(NumFunction, "a. Number of functions in this module");
STATISTIC(NumTimesOnFunctions, "b. Number of times we run on each function");
STATISTIC(InitNumBasicBlocks,
          "c. Initial number of basic blocks in this module");
STATISTIC(NumModifiedBasicBlocks, "d. Number of modified basic blocks");
STATISTIC(NumAddedBasicBlocks,
          "e. Number of added basic blocks in this module");
STATISTIC(FinalNumBasicBlocks,
          "f. Final number of basic blocks in this module");

// Options for the pass
const int defaultObfRate = 30, defaultObfTime = 1;

static cl::opt<int>
    ObfProbRate("randcf_prob",
                cl::desc("Choose the probability [%] each basic blocks will be "
                         "obfuscated by the -randcf pass"),
                cl::value_desc("probability rate"), cl::init(defaultObfRate),
                cl::Optional);

static cl::opt<int> ObfTimes(
    "randcf_loop",
    cl::desc("Choose how many time the -randcf pass loop on a function"),
    cl::value_desc("number of times"), cl::init(defaultObfTime), cl::Optional);

namespace {
struct RandomControlFlow : public FunctionPass {
  static char ID; // Pass identification
  bool flag;
  RandomControlFlow() : FunctionPass(ID) {}
  RandomControlFlow(bool flag) : FunctionPass(ID) {
    this->flag = flag;
    RandomControlFlow();
  }

  /* runOnFunction
   *
   * Overwrite FunctionPass method to apply the transformation
   */
  virtual bool runOnFunction(Function &F) {
    // Check if the percentage is correct
    if (ObfTimes <= 0) {
      errs() << "RandomControlFlow application number -randcf_loop=x must be x "
                "> 0";
      return false;
    }

    // Check if the number of applications is correct
    if (!((ObfProbRate > 0) && (ObfProbRate <= 100))) {
      errs() << "RandomControlFlow application basic blocks percentage "
                "-randcf_prob=x must be 0 < x <= 100";
      return false;
    }
    // If randcf annotations
    if (toObfuscate(flag, &F, "randcf")) {
      random(F);
      doF(*F.getParent());
      return true;
    }

    return false;
  } // end of runOnFunction()

  void random(Function &F) {
    // For statistics and debug
    ++NumFunction;
    int NumBasicBlocks = 0;
    bool firstTime = true; // First time we do the loop in this function
    bool hasBeenModified = false;
    DEBUG_WITH_TYPE("opt", errs() << "randcf: Started on function "
                                  << F.getName() << "\n");
    DEBUG_WITH_TYPE("opt", errs() << "randcf: Probability rate: " << ObfProbRate
                                  << "\n");
    if (ObfProbRate < 0 || ObfProbRate > 100) {
      DEBUG_WITH_TYPE("opt", errs()
                                 << "randcf: Incorrect value,"
                                 << " probability rate set to default value: "
                                 << defaultObfRate << " \n");
      ObfProbRate = defaultObfRate;
    }
    DEBUG_WITH_TYPE("opt",
                    errs() << "randcf: How many times: " << ObfTimes << "\n");
    if (ObfTimes <= 0) {
      DEBUG_WITH_TYPE("opt", errs()
                                 << "randcf: Incorrect value,"
                                 << " must be greater than 1. Set to default: "
                                 << defaultObfTime << " \n");
      ObfTimes = defaultObfTime;
    }
    NumTimesOnFunctions = ObfTimes;
    int NumObfTimes = ObfTimes;

    // Real begining of the pass
    // Loop for the number of time we run the pass on the function
    do {
      DEBUG_WITH_TYPE("cfg", errs() << "randcf: Function " << F.getName()
                                    << ", before the pass:\n");
      DEBUG_WITH_TYPE("cfg", F.viewCFG());
      // Put all the function's block in a list
      std::list<BasicBlock *> basicBlocks;
      for (Function::iterator i = F.begin(); i != F.end(); ++i) {
        basicBlocks.push_back(&*i);
      }
      DEBUG_WITH_TYPE(
          "gen",
          errs() << "randcf: Iterating on the Function's Basic Blocks\n");

      while (!basicBlocks.empty()) {
        NumBasicBlocks++;
        // Basic Blocks' selection
        if ((int)llvm::cryptoutils->get_range(100) <= ObfProbRate) {
          DEBUG_WITH_TYPE("opt", errs() << "randcf: Block " << NumBasicBlocks
                                        << " selected. \n");
          hasBeenModified = true;
          ++NumModifiedBasicBlocks;
          NumAddedBasicBlocks += 3;
          FinalNumBasicBlocks += 3;
          // Add random flow to the given Basic Block
          BasicBlock *basicBlock = basicBlocks.front();
          addRandomFlow(basicBlock, F);
        } else {
          DEBUG_WITH_TYPE("opt", errs() << "randcf: Block " << NumBasicBlocks
                                        << " not selected.\n");
        }
        // remove the block from the list
        basicBlocks.pop_front();

        if (firstTime) { // first time we iterate on this function
          ++InitNumBasicBlocks;
          ++FinalNumBasicBlocks;
        }
      } // end of while(!basicBlocks.empty())
      DEBUG_WITH_TYPE("gen", errs() << "randcf: End of function " << F.getName()
                                    << "\n");
      if (hasBeenModified) { // if the function has been modified
        DEBUG_WITH_TYPE("cfg", errs() << "randcf: Function " << F.getName()
                                      << ", after the pass: \n");
        DEBUG_WITH_TYPE("cfg", F.viewCFG());
      } else {
        DEBUG_WITH_TYPE("cfg",
                        errs() << "randcf: Function's not been modified \n");
      }
      firstTime = false;
    } while (--NumObfTimes > 0);
  }

  /* addRandomFlow
   *
   * Add random flow to a given basic block, according to the header's
   * description
   */
  virtual void addRandomFlow(BasicBlock *basicBlock, Function &F) {

    // Split the basic block in 3 parts
    BasicBlock::iterator i1 = basicBlock->begin();
    if (basicBlock->getFirstNonPHIOrDbgOrLifetime())
      i1 = (BasicBlock::iterator)basicBlock->getFirstNonPHIOrDbgOrLifetime();
    Twine *var;
    var = new Twine("originalBB");
    BasicBlock *originalBB = basicBlock->splitBasicBlock(i1, *var);
    DEBUG_WITH_TYPE("gen",
                    errs() << "randcf: First and original basic blocks: ok\n");

    // iterate on instruction just before the terminator of the originalBB
    BasicBlock::iterator i = originalBB->end();

    // Split at this point (we only want the terminator in the second part)
    Twine *var5 = new Twine("originalBBpart2");
    BasicBlock *originalBBpart2 = originalBB->splitBasicBlock(--i, *var5);
    DEBUG_WITH_TYPE(
        "gen", errs() << "randcf: Terminator part of the original basic block"
                      << " is isolated\n");

    // Creating the altered basic block on which the first basicBlock will jump
    Twine *var3 = new Twine("alteredBB");
    BasicBlock *alteredBB = createCloneBasicBlock(originalBB);
    DEBUG_WITH_TYPE("gen", errs() << "randcf: Altered basic block: ok\n");

    // Now that all the blocks are created,
    // we modify the terminators to adjust the control flow.

    // alteredBB->getTerminator()->eraseFromParent();
    basicBlock->getTerminator()->eraseFromParent();
    DEBUG_WITH_TYPE(
        "gen", errs() << "randcf: Terminator removed from" // the altered and"
                      << " first basic blocks\n");

    // Preparing a condition..
    // For now, the condition is an always true comparaison between 2 float
    // This will be complicated after the pass (in doFinalization())
    //   Value * LHS = ConstantFP::get(Type::getFloatTy(F.getContext()), 1.0);
    //   Value * RHS = ConstantFP::get(Type::getFloatTy(F.getContext()), 1.0);
    //   DEBUG_WITH_TYPE("gen", errs() << "randcf: Value LHS and RHS
    //   created\n");

    // Function *randfunc = Intrinsic::getDeclaration(
    //     basicBlock->getModule(),
    //     llvm::Intrinsic::x86_rdrand_32); //  wait to be
    //     verified
    // CallInst *callinst = CallInst::Create(randfunc, "", basicBlock);
    // Value *randVar = ExtractValueInst::Create(callinst, 0, "", basicBlock);
    // Insert the generated random number at the end of entryBB

    Value *randVar =
        ConstantInt::get(Type::getInt32Ty(F.getContext()), 3, false); // 3
    insertRandomBranch(randVar, originalBB, alteredBB,
                       basicBlock); // if randVar % 2 == 1, jump to alteredBB,
                                    // otherwise jump to originalBB
    DEBUG_WITH_TYPE(
        "gen",
        errs() << "randcf: Terminator instruction in first basic block: ok\n");

    originalBB->getTerminator()->eraseFromParent(); // remove the terminator
    alteredBB->getTerminator()->eraseFromParent();  // remove the terminator
    insertRandomBranch(
        randVar, originalBB, originalBBpart2,
        alteredBB); // if randVar % 2 == 1, jump to originalBBpart2, otherwise
                    // jump to alteredBB
    DEBUG_WITH_TYPE(
        "gen",
        errs() << "randcf: Terminator instruction in altered block: ok\n");
    insertRandomBranch(randVar, originalBBpart2, alteredBB,
                       originalBB); // if randVar % 2 == 1, jump to alteredBB,
                                    // otherwise jump to originalBB
    DEBUG_WITH_TYPE(
        "gen",
        errs() << "randcf: Terminator instruction in original block: ok\n");

  } // end of addRandomFlow()

  // Insert random branch, where the random number is randVar
  // If randVar % 2 == 1, jump to ifTrue, otherwise jump to ifFalse
  void insertRandomBranch(Value *randVar, BasicBlock *ifTrue,
                          BasicBlock *ifFalse, BasicBlock *insertAfter) {
    Value *alteredRandVar = alterVal(randVar, insertAfter);
    Value *randMod2 = BinaryOperator::CreateURem(alteredRandVar, CONST_I32(2),
                                                 "", insertAfter);
    CmpInst *cmp1 = new ICmpInst(*insertAfter, ICmpInst::ICMP_EQ, randMod2,
                                 CONST_I32(0), "");
    // if randVar % 2 == 1, jump to ifTrue, otherwise jump to ifFalse
    BranchInst *branch = BranchInst::Create(ifTrue, ifFalse, cmp1, insertAfter);
  } // end of insertRandomBranch()

  // Perform constant transformation on variables
  Value *alterVal(Value *startVar, BasicBlock *insertAfter) {
    uint32_t code = rand() % 3;
    Value *result;
    if (code == 0) {
      // x = x * (x + 1) - x^2
      BinaryOperator *op1 = BinaryOperator::Create(
          Instruction::Add, startVar, CONST_I32(1), "", insertAfter); // x + 1
      BinaryOperator *op2 = BinaryOperator::Create(
          Instruction::Mul, startVar, op1, "", insertAfter); // x * (x + 1)
      BinaryOperator *op3 = BinaryOperator::Create(
          Instruction::Mul, startVar, startVar, "", insertAfter); // x^2
      BinaryOperator *op4 = BinaryOperator::Create(
          Instruction::Sub, op2, op3, "", insertAfter); // x * (x + 1) - x^2
      result = op4;
    } else if (code == 1) {
      // x = 3 * x * (x - 2) - 3 * x^2 + 7 * x
      BinaryOperator *op1 = BinaryOperator::Create(
          Instruction::Mul, startVar, CONST_I32(3), "", insertAfter); // 3 * x
      BinaryOperator *op2 = BinaryOperator::Create(
          Instruction::Sub, startVar, CONST_I32(2), "", insertAfter); // x - 2
      BinaryOperator *op3 = BinaryOperator::Create(
          Instruction::Mul, op1, op2, "", insertAfter); // 3 * x * (x - 2)
      BinaryOperator *op4 = BinaryOperator::Create(
          Instruction::Mul, startVar, startVar, "", insertAfter); // x^2
      BinaryOperator *op5 = BinaryOperator::Create(
          Instruction::Mul, op4, CONST_I32(3), "", insertAfter); // 3 * x^2
      BinaryOperator *op6 = BinaryOperator::Create(
          Instruction::Mul, startVar, CONST_I32(7), "", insertAfter); // 7 * x
      BinaryOperator *op7 =
          BinaryOperator::Create(Instruction::Sub, op3, op5, "",
                                 insertAfter); // 3 * x * (x - 2) - 3 * x^2
      BinaryOperator *op8 = BinaryOperator::Create(
          Instruction::Add, op6, op7, "",
          insertAfter); // 3 * x * (x - 2) - 3 * x^2 + 7 * x
      result = op8;
    } else if (code == 2) {
      // x = (x - 1) * (x + 3) - (x + 4) * (x - 3) - 9
      BinaryOperator *op1 = BinaryOperator::Create(
          Instruction::Sub, startVar, CONST_I32(1), "", insertAfter); // x - 1
      BinaryOperator *op2 = BinaryOperator::Create(
          Instruction::Add, startVar, CONST_I32(3), "", insertAfter); // x + 3
      BinaryOperator *op3 = BinaryOperator::Create(
          Instruction::Add, startVar, CONST_I32(4), "", insertAfter); // x + 4
      BinaryOperator *op4 = BinaryOperator::Create(
          Instruction::Sub, startVar, CONST_I32(3), "", insertAfter); // x - 3
      BinaryOperator *op5 = BinaryOperator::Create(
          Instruction::Mul, op1, op2, "", insertAfter); // (x - 1) * (x + 3)
      BinaryOperator *op6 = BinaryOperator::Create(
          Instruction::Mul, op3, op4, "", insertAfter); // (x + 4) * (x - 3)
      BinaryOperator *op7 = BinaryOperator::Create(
          Instruction::Sub, op5, op6, "",
          insertAfter); // (x - 1) * (x + 3) - (x + 4) * (x - 3)
      BinaryOperator *op8 = BinaryOperator::Create(
          Instruction::Sub, op7, CONST_I32(9), "",
          insertAfter); // (x - 1) * (x + 3) - (x + 4) * (x - 3) - 9
      result = op8;
    }
    return result;
  }

  BasicBlock *createCloneBasicBlock(BasicBlock *BB) {
    // Fixing escaped variables
    vector<Instruction *> origReg;
    BasicBlock &entryBB = BB->getParent()->getEntryBlock();
    for (Instruction &I : *BB) {
      // Check if it is an escaped variable
      //(need to be processed if it is not allocated and defined in the entry
      //block)
      if (!(isa<AllocaInst>(&I) && I.getParent() == &entryBB) &&
          I.isUsedOutsideOfBlock(BB)) {
        origReg.push_back(&I);
      }
    }
    for (Instruction *I : origReg) {
      DemoteRegToStack(*I, entryBB.getTerminator());
    }

    ValueToValueMapTy VMap; // Variable mapping table
    BasicBlock *cloneBB = CloneBasicBlock(BB, VMap, "cloneBB", BB->getParent());
    for (Instruction &I : *cloneBB) {
      // Traverse the operands
      for (int i = 0; i < I.getNumOperands(); i++) {
        Value *V = MapValue(I.getOperand(i), VMap);
        // Check if the mapping is successful
        if (V) {
          I.setOperand(i, V);
        }
      }
    }

    return cloneBB;
  }

  /* doFinalization
   *
   * Overwrite FunctionPass method to apply the transformations to the whole
   * module. This part obfuscate all the always true predicates of the module.
   * More precisely, the condition which predicate is FCMP_TRUE.
   * It also remove all the functions' basic blocks' and instructions' names.
   */
  bool doF(Module &M) {
    // In this part we extract all always-true predicate and replace them with
    // opaque predicate: For this, we declare two global values: x and y, and
    // replace the FCMP_TRUE predicate with
    //
    //  better way to obfuscate the predicates would be welcome. In the meantime
    // we will erase the name of the basic blocks, the instructions and the
    // functions.
    DEBUG_WITH_TYPE("gen", errs() << "randcf: Starting doFinalization...\n");

    //  The global values
    Twine *varX = new Twine("x");
    Twine *varY = new Twine("y");
    Value *x1 = ConstantInt::get(Type::getInt32Ty(M.getContext()), 0, false);
    Value *y1 = ConstantInt::get(Type::getInt32Ty(M.getContext()), 0, false);

    //  The global variables
    GlobalVariable *x =
        new GlobalVariable(M, Type::getInt32Ty(M.getContext()), false,
                           GlobalValue::CommonLinkage, (Constant *)x1, *varX);
    GlobalVariable *y =
        new GlobalVariable(M, Type::getInt32Ty(M.getContext()), false,
                           GlobalValue::CommonLinkage, (Constant *)y1, *varY);

    std::vector<Instruction *> toEdit, toDelete;
    BinaryOperator *op, *op1, *op2, *op3 = NULL;
    LoadInst *opX, *opY;
    ICmpInst *condition, *condition2;
    // Looking for the conditions and branches to transform
    for (Module::iterator mi = M.begin(), me = M.end(); mi != me; ++mi) {
      for (Function::iterator fi = mi->begin(), fe = mi->end(); fi != fe;
           ++fi) {
        // fi->setName("");
        TerminatorInst *tbb = fi->getTerminator();
        if (tbb->getOpcode() == Instruction::Br) {
          BranchInst *br = (BranchInst *)(tbb);
          if (br->isConditional()) {
            FCmpInst *cond = (FCmpInst *)br->getCondition();
            unsigned opcode = cond->getOpcode();
            if (opcode == Instruction::FCmp) {
              if (cond->getPredicate() == FCmpInst::FCMP_TRUE) {
                DEBUG_WITH_TYPE(
                    "gen", errs() << "randcf: an always true predicate !\n");
                toDelete.push_back(cond); // The condition
                toEdit.push_back(tbb);    // The branch using the condition
              }
            }
          }
        }
        /*
        for (BasicBlock::iterator bi = fi->begin(), be = fi->end() ; bi != be;
        ++bi){ bi->setName(""); // setting the basic blocks' names
        }
        */
      }
    }
    // Replacing all the branches we found
    for (std::vector<Instruction *>::iterator i = toEdit.begin();
         i != toEdit.end(); ++i) {
      //  The global values
      opX = new LoadInst((Value *)x, "", (*i));
      opY = new LoadInst((Value *)y, "", (*i));

      // switch to choose the opaque predicate
      switch (llvm::cryptoutils->get_range(6)) {
      case 0: {
        // first opaque predicate if y < 10 || x*(x+1) % 2 == 0
        op = BinaryOperator::Create(
            Instruction::Sub, (Value *)opX,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 1, false), "",
            (*i));
        op1 = BinaryOperator::Create(Instruction::Mul, (Value *)opX, op, "",
                                     (*i));
        op = BinaryOperator::Create(
            Instruction::URem, op1,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 2, false), "",
            (*i));
        condition = new ICmpInst(
            (*i), ICmpInst::ICMP_EQ, op,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 0, false));
        condition2 = new ICmpInst(
            (*i), ICmpInst::ICMP_SLT, opY,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 10, false));
        op1 = BinaryOperator::Create(Instruction::Or, (Value *)condition,
                                     (Value *)condition2, "", (*i));
        // Replace the condition
        BranchInst::Create(((BranchInst *)*i)->getSuccessor(0),
                           ((BranchInst *)*i)->getSuccessor(1), (Value *)op1,
                           ((BranchInst *)*i)->getParent());
        DEBUG_WITH_TYPE("gen", errs() << "randcf: Erase branch instruction:"
                                      << *((BranchInst *)*i) << "\n");
        (*i)->eraseFromParent(); // erase the branch
        break;
      }
      case 1: {
        // second opaque predicate x == x * (x + 1) - x^2

        op = BinaryOperator::Create(
            Instruction::Add, (Value *)opX,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 1, false), "",
            (*i)); // x + 1
        op1 = BinaryOperator::Create(Instruction::Mul, (Value *)opX, op, "",
                                     (*i)); // x * (x + 1)
        op = BinaryOperator::Create(Instruction::Mul, (Value *)opX,
                                    (Value *)opX, "", (*i)); // x^2
        op1 = BinaryOperator::Create(Instruction::Sub, op1, op, "",
                                     (*i)); // x * (x + 1) - x^2
        condition = new ICmpInst((*i), ICmpInst::ICMP_EQ, opX,
                                 op1); // x == x * (x + 1) - x^2
        condition2 = new ICmpInst(
            (*i), ICmpInst::ICMP_SLT, opY,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 10, false));
        op1 = BinaryOperator::Create(Instruction::Or, (Value *)condition,
                                     (Value *)condition2, "", (*i));
        // Replace the condition
        BranchInst::Create(((BranchInst *)*i)->getSuccessor(0),
                           ((BranchInst *)*i)->getSuccessor(1), (Value *)op1,
                           ((BranchInst *)*i)->getParent());
        DEBUG_WITH_TYPE("gen", errs() << "randcf: Erase branch instruction:"
                                      << *((BranchInst *)*i) << "\n");
        (*i)->eraseFromParent(); // erase the branch
        break;
      }
      case 2: {
        // third opaque predicate x == 3 * x * (x - 2) - 3 * x^2 + 7 * x
        op1 = BinaryOperator::Create(
            Instruction::Mul, (Value *)opX,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 3, false), "",
            (*i)); // 3 * x
        op = BinaryOperator::Create(
            Instruction::Sub, (Value *)opX,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 2, false), "",
            (*i)); // x - 2
        op1 = BinaryOperator::Create(Instruction::Mul, op1, op, "",
                                     (*i)); // 3 * x * (x - 2)
        op = BinaryOperator::Create(Instruction::Mul, (Value *)opX,
                                    (Value *)opX, "",
                                    (*i)); // x^2
        op = BinaryOperator::Create(
            Instruction::Mul, op,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 3, false), "",
            (*i)); // 3 * x^2
        op1 = BinaryOperator::Create(Instruction::Sub, op1, op, "",
                                     (*i)); //  3 * x * (x - 2) - 3 * x^2
        op = BinaryOperator::Create(
            Instruction::Mul, (Value *)opX,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 7, false), "",
            (*i)); // 7 * x
        op1 = BinaryOperator::Create(Instruction::Add, op1, op, "",
                                     (*i)); // 3 * x * (x - 2) - 3 * x^2 + 7 * x
        condition = new ICmpInst((*i), ICmpInst::ICMP_EQ, opX,
                                 op1); // x == 3 * x * (x - 2) - 3 * x^2 + 7 * x
        condition2 = new ICmpInst(
            (*i), ICmpInst::ICMP_SLT, opY,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 10, false));
        op1 = BinaryOperator::Create(Instruction::Or, (Value *)condition,
                                     (Value *)condition2, "", (*i));
        // Replace the condition
        BranchInst::Create(((BranchInst *)*i)->getSuccessor(0),
                           ((BranchInst *)*i)->getSuccessor(1), (Value *)op1,
                           ((BranchInst *)*i)->getParent());
        DEBUG_WITH_TYPE("gen", errs() << "randcf: Erase branch instruction:"
                                      << *((BranchInst *)*i) << "\n");
        (*i)->eraseFromParent(); // erase the branch
        break;
      }
      case 3: {
        // fourth opaque predicate x == (x - 1) * (x + 3) - (x + 4) * (x - 3) -
        // 9
        op = BinaryOperator::Create(
            Instruction::Sub, (Value *)opX,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 1, false), "",
            (*i)); // x - 1
        op1 = BinaryOperator::Create(
            Instruction::Add, (Value *)opX,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 3, false), "",
            (*i)); // x + 3
        op = BinaryOperator::Create(Instruction::Mul, op, op1, "",
                                    (*i)); // (x - 1) * (x + 3)
        op2 = BinaryOperator::Create(
            Instruction::Add, (Value *)opX,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 4, false), "",
            (*i)); // x + 4
        // op = BinaryOperator::Create(Instruction::Mul, op1, op, "", (*i));//
        // (x + 4) * (x - 1) * (x + 3)
        op3 = BinaryOperator::Create(
            Instruction::Sub, (Value *)opX,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 3, false), "",
            (*i)); // x - 3
        op3 = BinaryOperator::Create(Instruction::Mul, op2, op3, "",
                                     (*i)); // (x + 4) * (x - 3)
        // op1 = BinaryOperator::Create(Instruction::Mul, op1, op1, "", (*i));
        op1 = BinaryOperator::Create(
            Instruction::Sub, op, op3, "",
            (*i)); // (x - 1) * (x + 3) - (x + 4) * (x - 3)
        op1 = BinaryOperator::Create(
            Instruction::Sub, (Value *)op1,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 9, false), "",
            (*i)); // (x - 1) * (x + 3) - (x + 4) * (x - 3) - 9
        condition =
            new ICmpInst((*i), ICmpInst::ICMP_EQ, opX,
                         op1); // x == (x - 1) * (x + 3) - (x + 4) * (x - 3) - 9
        condition2 = new ICmpInst(
            (*i), ICmpInst::ICMP_SLT, opY,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 10, false));
        op1 = BinaryOperator::Create(Instruction::Or, (Value *)condition,
                                     (Value *)condition2, "", (*i));
        // Replace the condition
        BranchInst::Create(((BranchInst *)*i)->getSuccessor(0),
                           ((BranchInst *)*i)->getSuccessor(1), (Value *)op1,
                           ((BranchInst *)*i)->getParent());
        DEBUG_WITH_TYPE("gen", errs() << "randcf: Erase branch instruction:"
                                      << *((BranchInst *)*i) << "\n");
        (*i)->eraseFromParent(); // erase the branch
        break;
      }
      case 4: {
        // fifth opaque predicate x^2 − 34 * y^2 != −1
        op1 = BinaryOperator::Create(Instruction::Mul, (Value *)opX,
                                     (Value *)opX, "", (*i)); // x^2
        op = BinaryOperator::Create(
            Instruction::Mul, (Value *)opY,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 34, false), "",
            (*i)); // 34 * y
        op = BinaryOperator::Create(Instruction::Mul, op, (Value *)opY, "",
                                    (*i)); // 34 * y^2
        op1 = BinaryOperator::Create(Instruction::Sub, op1, op, "",
                                     (*i)); // x^2 - 34 * y^2
        condition =
            new ICmpInst((*i), ICmpInst::ICMP_NE, op1,
                         ConstantInt::get(Type::getInt32Ty(M.getContext()), -1,
                                          false)); // x^2 - 34 * y^2 != -1
        condition2 = new ICmpInst(
            (*i), ICmpInst::ICMP_SLT, opY,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 10, false));
        op1 = BinaryOperator::Create(Instruction::Or, (Value *)condition,
                                     (Value *)condition2, "", (*i));
        // Replace the condition
        BranchInst::Create(((BranchInst *)*i)->getSuccessor(0),
                           ((BranchInst *)*i)->getSuccessor(1), (Value *)op1,
                           ((BranchInst *)*i)->getParent());
        DEBUG_WITH_TYPE("gen", errs() << "randcf: Erase branch instruction:"
                                      << *((BranchInst *)*i) << "\n");
        (*i)->eraseFromParent(); // erase the branch
        break;
      }
      case 5: {
        // sixth opaque predicate x^2 − 221 * y^2 != −1
        op1 = BinaryOperator::Create(Instruction::Mul, (Value *)opX,
                                     (Value *)opX, "", (*i)); // x^2
        op = BinaryOperator::Create(
            Instruction::Mul, (Value *)opY,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 221, false), "",
            (*i)); // 221 * y
        op = BinaryOperator::Create(Instruction::Mul, op, (Value *)opY, "",
                                    (*i)); // 221 * y^2
        op1 = BinaryOperator::Create(Instruction::Sub, op1, op, "",
                                     (*i)); // x^2 - 221 * y^2
        condition =
            new ICmpInst((*i), ICmpInst::ICMP_NE, op1,
                         ConstantInt::get(Type::getInt32Ty(M.getContext()), -1,
                                          false)); // x^2 - 221 * y^2 != -1
        condition2 = new ICmpInst(
            (*i), ICmpInst::ICMP_SLT, opY,
            ConstantInt::get(Type::getInt32Ty(M.getContext()), 10, false));
        op1 = BinaryOperator::Create(Instruction::Or, (Value *)condition,
                                     (Value *)condition2, "", (*i));
        // Replace the condition
        BranchInst::Create(((BranchInst *)*i)->getSuccessor(0),
                           ((BranchInst *)*i)->getSuccessor(1), (Value *)op1,
                           ((BranchInst *)*i)->getParent());
        DEBUG_WITH_TYPE("gen", errs() << "randcf: Erase branch instruction:"
                                      << *((BranchInst *)*i) << "\n");
        (*i)->eraseFromParent(); // erase the branch
        break;
      }
      default:
        break;
      }
    }
    // Erase all the associated conditions we found
    for (std::vector<Instruction *>::iterator i = toDelete.begin();
         i != toDelete.end(); ++i) {
      DEBUG_WITH_TYPE("gen", errs() << "randcf: Erase condition instruction:"
                                    << *((Instruction *)*i) << "\n");
      (*i)->eraseFromParent();
    }

    // Only for debug
    DEBUG_WITH_TYPE("cfg", errs() << "randcf: End of the pass, here are the "
                                     "graphs after doFinalization\n");
    for (Module::iterator mi = M.begin(), me = M.end(); mi != me; ++mi) {
      DEBUG_WITH_TYPE("cfg",
                      errs() << "randcf: Function " << mi->getName() << "\n");
      DEBUG_WITH_TYPE("cfg", mi->viewCFG());
    }

    return true;
  } // end of doFinalization
}; // end of struct RandomControlFlow : public FunctionPass
} // namespace

char RandomControlFlow::ID = 0;
static RegisterPass<RandomControlFlow> X("randomcf",
                                         "inserting random control flow");

Pass *llvm::createRandom() { return new RandomControlFlow(); }

Pass *llvm::createRandom(bool flag) { return new RandomControlFlow(flag); }
