//===- FunctionWrapper.cpp - FunctionWrapper Pass -------------------------===//
//
// This file implements the FunctionWrapper pass, which is responsible for
// obfuscating function calls in LLVM modules. The pass identifies function
// calls that are eligible for obfuscation based on certain criteria, and
// replaces them with wrapper functions. The wrapper functions are created to
// call the original functions, adding an extra layer of indirection. This pass
// is useful for code obfuscation purposes, as it makes it harder to understand
// the control flow of the program.
//
//===----------------------------------------------------------------------===//

#include "llvm/Transforms/Obfuscation/FunctionWrapper.h"
#include "llvm/IR/CallSite.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InstIterator.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Pass.h"
#include <cstdlib>
#include <fstream>
#include <iostream>
#include <llvm/Transforms/Obfuscation/Utils.h>
#include <string>
using namespace llvm;
using namespace std;

// Command line options for the FunctionWrapper pass
static cl::opt<int>
    ProbRate("fw_prob",
             cl::desc("Choose the probability [%] For Each CallSite To Be "
                      "Obfuscated By FunctionWrapper"),
             cl::value_desc("Probability Rate"), cl::init(30), cl::Optional);
static cl::opt<int> ObfTimes(
    "fw_times",
    cl::desc(
        "Choose how many time the FunctionWrapper pass loop on a CallSite"),
    cl::value_desc("Number of Times"), cl::init(2), cl::Optional);

namespace llvm {
struct FunctionWrapper : public ModulePass {
  static char ID;
  bool flag;

  // Default constructor for the FunctionWrapper pass
  FunctionWrapper() : ModulePass(ID) { this->flag = true; }

  // Constructor for the FunctionWrapper pass with a flag parameter
  FunctionWrapper(bool flag) : ModulePass(ID) { this->flag = flag; }

  // Returns the name of the pass
  StringRef getPassName() const override {
    return StringRef("FunctionWrapper");
  }

  // Runs the FunctionWrapper pass on the given module
  bool runOnModule(Module &M) override {
    vector<CallSite *> callsites;

    // Iterate over all functions in the module
    for (Module::iterator iter = M.begin(); iter != M.end(); iter++) {
      Function &F = *iter;

      // Check if the function is eligible for obfuscation
      if (toObfuscate(flag, &F, "fw")) {
        errs() << "Running FunctionWrapper On " << F.getName() << "\n";

        // Iterate over all instructions in the function
        for (inst_iterator fi = inst_begin(&F); fi != inst_end(&F); fi++) {
          Instruction *Inst = &*fi;

          // Check if the instruction is a function call
          if (isa<CallInst>(Inst) || isa<InvokeInst>(Inst)) {
            // Randomly decide whether to obfuscate the call site
            if ((int)llvm::cryptoutils->get_range(100) <= ProbRate) {
              callsites.push_back(new CallSite(Inst));
            }
          }
        }
      }
    }

    // Obfuscate the selected call sites
    for (CallSite *CS : callsites) {
      for (int i = 0; i < ObfTimes && CS != nullptr; i++) {
        CS = HandleCallSite(CS);
      }
    }

    return true;
  }

  // Handles the obfuscation of a call site
  CallSite *HandleCallSite(CallSite *CS) {
    Value *calledFunction = CS->getCalledFunction();

    // If the called function is null, try to strip pointer casts
    if (calledFunction == nullptr) {
      calledFunction = CS->getCalledValue()->stripPointerCasts();
    }

    // Filter out indirect calls and intrinsics
    if (calledFunction == nullptr ||
        (!isa<ConstantExpr>(calledFunction) &&
         !isa<Function>(calledFunction)) ||
        CS->getIntrinsicID() != Intrinsic::ID::not_intrinsic) {
      return nullptr;
    }

    if (Function *tmp = dyn_cast<Function>(calledFunction)) {
      if (tmp->getName().startswith("clang.")) {
        // Clang Intrinsic
        return nullptr;
      }
    }

    // Create a new function to act as a wrapper for the original function
    vector<Type *> types;
    for (unsigned i = 0; i < CS->getNumArgOperands(); i++) {
      types.push_back(CS->getArgOperand(i)->getType());
    }
    FunctionType *ft =
        FunctionType::get(CS->getType(), ArrayRef<Type *>(types), false);
    Function *func =
        Function::Create(ft, GlobalValue::LinkageTypes::InternalLinkage,
                         "FunctionWrapper", CS->getParent()->getModule());

    // Add the wrapper function to the compiler-used list
    appendToCompilerUsed(*func->getParent(), {func});

    BasicBlock *BB = BasicBlock::Create(func->getContext(), "", func);
    IRBuilder<> IRB(BB);
    vector<Value *> params;
    for (auto arg = func->arg_begin(); arg != func->arg_end(); arg++) {
      params.push_back(&*arg);
    }
    Value *retval = IRB.CreateCall(
        ConstantExpr::getBitCast(cast<Function>(calledFunction),
                                 CS->getCalledValue()->getType()),
        ArrayRef<Value *>(params));

    // Create return instruction based on the return type
    if (ft->getReturnType()->isVoidTy()) {
      IRB.CreateRetVoid();
    } else {
      IRB.CreateRet(retval);
    }

    // Replace the original call site with the wrapper function
    CS->setCalledFunction(func);
    CS->mutateFunctionType(ft);
    Instruction *Inst = CS->getInstruction();
    delete CS;
    return new CallSite(Inst);
  }
};

// Create a new instance of the FunctionWrapper pass
ModulePass *createFunctionWrapperPass() { return new FunctionWrapper(); }

// Create a new instance of the FunctionWrapper pass with a flag parameter
ModulePass *createFunctionWrapperPass(bool flag) {
  return new FunctionWrapper(flag);
}
} // namespace llvm

// Register the FunctionWrapper pass
char FunctionWrapper::ID = 0;
static RegisterPass<FunctionWrapper> X("funcwra", "Enable FunctionWrapper");