#ifndef _FUNCTION_WRAPPER_H_
#define _FUNCTION_WRAPPER_H_

#include "llvm/Transforms/Utils/BasicBlockUtils.h"
#include "llvm/Transforms/Utils/ModuleUtils.h"
#include "llvm/IR/Verifier.h"
#include "llvm/IR/NoFolder.h"
#include "llvm/IR/CFG.h"
#include "llvm/IR/Dominators.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/InlineAsm.h"
#include "llvm/CryptoUtils.h"
#include "llvm/Transforms/Scalar.h"
#if __has_include("llvm/Transforms/Utils.h")
#include "llvm/Transforms/Utils.h"
#endif


namespace llvm {
	ModulePass* createFunctionWrapperPass();
	ModulePass* createFunctionWrapperPass(bool flag);
}

#endif