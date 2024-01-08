#include <llvm-c/Support.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <mlir/CAPI/IR.h>
#include <mlir/CAPI/Support.h>
#include <mlir/CAPI/Wrap.h>
#include <mlir/Dialect/LLVMIR/LLVMTypes.h>
#include <mlir/IR/Types.h>
#include <mlir/Target/LLVMIR/ModuleTranslation.h>

extern "C" LLVMModuleRef mlirTranslateModuleToLLVMIR(MlirOperation module,
                                          LLVMContextRef context) {
  mlir::Operation *moduleOp = unwrap(module);

  llvm::LLVMContext *ctx = llvm::unwrap(context);

  std::unique_ptr<llvm::Module> llvmModule = mlir::translateModuleToLLVMIR(
      moduleOp, *ctx);

  LLVMModuleRef moduleRef = llvm::wrap(llvmModule.release());

  return moduleRef;
}
