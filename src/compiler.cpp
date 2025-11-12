#include <llvm/IR/IRBuilder.h>
#include "compiler.hpp"

using namespace Flame;
using namespace llvm;

void Compiler::compile() {
    // This file is just for testing LLVM IR generation for now
    for (auto& stmt : program) {
        if (stmt->is<FunctionStmt>()) {
            println(stmt);
        }
    }

    LLVMContext ctx;
    Module module("my_module", ctx);

    auto llvm_i32 = Type::getInt32Ty(ctx);
    auto llvm_f64 = Type::getDoubleTy(ctx);

    auto MyStructTy = StructType::create(
        ctx,
        {llvm_i32, llvm_f64},
        "MyStruct"
    );

    vector<Type*> ArgTypes(2, llvm_i32);
    auto FuncTy = FunctionType::get(llvm_i32, ArgTypes, false);
    auto AddFunc = Function::Create(
        FuncTy,
        Function::ExternalLinkage,
        "add",
        module
    );

    auto ArgIter = AddFunc->arg_begin();
    ArgIter->setName("a");
    (++ArgIter)->setName("b");

    auto BB = BasicBlock::Create(ctx, "entry", AddFunc);

    IRBuilder Builder(BB);

    auto A = AddFunc->getArg(0);
    auto B = AddFunc->getArg(1);

    auto Sum = Builder.CreateAdd(A, B, "sum");

    Builder.CreateRet(Sum);

    println(module);
}
