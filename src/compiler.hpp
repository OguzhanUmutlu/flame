#pragma once
#include "parser.hpp"
#include <llvm/IR/IRBuilder.h>

using namespace llvm;

namespace Flame {
    struct TypeOrValue {
        size_t index; // 0 = Value*, 1 = Type*
        void* ptr;

        explicit TypeOrValue(Value* v) : index(0), ptr(static_cast<void*>(v)) {
        }

        explicit TypeOrValue(Type* t) : index(1), ptr(static_cast<void*>(t)) {
        }

        [[nodiscard]] constexpr Value* value() const {
            if (index != 0) throw std::runtime_error("TypeOrValue does not hold a Value*");
            return static_cast<Value*>(ptr);
        }

        [[nodiscard]] constexpr Type* type() const {
            if (index != 1) throw std::runtime_error("TypeOrValue does not hold a Type*");
            return static_cast<Type*>(ptr);
        }
    };

    struct StructInfo;

    struct FunctionInfo {
        Function* llvmFunc;
        PropertyExpr* name;
        hashmap<IdentifierToken, Type*> paramTypes;
        hashmap<IdentifierToken, TypeOrValue> templates;
    };

    struct StructInfo {
        StructType* llvm_type;
        PropertyExpr* name;
        hashmap<IdentifierToken, Type*> fields;
        hashmap<IdentifierToken, TypeOrValue> templates;
        hashmap<string, FunctionInfo> methods;

        bool operator==(const StructInfo& rhs) const {
            if (name != rhs.name) return false;

            return std::ranges::all_of(templates, [&](const auto& pair) {
                const auto& [k, v] = pair;
                return rhs.templates.at(k).ptr == v.ptr;
            });
        }
    };

    struct Compiler {
        Parser& parser;
        vector<ASTNode>& program;
        hashmap<IdentifierToken, StructStmt&> structs;

        vector<Function*> compiled_functions;

        Compiler(Parser& parser, vector<ASTNode>& program) : parser(parser), program(program) {
        }

        void compile();
    };
}

namespace llvm {
    static std::ostream& operator<<(std::ostream& os, const Module& module) {
        std::string buffer;
        raw_string_ostream rso(buffer);
        module.print(rso, nullptr);
        rso.flush();
        os << buffer;
        return os;
    }
}
