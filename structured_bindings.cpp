#include "ast_visitor.hpp"
#include "rewriter.hpp"

#include <clang/AST/ASTContext.h>

#include <iostream>

namespace cftf {

// TODO: Should this operate with QualTypes instead?
static bool TypeHasStdTupleSizeSpecialization(clang::ASTContext& context, const clang::Type* type) {
    auto tu_decl = context.getTranslationUnitDecl();
    auto& id = context.Idents.get("std");
    auto std_id = context.DeclarationNames.getIdentifier(&id);
    auto lookup_result = tu_decl->lookup(std_id);
    assert(lookup_result.size() < 2);
    if (lookup_result.size() == 0) {
        // No standard library headers included
        // => std::tuple_size is not available
        return false;
    }

    clang::NamespaceDecl* std_decl = clang::dyn_cast<clang::NamespaceDecl>(lookup_result.front());
    auto tuple_size_lookup_result = std_decl->lookup(&context.Idents.get("tuple_size"));
    assert(tuple_size_lookup_result.size() < 2);
    if (tuple_size_lookup_result.size() == 0) {
        // <tuple> is not included => std::tuple_size is not available
        return false;
    }

    auto tuple_size_decl = clang::dyn_cast<clang::ClassTemplateDecl>(tuple_size_lookup_result.front());
    assert(tuple_size_decl);

    std::cerr << "Checking if any std::tuple_size specializations match" << std::endl;

    auto specs = tuple_size_decl->specializations();
    auto spec = std::find_if(std::begin(specs), std::end(specs),
                                [type](clang::ClassTemplateSpecializationDecl* spec) {
                                    auto&& template_args = spec->getTemplateArgs();
                                    assert(template_args.size() == 1);
                                    bool match = (template_args[0].getAsType().getTypePtrOrNull() == type->getAs<clang::RecordType>());
                                    // NOTE: tuple_size is only considered when
                                    //       a proper definition is provided
                                    //       (plain forward declarations will
                                    //       not have any effect)
                                    if (match && !spec->isCompleteDefinition()) {
                                        std::cerr << "Skipping incomplete specialization" << std::endl;
                                        return false;
                                    }
                                    return match;
                                });
    return (spec != std::end(specs));
}

// TODO: Support bit fields. I guess we could just create a local struct type to define a bitfield of the given size?
bool ASTVisitor::VisitDecompositionDecl(clang::DecompositionDecl* decl) {
    auto init_expr = decl->getInit();
    assert(init_expr);
    auto unqualified_type = init_expr->getType().getTypePtrOrNull();
    assert(unqualified_type);

    std::string temp_name;
    for (auto* binding : decl->bindings()) {
        if (!temp_name.empty()) {
            temp_name += '_';
        }
        temp_name += binding->getNameAsString();
    }
    auto rewritten_decl = "auto " + temp_name + " = " + GetClosedStringFor(decl->getInit()->getLocStart(), decl->getInit()->getLocEnd()) + ";\n";

    if (unqualified_type->isArrayType()) {
        // TODO: For auto, create a new array as a copy of the reference one
        // TODO: Decompose array elements; decomposed type is "add_reference_t<cv-qualified element_type>"
        // TODO: Assign decomposed elements to "referenced_array[i]"
        std::cerr << "Decomposing array" << std::endl;
        // TODO: Implement.
        return true;
    } else if (TypeHasStdTupleSizeSpecialization(context, unqualified_type)) {
        std::cerr << "Decomposing via get<>" << std::endl;

        // TODO: The decomposed types are references to
        //       "std::tuple_element<i, E>::type", but we still need to decide
        //       what kind of reference. For now, we just use "auto(&&)"
        //       everywhere, but that may not be sufficient in the future. In
        //       some cases, the bindings shouldn't be C++-references at all,
        //       but instead should act like transparent references (the type
        //       of which behaves like a value)

        auto&& bindings = decl->bindings();
        for (auto binding_it = bindings.begin(); binding_it != bindings.end(); ++binding_it) {
            auto* binding = *binding_it;
            auto index = std::distance(bindings.begin(), binding_it);
            auto* var = binding->getHoldingVar();

            // TODO: Does a plain "get" properly ADL-match all
            //       cases here? Things to consider:
            //       * std::get
            //       * Free function get(Object)
            //       * Member function get()
            //       * Friend member function get(Object)
            rewritten_decl += "auto&& " + var->getNameAsString() + " = get<" + std::to_string(index) + ">(" + temp_name + ");\n";
        }
    } else {
        // Decomposed types are references to the respective data members
        std::cerr << "Decomposing struct" << std::endl;

        auto bindings = decl->bindings();
        auto init_expr_record = init_expr->getType().getTypePtr()->getAsCXXRecordDecl();
        if (init_expr_record) {
            init_expr->getType().dump();

            struct FindNonEmptyBase {
                clang::CXXRecordDecl* operator()(clang::CXXBaseSpecifier& base) {
                    auto type = base.getType().getTypePtr();
                    assert(type);
                    auto record = type->getAsCXXRecordDecl();
                    assert(record);
                    std::cerr << "Checking base ";
                    type->dump();
                    std::cerr << std::endl;
                    return (*this)(record);
                }

                clang::CXXRecordDecl* operator()(clang::CXXRecordDecl* record) {
                    auto&& fields = record->fields();
                    if (fields.begin() != fields.end()) {
                        std::cerr << "Done." << std::endl;
                        return record;
                    }
                    for (auto&& base : record->bases()) {
                        auto match = (*this)(base);
                        if (match) {
                            return match;
                        }
                    }

                    return nullptr;
                }
            };

            // Current structure may not have any direct data members, in which case they must have been inherited from a parent
            auto non_empty_base = FindNonEmptyBase{}(init_expr_record);
            assert(non_empty_base);
            auto fields = non_empty_base->fields();

            auto fields_it = fields.begin();
            for (size_t i = 0; i < bindings.size(); ++i) {
                assert(fields_it != fields.end());
                assert(fields_it->getFieldIndex() == i);
                rewritten_decl += "auto&& " + bindings[i]->getNameAsString() + " = " + temp_name + "." + fields_it->getNameAsString() + ";\n";
                ++fields_it;
            }
            assert(fields_it == fields.end());

            rewriter->ReplaceTextIncludingEndToken(decl->getSourceRange(), rewritten_decl);
        } else {
            // This will happen in dependent contexts. TODO!
            std::cerr << "Right-hand side is not a CXXRecordDecl; not sure what to do with this..." << std::endl;
        }
    }

    rewriter->ReplaceTextIncludingEndToken(decl->getSourceRange(), rewritten_decl);

    return true;
}

} // namespace cftf
