#include "rtl.hh"

#include <filesystem>
#include <fstream>

#include "fmt/format.h"
#include "hgdb/json.hh"
#include "slang/ast/ASTVisitor.h"
#include "slang/diagnostics/TextDiagnosticClient.h"
#include "slang/driver/Driver.h"

namespace hgdb::rtl {

class NotSupportedException : public std::runtime_error {
public:
    explicit NotSupportedException(const std::string &what) : std::runtime_error(what) {}
    NotSupportedException(const std::string &what, slang::SourceLocation loc)
        : std::runtime_error(what), loc_(loc) {}

    void report(slang::TextDiagnosticClient &client) const {
        if (loc_) {
            slang::Diagnostic diag(slang::diag::NotYetSupported, *loc_);
            diag << std::string_view(this->what());
            slang::ReportedDiagnostic reported_diag(diag);
            reported_diag.severity = slang::DiagnosticSeverity::Fatal;
            reported_diag.location = *loc_;
            reported_diag.formattedMessage = what();
            client.report(reported_diag);
            slang::OS::printE(client.getString());
        } else {
            slang::OS::printE(what());
        }
    }

private:
    std::optional<slang::SourceLocation> loc_;
};

class ExpressionPrinter : public slang::ast::ASTVisitor<ExpressionPrinter, false, true> {
public:
    explicit ExpressionPrinter(const slang::SourceLocation &loc) : loc_(loc) {}

    void handle(const slang::ast::StringLiteral &) {
        throw NotSupportedException("StringLiteral not supported for hgdb", loc_);
    }

    void handle(const slang::ast::IntegerLiteral &i) {
        auto v = i.getValue();
        auto uint_opt = v.as<int>();
        int value = uint_opt ? *uint_opt : 0;
        ss_ << value;
    }

    void handle(const slang::ast::NamedValueExpression &n) { handle(n.symbol); }

    void handle(const slang::ast::ValueSymbol &sym) { ss_ << sym.name; }

    void handle(const slang::ast::ConversionExpression &c) { visitDefault(c.operand()); }

    void handle(const slang::ast::LValueReferenceExpression &) {
        throw NotSupportedException("LValueReferenceExpression not supported for hgdb", loc_);
    }

    void handle(const slang::ast::UnaryExpression &expr) {
        auto const &op = expr.operand();
        ss_ << "(";
        // simple ones that have C++ operator overloaded
        switch (expr.op) {
            case slang::ast::UnaryOperator::Minus:
                ss_ << "-";
                op.visit(*this);
                break;
            case slang::ast::UnaryOperator::Plus:
                ss_ << "+";
                op.visit(*this);
                break;
            case slang::ast::UnaryOperator::Predecrement:
                ss_ << "--";
                op.visit(*this);
                break;
            case slang::ast::UnaryOperator::Preincrement:
                ss_ << "++";
                op.visit(*this);
                break;
            case slang::ast::UnaryOperator::LogicalNot:
                ss_ << "!";
                op.visit(*this);
                break;
            case slang::ast::UnaryOperator::BitwiseNot:
                ss_ << "~";
                op.visit(*this);
                break;
            case slang::ast::UnaryOperator::Postdecrement:
                op.visit(*this);
                ss_ << "--";
                break;
            case slang::ast::UnaryOperator::Postincrement:
                op.visit(*this);
                ss_ << "++";
                break;
            default:
                throw NotSupportedException(fmt::format("Unary type {0} not supported for hgdb",
                                                        slang::ast::toString(expr.op)),
                                            loc_);
        }
    }

    void handle(const slang::ast::BinaryExpression &expr) {
        auto const &left = expr.left();
        auto const &right = expr.right();
        ss_ << "(";
        left.visit(*this);
        switch (expr.op) {
            case slang::ast::BinaryOperator::Add:
                ss_ << " + ";
                break;
            case slang::ast::BinaryOperator::Subtract:
                ss_ << " - ";
                break;
            case slang::ast::BinaryOperator::Multiply:
                ss_ << " * ";
                break;
            case slang::ast::BinaryOperator::Mod:
                ss_ << " % ";
                break;
            case slang::ast::BinaryOperator::Divide:
                ss_ << " / ";
                break;
            case slang::ast::BinaryOperator::BinaryAnd:
                ss_ << " & ";
                break;
            case slang::ast::BinaryOperator::BinaryOr:
                ss_ << " | ";
                break;
            case slang::ast::BinaryOperator::BinaryXor:
                ss_ << " ^ ";
                break;
            case slang::ast::BinaryOperator::Equality:
                ss_ << " == ";
                break;
            case slang::ast::BinaryOperator::Inequality:
                ss_ << " != ";
                break;
            case slang::ast::BinaryOperator::LogicalAnd:
                ss_ << " && ";
                break;
            case slang::ast::BinaryOperator::LogicalOr:
                ss_ << " || ";
                break;
            case slang::ast::BinaryOperator ::LessThan:
                ss_ << " < ";
                break;
            case slang::ast::BinaryOperator::LessThanEqual:
                ss_ << " <= ";
                break;
            case slang::ast::BinaryOperator::GreaterThan:
                ss_ << " > ";
                break;
            case slang::ast::BinaryOperator::GreaterThanEqual:
                ss_ << " >= ";
                break;
            case slang::ast::BinaryOperator::LogicalShiftLeft:
                ss_ << " << ";
                break;
            case slang::ast::BinaryOperator::LogicalShiftRight:
                ss_ << " >> ";
                break;
            default:
                throw NotSupportedException(
                    fmt::format("Unsupported operator {0}", slang::ast::toString(expr.op)),
                    expr.sourceRange.start());
        }

        right.visit(*this);
        ss_ << ")";
    }

    void handle(const slang::ast::ConditionalExpression &) {
        throw NotSupportedException("ConditionalExpression not supported for hgdb", loc_);
    }

    void handle(const slang::ast::ElementSelectExpression &sym) {
        auto const &value = sym.value();
        auto const &selector = sym.selector();

        value.visit(*this);

        // depends on whether the selector is a constant or not
        std::optional<uint64_t> select_value;
        if (selector.constant) {
            auto const &v = *selector.constant;
            select_value = get_constant_value(v);
            if (select_value) {
                ss_ << '[' << *select_value << ']';
            } else {
                throw NotSupportedException("Only integer selection is supported", loc_);
            }
        } else {
            ss_ << ".";
            selector.visit(*this);
        }
    }

    void handle(const slang::ast::RangeSelectExpression &) {
        throw NotSupportedException("RangeSelectExpression not supported for hgdb expression",
                                    loc_);
    }

    void handle(const slang::ast::ConcatenationExpression &) {
        throw NotSupportedException("ConcatenationExpression not supported for hgdb expression",
                                    loc_);
    }

    void handle(const slang::ast::CallExpression &) {
        throw NotSupportedException("CallExpression not supported for hgdb expression", loc_);
    }

    [[nodiscard]] std::string str() { return ss_.str(); }

private:
    const slang::SourceLocation &loc_;
    std::stringstream ss_;

    static std::optional<int64_t> get_constant_value(const slang::ConstantValue &constant) {
        if (!constant.isInteger()) {
            return std::nullopt;
        }
        return *constant.integer().as<int64_t>();
    }
};

class Serializer : public slang::ast::ASTVisitor<Serializer, true, false> {
public:
    Serializer(hgdb::json::SymbolTable &table, slang::SourceManager &sm) : table_(table), sm_(sm) {}

    void handle(const slang::ast::InstanceSymbol &inst) {
        auto const *def = &inst.getDefinition();
        hgdb::json::Module *inst_module = nullptr;
        auto visited = definitions_.find(def->name) != definitions_.end();
        if (!visited) {
            inst_module = table_.add_module(std::string(def->name));
        } else {
            inst_module = def_module_mapping_.at(def->name);
        }

        auto *parent_module = current_module_;

        if (parent_module) {
            parent_module->add_instance(std::string(inst.name), inst_module);
        }

        // only visit unique definition
        if (visited) return;
        definitions_.emplace(def->name);

        current_module_ = inst_module;
        def_module_mapping_.emplace(def->name, current_module_);

        auto *temp_current_scope_ = current_scope_;
        auto *temp_root_scope_ = root_scope_;
        // to make things easier, everything is wrapped in a single scope
        current_scope_ = current_module_->create_scope<hgdb::json::Scope<>>();
        auto filename = std::string(sm_.getFileName(inst.location));
        filename = std::filesystem::canonical(std::filesystem::absolute(filename)).string();
        current_scope_->filename = std::string(filename);
        root_scope_ = current_scope_;

        visitDefault(inst);
        // reset
        current_module_ = parent_module;
        current_scope_ = temp_current_scope_;
        root_scope_ = temp_root_scope_;
    }

    void handle(const slang::ast::NetSymbol &net) {
        if (!current_module_) return;
        current_module_->add_variable(json::Variable{
            .name = std::string(net.name), .value = std::string(net.name), .rtl = true});
    }

    void handle(const slang::ast::VariableSymbol &var) {
        json::Variable v{
            .name = std::string(var.name), .value = std::string(var.name), .rtl = true};
        if (root_scope_ == current_scope_) {
            current_module_->add_variable(v);
        } else {
            // it's a "declare" in some blocks
            // we don't support this rn
        }
    }

    void handle(const slang::ast::ExpressionStatement &stmt) {
        auto const &expr = stmt.expr;
        bool handled = false;
        auto line = sm_.getLineNumber(stmt.sourceRange.start());
        if (expr.kind == slang::ast::ExpressionKind::Assignment) {
            auto const &assign_expr = expr.as<slang::ast::AssignmentExpression>();
            auto const &target_sym = assign_expr.left();
            // need to print out expression
            ExpressionPrinter p(expr.sourceRange.start());
            try {
                target_sym.visit(p);
                json::Variable v;
                v.name = p.str();
                v.value = v.name;
                v.rtl = true;
                current_scope_->create_scope<hgdb::json::VarStmt>(v, line, false);
                handled = true;
            } catch (NotSupportedException &) {
                // don't care if not working
            }
        }
        if (!handled) {
            current_scope_->create_scope<hgdb::json::Scope<>>(line);
        }
    }

    void handle(const slang::ast::ConditionalStatement &stmt) {
        auto const &predicates = stmt.conditions;
        std::vector<std::string> predicate_strs;
        predicate_strs.reserve(predicates.size());
        for (auto const &cond : predicates) {
            ExpressionPrinter p(cond.expr->sourceRange.start());
            cond.expr->visit(p);
            predicate_strs.emplace_back(p.str());
        }
        current_condition_ =
            fmt::format("{}", fmt::join(predicate_strs.begin(), predicate_strs.end(), "&&"));

        visit_block_stmt(&stmt.ifTrue);

        if (stmt.ifFalse) {
            current_condition_ =
                fmt::format("!({})", fmt::join(predicate_strs.begin(), predicate_strs.end(), "&&"));
            visit_block_stmt(stmt.ifFalse);
        }
    }

    void handle(const slang::ast::CaseStatement &stmt) {
        auto const &target = stmt.expr;
        std::string target_str;
        {
            ExpressionPrinter p(target.sourceRange.start());
            target.visit(p);
            target_str = p.str();
        }

        std::vector<std::string> all_conditions;

        for (auto const &case_ : stmt.items) {
            auto values = case_.expressions;
            std::vector<std::string> values_str;
            values_str.reserve(values.size());
            for (auto *e : values) {
                if (e) {
                    ExpressionPrinter p(e->sourceRange.start());
                    e->visit(p);
                    values_str.emplace_back(p.str());
                    if (stmt.defaultCase) {
                        all_conditions.emplace_back(p.str());
                    }
                }
            }
            for (auto &s : values_str) {
                s = fmt::format("({0} == {1})", s, target_str);
            }
            current_condition_ =
                fmt::format("{0}", fmt::join(values_str.begin(), values_str.end(), " && "));
            visit_block_stmt(case_.stmt);
        }

        if (stmt.defaultCase) {
            for (auto &s : all_conditions) {
                s = fmt::format("({0} != {1})", target_str, s);
            }
            current_condition_ =
                fmt::format("{0}", fmt::join(all_conditions.begin(), all_conditions.end(), " && "));
            visit_block_stmt(stmt.defaultCase);
        }
    }

    void visit_block_stmt(const slang::ast::Statement *stmt) {
        auto *temp = current_scope_;
        auto line = sm_.getLineNumber(stmt->sourceRange.start());
        current_scope_ =
            current_scope_->template create_scope<hgdb::json::Scope<std::nullptr_t>>(line);
        if (!current_condition_.empty()) {
            current_scope_->condition = std::move(current_condition_);
        }
        stmt->visit(*this);
        current_scope_ = temp;
    }

private:
    hgdb::json::SymbolTable &table_;
    slang::SourceManager &sm_;
    hgdb::json::Scope<std::nullptr_t> *current_scope_ = nullptr;
    hgdb::json::ScopeBase *root_scope_ = nullptr;
    hgdb::json::Module *current_module_ = nullptr;
    std::unordered_set<std::string_view> definitions_;
    std::string current_condition_;

    // hgdb implementation doesn't care about naming. track it here
    std::unordered_map<std::string_view, hgdb::json::Module *> def_module_mapping_;
};

void SymbolTableGenerator::output() {
    slang::driver::Driver driver;
    std::optional<std::string> filename;
    std::optional<bool> show_help;
    driver.addStandardArgs();
    driver.cmdLine.add("-o", filename, "Output symbol table file");
    driver.cmdLine.add("-h,--help", show_help, "Display available options");
    bool res = driver.parseCommandLine(static_cast<int>(commandline_args_.size()),
                                       commandline_args_.data());
    if (!res) return;
    if (show_help == true) {
        slang::OS::print(
            fmt::format("{}", driver.cmdLine.getHelpText("hgdb-rtl symbol table generator")));
        return;
    }

    res = driver.parseAllSources();
    if (!res) return;
    auto compilation = driver.createCompilation();

    hgdb::json::SymbolTable table("SystemVerilog");

    Serializer serializer(table, driver.sourceManager);
    auto instances = compilation->getRoot().topInstances;
    for (auto *instance : instances) {
        instance->visit(serializer);
    }
    table.compress();

    std::string output;

    try {
        output = table.output();
    } catch (NotSupportedException &e) {
        e.report(*driver.diagClient);
    }

    if (filename) {
        if (filename == "-") {
            slang::OS::print(output);
        } else {
            std::ofstream stream(*filename);
            if (!stream.bad()) {
                stream << output;
            }
            stream.close();
        }
    }
}

}  // namespace hgdb::rtl