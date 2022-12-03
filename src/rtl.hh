#ifndef HGDB_RTL_RTL_HH
#define HGDB_RTL_RTL_HH

#include <string>
#include <vector>

namespace hgdb::rtl {
class SymbolTableGenerator {
public:
    explicit SymbolTableGenerator(const std::vector<const char *> &commandline_args)
        : commandline_args_(commandline_args) {}

    void output();

private:
    std::vector<const char *> commandline_args_;
};
}  // namespace hgdb::rtl

#endif  // HGDB_RTL_RTL_HH
