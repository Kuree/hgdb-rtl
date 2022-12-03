#include "rtl.hh"

int main(int argc, char **argv) {
    std::vector<const char *> args;
    args.reserve(argc);
    for (auto i = 0; i < argc; i++) args.emplace_back(argv[i]);
    hgdb::rtl::SymbolTableGenerator gen(args);
    gen.output();
}