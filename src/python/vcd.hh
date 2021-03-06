#ifndef HGDB_RTL_PYTHON_VCD_HH
#define HGDB_RTL_PYTHON_VCD_HH

#include "../vcd.hh"
#include "data_source.hh"

struct VCDSignal: public QueryObject {
    explicit VCDSignal(Ooze *ooze): QueryObject(ooze) {}
    std::string path;
    std::string name;
    hgdb::vcd::VCDSignal *signal = nullptr;

    [[nodiscard]] std::map<std::string, pybind11::object> values() const override;
};


class VCD: public DataSource {
public:
    explicit VCD(std::string path);

    [[nodiscard]] inline std::vector<py::handle> provides() const override {
        return {py::type::of<VCDSignal>()};
    }

    std::shared_ptr<QueryArray> get_selector(py::handle handle) override;

    std::shared_ptr<QueryObject> bind(const std::shared_ptr<QueryObject> &obj, const py::object &type) override;

    [[nodiscard]] auto get_stats() const { return db_->get_stats(); }

    void on_added(Ooze *ooze) override;

    std::unique_ptr<FilterMapperGenerator> filter_generator() const override;

private:
    std::unique_ptr<hgdb::vcd::VCDDatabase> db_;
    std::string filename_;
    Ooze *ooze_ = nullptr;
    void parse();
};

#endif  // HGDB_RTL_PYTHON_VCD_HH
