import os
import json
import subprocess


def get_vector_file(name):
    test_dir = os.path.dirname(os.path.abspath(__file__))
    vector_dir = os.path.join(test_dir, "vectors")
    path = os.path.join(vector_dir, name + ".sv")
    assert os.path.isfile(path)
    return path


def get_json(name):
    filename = get_vector_file(name)
    output = subprocess.check_output(["hgdb-rtl", filename, "-o", "-"]).decode("ascii")
    return json.loads(output)


def test_hierarchy():
    table = get_json("hierarchy")
    top = table["table"][0]
    inst = top["instances"][0]
    assert inst["name"] == "inst"
    assert inst["module"] == "child"


if __name__ == "__main__":
    test_hierarchy()
