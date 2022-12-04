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


def test_nested_if():
    table = get_json("nested_if")
    top = table["table"][0]

    comb = top["scope"][0]
    assert comb["filename"] == get_vector_file("nested_if")
    assert len(comb["scope"]) == 2
    assert comb["scope"][0]["condition"] == "a"
    assert comb["scope"][1]["condition"] == "!(a)"

    if_true = comb["scope"][0]["scope"][0]
    assert if_true["condition"] == "b"
    if_false = comb["scope"][0]["scope"][1]
    assert if_false["condition"] == "!(b)"


if __name__ == "__main__":
    test_nested_if()
