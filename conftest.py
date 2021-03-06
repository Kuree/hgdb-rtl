import pytest
import os
import ooze


def get_vector_file_fn(filename):
    vector_dir = os.path.join(os.path.dirname(os.path.realpath(__file__)), "tests", "vectors")
    f = os.path.join(vector_dir, filename)
    assert os.path.exists(f)
    return f


@pytest.fixture()
def get_vector_file():
    return get_vector_file_fn


@pytest.fixture(autouse=True)
def clear_kratos_context():
    ooze.clear_cache()
