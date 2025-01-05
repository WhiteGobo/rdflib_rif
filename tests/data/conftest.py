import pytest
from .official_examples.conftest import official_test_data
from .custom_examples.conftest import custom_test_data

@pytest.fixture(params=official_test_data + custom_test_data)
def different_files_with_same_information(request) -> dict[str, str]:
    return request.param.format_to_file
