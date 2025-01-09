import pytest
from pytest import param, mark

from . import GroupPriorityBehaviour
from . import MultipleActionVars

custom_test_data = [
        param(GroupPriorityBehaviour,
              id="GroupPriorityBehaviour"),
        param(MultipleActionVars,
              id="MultipleActionVars"),
        ]
