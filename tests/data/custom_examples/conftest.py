import pytest
from pytest import param, mark

from . import GroupPriorityBehaviour
from . import MultipleActionVars
from . import MultiplePattern

custom_test_data = [
        param(GroupPriorityBehaviour,
              id="GroupPriorityBehaviour"),
        param(MultipleActionVars,
              id="MultipleActionVars"),
        param(MultiplePattern,
              id="MultiplePattern"),
        ]
