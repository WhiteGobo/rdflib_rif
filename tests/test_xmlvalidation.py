import rdflib_rif
from rdflib_rif.rifxml_validation import rif_schema
from rdflib_rif.rifxml_validation import validate_rifxml
from rdflib_rif import rifxml_validation

import unittest
import logging
logger = logging.getLogger(__name__)

import rdflib
import rdflib.serializer
import rdflib.parser
import rdflib.plugin

from rdflib import compare
from lxml import etree

import importlib.resources
from . import data
_exampleriffile = importlib.resources.files(data).joinpath("bld-8.rif")
import pytest

@pytest.mark.skip(reason="bld-8.rif is currently not available, whatever that is")
class TestParsingPlugin(unittest.TestCase):
    def test_riftottl(self, filename=_exampleriffile):
        """Validates given file as rif.
        """
        rifxml_validation.validate_rifxml.main(filename)

        with open(filename, "r") as f:
            doc = etree.parse(f)
        #print(rif_schema.validate(doc))
        rif_schema.assertValid(doc)


if __name__=='__main__':
    logging.basicConfig( level=logging.WARNING )
    #flowgraph_logger = logging.getLogger("find_generationpath.Flowgraph")
    #graphstate_logger = logging.getLogger("find_generationpath.Graphstate")
    #flowgraph_logger.setLevel(logging.DEBUG)
    #graphstate_logger.setLevel(logging.DEBUG)
    logger.setLevel(logging.DEBUG)
    unittest.main()
