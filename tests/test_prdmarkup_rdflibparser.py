import rdflib_rif

import unittest
import logging
logger = logging.getLogger(__name__)

import rdflib
import rdflib.serializer
import rdflib.parser
import rdflib.plugin

from rdflib_rif.rifpl import ebnf as Parsing

import importlib.resources
from . import testrif_documents
blueprint = importlib.resources.files(testrif_documents).joinpath("test.rifpl")


class TestMarkupParser(unittest.TestCase):
    def test_term_Name(self, prdmarkup_suffix="rifpl",
                       markupdocument=blueprint):
        rdflib.plugin.register(prdmarkup_suffix, rdflib.parser.Parser,
                               "rdflib_rif", "RIFMarkupParser")
        g = rdflib.Graph()
        g.parse(markupdocument, format=prdmarkup_suffix)
        print(g.serialize())



if __name__=='__main__':
    logging.basicConfig( level=logging.WARNING )
    #flowgraph_logger = logging.getLogger("find_generationpath.Flowgraph")
    #graphstate_logger = logging.getLogger("find_generationpath.Graphstate")
    #flowgraph_logger.setLevel(logging.DEBUG)
    #graphstate_logger.setLevel(logging.DEBUG)
    logger.setLevel(logging.DEBUG)
    unittest.main()
