import rdflib_rif

import unittest
import logging
logger = logging.getLogger(__name__)

import rdflib
import rdflib.serializer
import rdflib.parser
import rdflib.plugin

from rdflib import compare

import importlib.resources
from . import data
input8bld = importlib.resources.files(data).joinpath("bld-8.rif")
output8bld = importlib.resources.files(data).joinpath("bld-8_pp.ttl")
testrif_without_xmlns = importlib.resources.files(data).joinpath("testrif_without_xmlns.rif")

class TestParsingPlugin(unittest.TestCase):
    def setUp(self):
        rdflib.plugin.register("rifxml", rdflib.parser.Parser,
                               "rdflib_rif", "RIFXMLParser")

    @unittest.skip("Im not sure, what the result of this test should look "
                   "like. Either the parser automaticly adds the missing"
                   "xmlns or it fails with a meaningfull message.")
    def test_withoutxmlns(self, testfile=testrif_without_xmlns):
        raise NotImplementedError()

    def test_riftottl(self, rifxml_format="rifxml",
                    rifdocument=input8bld,
                    ttldocument=output8bld):
        """Tests rule translation from Swrl to Clips and if given
        rule works correctly.

        :TODO: Missing printout of watching facts and rules
        """
        g = rdflib.Graph()
        g.parse(rifdocument, format=rifxml_format)
        out_g = rdflib.Graph().parse(ttldocument)
        cg = compare.to_isomorphic(g)
        cout_g = compare.to_isomorphic(out_g)
        try:
            self.assertEqual(cg, cout_g)
        except AssertionError:
            shared, additional, missing = rdflib.compare.graph_diff(cg, cout_g)
            g.bind("rif", "http://www.w3.org/2007/rif#")
            additional.bind("rif", "http://www.w3.org/2007/rif#")
            missing.bind("rif", "http://www.w3.org/2007/rif#")
            shared.bind("rif", "http://www.w3.org/2007/rif#")
            logger.debug("produced graph:\n%s" % g.serialize())
            logger.debug("additional info:\n%s"% additional.serialize())
            logger.debug("missing info:\n%s"% missing.serialize())
            logger.debug("shared info:\n%s"%shared.serialize())
            raise

if __name__=='__main__':
    logging.basicConfig( level=logging.WARNING )
    #flowgraph_logger = logging.getLogger("find_generationpath.Flowgraph")
    #graphstate_logger = logging.getLogger("find_generationpath.Graphstate")
    #flowgraph_logger.setLevel(logging.DEBUG)
    #graphstate_logger.setLevel(logging.DEBUG)
    logger.setLevel(logging.DEBUG)
    unittest.main()
