import rdflib_rif

import logging
logger = logging.getLogger(__name__)
import pytest
import rdflib_rif
import rdflib
import rdflib.compare

import importlib.resources
from . import data
DoNew = importlib.resources.files(data).joinpath("PRD/DoNew/")

class TestTranslateRIFPSConsistency:
    def setUp(self):
        rdflib.plugin.register("rifps", rdflib.parser.Parser,
                               "rdflib_rif", "RIFMarkupParser")
        rdflib.plugin.register("RIFPRD-PS", rdflib.parser.Parser,
                               "rdflib_rif", "RIFMarkupParser")

        rdflib.plugin.register("rif", rdflib.parser.Parser,
                               "rdflib_rif", "RIFXMLParser")
        rdflib.plugin.register("RIF/XML", rdflib.parser.Parser,
                               "rdflib_rif", "RIFXMLParser")

    @pytest.mark.parametrize("rifps_file, rif_file", [
        (DoNew.joinpath("DoNew.rifps"), DoNew.joinpath("DoNew.rif")),
        ])
    def test_PRD(self, rifps_file, rif_file):
        self.setUp()
        g_rifps = rdflib.Graph().parse(rifps_file, format="rifps", pretty_logging=True)
        g_rif = rdflib.Graph().parse(rif_file, format="rif")
        from rdflib.compare import to_isomorphic
        cg_ps = to_isomorphic(g_rifps)
        cg = to_isomorphic(g_rif)
        logger.info("File that was loaded: %s" % rifps_file)
        try:
            assert cg == cg_ps
        except AssertionError as err:
            shared, in_orig, in_md = rdflib.compare.graph_diff(cg, cg_ps)
            for x in (shared, in_orig, in_md, cg, cg_ps):
                x.bind("rif", "http://www.w3.org/2007/rif#")
            logger.debug("more information for err: %s" %err)
            logger.debug("first graph:\n%s" % cg.serialize())
            logger.debug("second graph:\n%s" % cg_ps.serialize())
            logger.debug("info only in first:\n%s" % in_orig.serialize())
            logger.debug("info only in second:\n%s" % in_md.serialize())
            logger.debug("shared info:\n%s" % shared.serialize())
            raise err
        #raise Exception(rifps_file, rif_file)
