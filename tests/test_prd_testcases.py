import rdflib_rif

import logging
logger = logging.getLogger(__name__)
import pytest
import rdflib_rif
import rdflib

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
        g_rifps = rdflib.Graph().parse(rifps_file, format="rifps")
        g_rif = rdflib.Graph().parse(rif_file, format="rif")
        raise Exception(rifps_file, rif_file)
