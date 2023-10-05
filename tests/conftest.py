import pytest
import rdflib.plugin
import rdflib.parser

@pytest.fixture
def register_rif_format() -> None:
    rdflib.plugin.register("rifps", rdflib.parser.Parser,
                           "rdflib_rif", "RIFMarkupParser")
    rdflib.plugin.register("RIFPRD-PS", rdflib.parser.Parser,
                           "rdflib_rif", "RIFMarkupParser")
    rdflib.plugin.register("rif", rdflib.parser.Parser,
                           "rdflib_rif", "RIFXMLParser")
    rdflib.plugin.register("RIF/XML", rdflib.parser.Parser,
                           "rdflib_rif", "RIFXMLParser")
