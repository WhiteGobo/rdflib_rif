import pytest
from pytest import param, mark
import pyparsing as pp

import rdflib
from rdflib import Graph
import rdflib.plugin
#from rdflib.compare import to_isomorphic, graph_diff
import logging
logger = logging.getLogger(__name__)

from collections import namedtuple

find_suberrormessage_info = namedtuple(
        "find_suberrormessage_info",
        ["inputdata", "expected_substring", "id"])


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

_info = [
        find_suberrormessage_info("""Document( 
 Prefix(ex <http://example.com/test#>)
  Group (
   Forall ?X (External( ex:member(?X)))
  )
 )
    """, "expected RULE", "Rule expected"),
        ]
@pytest.fixture(params=[param(x, id=x.id) for x in _info])
def info(request) -> find_suberrormessage_info:
    return request.param

def test_find_suberrormessage_parse_rifps(register_rif_format,
                                          info: find_suberrormessage_info):
    try:
        Graph().parse(data=info.inputdata, format="rifps")
        errmsg = None
    except Exception as err:
        errmsg = str(err)
    if errmsg is None:
        raise Exception("Parsing didnt produce error.")
    logger.debug("error producing data:\n{}".format(info.inputdata))
    assert info.expected_substring in errmsg,\
            "Expected substring '{}' in errormessage. Got:\n{}\n"\
            .format(info.expected_substring, errmsg)

