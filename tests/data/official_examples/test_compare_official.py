import pytest
from pytest import param, mark
from rdflib import Graph
import rdflib.plugin
from rdflib.compare import to_isomorphic, graph_diff
import logging
logger = logging.getLogger(__name__)

from . import PRD_PET_AssertRetract2_conclusion
from . import PRD_PET_AssertRetract2_premise
from . import PRD_PET_AssertRetract_conclusion
from . import PRD_PET_AssertRetract_premise
from . import PRD_PET_Assert_conclusion
from . import PRD_PET_Assert_premise
from . import PRD_PET_Modify_conclusion
from . import PRD_PET_Modify_loop_conclusion
from . import PRD_PET_Modify_loop_premise
from . import PRD_PET_Modify_premise

from . import Core_NET_Local_Constant_conclusion
from . import Core_NET_Local_Constant_premise
from . import Core_NET_Local_Predicate_conclusion
from . import Core_NET_Local_Predicate_premise
from . import Core_NET_NestedListsAreNotFlatLists_conclusion
from . import Core_NET_NestedListsAreNotFlatLists_premise
from . import Core_NET_Non_Annotation_Entailment_conclusion
from . import Core_NET_Non_Annotation_Entailment_premise
from . import Core_NET_RDF_Combination_SubClass_conclusion
from . import Core_NET_RDF_Combination_SubClass_premise
from . import Core_NST_Core_NonSafeness_2_input
from . import Core_NST_Core_NonSafeness_input
from . import Core_NST_No_free_variables_input

from . import Core_PST_Core_Safeness_2_input
from . import Core_PST_Core_Safeness_3_input
from . import Core_PST_Core_Safeness_input

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


@pytest.fixture(params=[
    param(PRD_PET_AssertRetract2_conclusion,
          id="PRD_PET_AssertRetract2_conclusion"),
    param(PRD_PET_AssertRetract2_premise,
          id="PRD_PET_AssertRetract2_premise"),
    param(PRD_PET_AssertRetract_conclusion,
          marks=mark.skip("error in rdflib.compare"),
          id="PRD_PET_AssertRetract_conclusion"),
    param(PRD_PET_AssertRetract_premise,
          marks=mark.skip("error in rdflib.compare"),
          id="PRD_PET_AssertRetract_premise"),
    param(PRD_PET_Assert_conclusion,
          id="PRD_PET_Assert_conclusion"),
    param(PRD_PET_Assert_premise,
          id="PRD_PET_Assert_premise"),
    param(PRD_PET_Modify_conclusion,
          id="PRD_PET_Modify_conclusion"),
    param(PRD_PET_Modify_loop_conclusion,
          id="PRD_PET_Modify_loop_conclusion"),
    param(PRD_PET_Modify_loop_premise,
          id="PRD_PET_Modify_loop_premise"),
    param(PRD_PET_Modify_premise,
          id="PRD_PET_Modify_premise"),
    param(Core_NET_Local_Constant_conclusion,
          id="Core_NET_Local_Constant_conclusion"),
    param(Core_NET_Local_Constant_premise,
          id="Core_NET_Local_Constant_premise"),
    param(Core_NET_Local_Predicate_conclusion,
          id="Core_NET_Local_Predicate_conclusion"),
    param(Core_NET_Local_Predicate_premise,
          id="Core_NET_Local_Predicate_premise"),
    param(Core_NET_NestedListsAreNotFlatLists_conclusion,
          id="Core_NET_NestedListsAreNotFlatLists_conclusion"),
    param(Core_NET_NestedListsAreNotFlatLists_premise,
          id="Core_NET_NestedListsAreNotFlatLists_premise"),
    param(Core_NET_Non_Annotation_Entailment_conclusion,
          id="Core_NET_Non_Annotation_Entailment_conclusion"),
    param(Core_NET_Non_Annotation_Entailment_premise,
          marks=mark.skip("not implemented yet"),
          id="Core_NET_Non_Annotation_Entailment_premise"),
    param(Core_NET_RDF_Combination_SubClass_conclusion,
          id="Core_NET_RDF_Combination_SubClass_conclusion"),
    param(Core_NET_RDF_Combination_SubClass_premise,
          id="Core_NET_RDF_Combination_SubClass_premise"),
    param(Core_NST_Core_NonSafeness_2_input,
          marks=mark.skip("not implemented yet"),
          id="Core_NST_Core_NonSafeness_2_input"),
    param(Core_NST_Core_NonSafeness_input,
          marks=mark.skip("not implemented yet"),
          id="Core_NST_Core_NonSafeness_input"),
    param(Core_NST_No_free_variables_input,
          marks=mark.skip("not implemented yet"),
          id="Core_NST_No_free_variables_input"),
    param(Core_PST_Core_Safeness_2_input,
          marks=mark.skip("not implemented yet"),
          id="Core_PST_Core_Safeness_2_input"),
    param(Core_PST_Core_Safeness_3_input,
          marks=mark.skip("not implemented yet"),
          id="Core_PST_Core_Safeness_3_input"),
    param(Core_PST_Core_Safeness_input,
          marks=mark.skip("not implemented yet"),
          id="Core_PST_Core_Safeness_input"),
    ])           
def different_files_with_same_information(request) -> dict[str, str]:
    return request.param.format_to_file


def test_compare_rif_and_rifps(register_rif_format,
                               different_files_with_same_information):
    q = iter(different_files_with_same_information.items())
    compare_format, filepath = q.__next__()
    compare_graph = Graph().parse(filepath, format=compare_format)
    compset = set(compare_graph)
    iso_comparegraph = to_isomorphic(compare_graph)
    for format_, filepath in q:
        g = Graph().parse(filepath, format=format_)
        iso_g = to_isomorphic(g)
        in_both, in_comp, in_g = graph_diff(iso_comparegraph, iso_g)
        try:
            assert not list(in_comp) and not list(in_g)
        except Exception:
            logger.critical(
                    "Comparing two different graphs holds different "
                    "information\nComparegraph has format %s and holds info:"
                    "\n%s\n\ncompared to graph with format %s:\n%s\n\n"
                    "Shared info netween both graphs:%s"
                    % (compare_format, in_comp.serialize(),
                       format_, in_g.serialize(), in_both.serialize()))
            raise
