import pytest
from pytest import param, mark
from rdflib import Graph
import rdflib.plugin
from rdflib.compare import to_isomorphic, graph_diff
import logging
logger = logging.getLogger(__name__)


def test_compare_rif_and_rifps(register_rif_format,
                               different_files_with_same_information):

    q = iter(different_files_with_same_information.items())
    compare_format, filepath = q.__next__()
    compare_graph = Graph().parse(filepath, format=compare_format)
    compset = set(compare_graph)
    iso_comparegraph = to_isomorphic(compare_graph)
    #logger.critical(compare_graph.serialize())
    logger.debug(filepath)
    for format_, filepath in q:
        g = Graph().parse(filepath, format=format_)
        logger.critical(filepath)
        #logger.critical(g.serialize())
        iso_g = to_isomorphic(g)
        in_both, in_comp, in_g = graph_diff(iso_comparegraph, iso_g)
        try:
            assert not list(in_comp) and not list(in_g)
        except Exception:
            logger.info(
                    "Comparing two different graphs holds different "
                    "information\nComparegraph has format %s and holds info:"
                    "\n%s\n\ncompared to graph with format %s:\n%s\n\n"
                    % (compare_format, in_comp.serialize(),
                       format_, in_g.serialize()))
            logger.debug("Shared info netween both graphs:%s"
                         % in_both.serialize())
            logger.debug("base_graph:\n%s" % iso_comparegraph.serialize())
            #logger.debug("second_graph:\n%s" % iso_g.serialize())
            raise
