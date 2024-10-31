import typing as typ
from .ebnf import RIFPSParser
import pyparsing as pp
from . import container
from rdflib import Graph
from ..rif_namespace import rif_namespaces

def parse_rifpl(stream: typ.Union[str]):
    myparser = RIFPSParser(loglevel=5)
    #q = ebnf.Document.parse_file(stream)[0]

    container.init_global_graph()
    try:
        q = myparser.parseString(stream)[0]
    except (pp.ParseException, pp.ParseSyntaxException) as err:
        raise Exception("\n"+err.explain()) from err
    #return q
    #g = container.global_graph
    g = Graph()
    for key, ns in rif_namespaces.items():
        g.namespace_manager.bind(key, ns)
    q.add_to_global_graph(g)
    #print(g.serialize())
    #raise Exception()
    container.close_global_graph()
    return g
