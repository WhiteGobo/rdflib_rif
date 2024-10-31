from typing import Iterable
from rdflib import Graph
from rdflib.term import Node
from rdflib.collection import Collection


def extract_action_vars_from_action(g: Graph, action: Node) -> Iterable[Node]:
    q = Collection(g, action)
    raise Exception("not implemented")

