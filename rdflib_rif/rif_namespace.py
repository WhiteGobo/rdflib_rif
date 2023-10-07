from rdflib.namespace import DefinedNamespace, Namespace
from rdflib.term import URIRef

class RIF(DefinedNamespace):
    _fail = True
    _extras = [
            "id",
            "vars",# Term only in rdf/rif. See :term:`table 3`
            "sentences",# Term only in rdf/rif. See :term:`table 3`
            "formulas",# Term only in rdf/rif. See :term:`table 3`
            "slot",# Term only in xml/rif. See :term:`table 3`
            "Slot",# Term only in rdf/rif. See :term:`table 3`
            "slots",# Term only in rdf/rif. See :term:`table 3`
            "slotkey",# Term only in rdf/rif. See :term:`table 3`
            "slotvalue",# Term only in rdf/rif. See :term:`table 3`
            "if",
            "type",
            ]

    Document: URIRef
    Import: URIRef
    location: URIRef
    profile: URIRef
    payload: URIRef
    Group: URIRef
    Const: URIRef
    meta: URIRef
    directive: URIRef
    Exists: URIRef
    directives: URIRef
    sentence: URIRef
    Forall: URIRef
    Then: URIRef
    declare: URIRef
    Var: URIRef
    Frame: URIRef
    formula: URIRef
    Implies: URIRef
    then: URIRef
    And: URIRef
    Or: URIRef
    Atom: URIRef
    op: URIRef
    args: URIRef
    Equal: URIRef
    left: URIRef
    right: URIRef
    External: URIRef
    content: URIRef
    Expr: URIRef
    varname: URIRef
    constIRI: URIRef
    constname: URIRef
    value: URIRef
    iri: URIRef
    local: URIRef
    Member: URIRef
    List: URIRef
    Do: URIRef
    Retract: URIRef
    Assert: URIRef
    Modify: URIRef
    New: URIRef
    INeg: URIRef
    Execute: URIRef
    _NS = Namespace("http://www.w3.org/2007/rif#")
    #_NS = Namespace("http://www.w3.org/2007/rif-builtin-predicate#")
