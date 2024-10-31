from rdflib.namespace import DefinedNamespace, Namespace
from rdflib.term import URIRef
from rdflib import XSD

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
    Subclass: URIRef
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
    Name: URIRef
    namedargs: URIRef
    NamedArg: URIRef
    argname: URIRef
    argvalue: URIRef
    _NS = Namespace("http://www.w3.org/2007/rif#")
    #_NS = Namespace("http://www.w3.org/2007/rif-builtin-predicate#")


# see `https://www.w3.org/TR/2013/REC-rif-dtb-20130205/#The_Base_and_Prefix_Directives`_
FUNC = Namespace("http://www.w3.org/2007/rif-builtin-function#")
PRED = Namespace("http://www.w3.org/2007/rif-builtin-predicate#")
# see `https://www.w3.org/TR/2013/REC-rif-prd-20130205/#Built-in_functions.2C_predicates_and_actions`_
ACT = Namespace("http://www.w3.org/2007/rif-builtin-action#")

rif_namespaces = {
        # see `https://www.w3.org/TR/2013/REC-rif-dtb-20130205/#The_Base_and_Prefix_Directives`_
        "xs": XSD,
        "rif": RIF,
        "func": FUNC,
        "pred": PRED,
        # see `https://www.w3.org/TR/2013/REC-rif-prd-20130205/#Built-in_functions.2C_predicates_and_actions`_
        "act": ACT,
        }
"""dict: Namespaces expected in a rif document."""
