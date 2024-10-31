"""This module supplies all the classes, that are equivalent to classes 
in rdf/rif or in xml/rif.
"""
from typing import Optional, List, Tuple, Iterable
import abc
from abc import ABC, abstractmethod
import itertools as it
import rdflib
from rdflib.term import Node, URIRef, Literal
from rdflib import *
from rdflib import RDF, Graph
import xml.etree.ElementTree as ET
import pyparsing as pp
from dataclasses import dataclass

from ..utils import extract_action_vars_from_action
from .container_classes import MetaContainer, rif_element, TextContainer, prefix_transporter, MissingPrefix

_RIF = rdflib.Namespace("http://www.w3.org/2007/rif#")
_XSD = rdflib.Namespace("http://www.w3.org/2001/XMLSchema#")
_FUNC = rdflib.Namespace("http://www.w3.org/2007/rif-builtin-function#")
_PRED = rdflib.Namespace("http://www.w3.org/2007/rif-builtin-predicate#")
_ACT = rdflib.Namespace("http://www.w3.org/2007/rif-builtin-action#")

global_graph: "Optional[rdflib.Graph]" = None
localiriToNode: "Optional[Dict[str, Node]]" = None

def init_global_graph():
    global global_graph
    global_graph = rdflib.Graph()
    global localiriToNode
    localiriToNode = {}
    #This are expected namespace
    # see `https://www.w3.org/TR/2013/REC-rif-dtb-20130205/#The_Base_and_Prefix_Directives`_
    global_graph.namespace_manager.bind("rdf", RDF)
    global_graph.namespace_manager.bind("xs", _XSD)
    global_graph.namespace_manager.bind("rif", _RIF)
    global_graph.namespace_manager.bind("func", _FUNC)
    global_graph.namespace_manager.bind("pred", _PRED)
    # see `https://www.w3.org/TR/2013/REC-rif-prd-20130205/#Built-in_functions.2C_predicates_and_actions`_
    global_graph.namespace_manager.bind("act", _ACT)

def close_global_graph():
    global global_graph
    global_graph = None
    global localiriToNode
    localiriToNode = None

class PyParse_Parser(abc.ABC):
    @classmethod
    def _parse_pyparsing(cls, parseresults: pp.ParseResults):
        try:
            return cls.parse_rifpl(**parseresults.as_dict())
        except TypeError as err:
            raise Exception(f"Got wrong data from parser for class {cls}. "
                            "Got: %s" % (parseresults.asDict())) from err
        except Exception as err:
            raise Exception(f"raised {type(err)} from "
                            f"parseruslt: {parseresults}") from err

    @classmethod
    @abstractmethod
    def parse_rifpl(cls):
        raise Exception(f"not implemented {cls}")


class RDFSObject:
    idnode: Node
    additional_information: List[Tuple[Node, Node, Node]]

    def __init__(self, idnode=None,
                 additional_information: Optional[Iterable]=None):
        self.idnode = BNode() if idnode is None else idnode
        self.additional_information\
                = [] if additional_information is None\
                else list(additional_information)

    def add_meta(self, meta: Optional["MetaObject"]):
        if meta is not None:
            self.idnode = meta.idnode
            if meta.additional_information is not None:
                self.additional_information.extend(meta.additional_information)

    def add_to_global_graph(self, g=None):
        for subj, pred, obj in self.additional_information:
            add_to_global_graph(subj, pred, obj, g)


def add_to_global_graph(subj: Node, pred: Node, obj: Node | RDFSObject,
                        g: Optional[Graph] = None):
    global global_graph
    if g is None:
        g = global_graph
    if isinstance(obj, RDFSObject):
        o = obj.idnode
        obj.add_to_global_graph(g)
    elif isinstance(obj, Node):
        o = obj
    else:
        raise ValueError(obj, type(obj))
    try:
        g.add((subj, pred, o))
    except AssertionError as err:
        raise Exception("wrong use of add_to_global_graph") from err


def add_collection_to_global_graph(
        subj: Node, pred: Node, collection: Iterable[Node | RDFSObject],
        g: Optional[Graph] = None, base=None):
    global global_graph
    if g is None:
        g = global_graph
    coll: list[Node] = []
    for obj in collection:
        if isinstance(obj, RDFSObject):
            coll.append(obj.idnode)
            obj.add_to_global_graph(g)
        elif isinstance(obj, Node):
            coll.append(obj)
        else:
            raise ValueError(collection)
    try:
        if len(collection) == 0:
            g.add((subj, pred, RDF.nil))
        else:
            base = BNode() if base is None else base
            g.add((subj, pred, base))
            rdflib.collection.Collection(g, base, coll)
    except AssertionError as err:
        raise Exception("wrong use of add_collection_to_global_graph") from err


class MetaObject(PyParse_Parser):
    def __init__(self, idnode: Optional[Node] = None,
                 additional_information: Optional[List[Tuple[Node, Node, Node]]] = None):
        self.idnode = BNode() if idnode is None else idnode
        self.additional_information = additional_information

    @classmethod
    def parse_rifpl(cls, iri=None, config_multi=None, config_single=None,
                    constIRI=None):
        """
        :param constIRI: Im not sure where this option comes from.
        """
        if iri is not None:
            iri = iri.as_node()
        if config_multi is not None:
            assert config_single is None
            raise NotImplementedError("multi", config_multi)
            add_info = list(it.chain.from_iterable(x.to_triples()
                                                   for x in config_multi))
        elif config_single is not None:

            assert config_multi is None
            raise NotImplementedError("single", config_single)
            add_info = list(config_single.to_triples())
            if iri is None:
                add_info[0][0]
        else:
            add_info = []
        return cls(iri, add_info)


def complete_object(parseresults: pp.ParseResults) -> Node:
    meta: Optional[MetaObject]
    rdfs_object: RDFSObject
    try:
        meta, rdfs_object = parseresults
    except ValueError as err:
        raise Exception(parseresults) from err
    assert isinstance(rdfs_object, RDFSObject),\
            f"Can only complete RDFSObject, got : {rdfs_object}, {list(parseresults)}"
    rdfs_object.add_meta(meta)
    #rdfs_object.add_to_global_graph()
    #print(f"completed object{rdfs_object} and return id {rdfs_object.idnode}")
    return rdfs_object

def createLocalIri(parseresult):
    raise Exception("not implemented")

def curieToIri(parseresult) -> URIRef:
    shortcut: str
    suffix: str
    try:
        shortcut, suffix = parseresult
    except ValueError as err:
        raise Exception(parseresult)

    global global_graph
    pref = shortcut
    suff = suffix
    try:
        iri = global_graph.namespace_manager.expand_curie(f"{pref}:{suff}")
    except Exception as err:
        raise Exception(f"Missing prefix {pref}") from err
    return iri

class ObjSlot(PyParse_Parser, RDFSObject):
    slotkey: "ObjConst"
    slotvalue: "ObjConst"
    def __init__(self, slotkey, slotvalue):
        super().__init__()
        self.slotkey = slotkey
        self.slotvalue = slotvalue

    @classmethod
    def parse_rifpl(cls, slotkey: Node, slotvalue: Node):
        return cls(slotkey, slotvalue)

    def add_to_global_graph(self, g: Optional[Graph] = None):
        super().add_to_global_graph()
        add_to_global_graph(self.idnode, RDF.type, _RIF.Slot, g)
        add_to_global_graph(self.idnode, _RIF.slotkey, self.slotkey, g)
        add_to_global_graph(self.idnode, _RIF.slotvalue, self.slotvalue, g)

class ObjNamedArg(PyParse_Parser, RDFSObject):
    argname: str
    argvalue: "ObjConst"
    def __init__(self, argname, argvalue):
        super().__init__()
        self.argname = argname
        self.argvalue = argvalue

    @classmethod
    def parse_rifpl(cls, argname: str, argvalue: "ObjConst"):
        return cls(argname, argvalue)

    def add_to_global_graph(self, g: Optional[Graph] = None):
        super().add_to_global_graph()
        add_to_global_graph(self.idnode, RDF.type, _RIF.NamedArg, g)
        argname = Literal(self.argname)
        add_to_global_graph(self.idnode, _RIF.argname, argname, g)
        add_to_global_graph(self.idnode, _RIF.argvalue, self.argvalue, g)

class ObjConst(PyParse_Parser, RDFSObject):
    @abstractmethod
    def as_node(self): pass

class ObjConstValue(ObjConst):
    value: Node
    def __init__(self, value: Literal):
        super().__init__()
        assert isinstance(value, Literal), [value, type(value)]
        self.value = value

    def as_node(self):
        return self.value

    @classmethod
    def parse_rifpl(cls, value: Node):
        if isinstance(value, list):
            value, = value
        return cls(value)

    def add_to_global_graph(self, g: Optional[Graph] = None):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Const, g)
        add_to_global_graph(self.idnode, _RIF.value, self.value, g)

class ObjConstLiteral(ObjConst):
    @classmethod
    def parse_rifpl(cls, lexical_or_value, lang=None, datatype=None):
        if lang is not None:
            return ObjConstLiteralPlainLiteral(lexical_or_value, lang)
        elif datatype == str(RDF.PlainLiteral) and lang is None and "@" in lexical_or_value:
            #handling of errorneos input of rif testcase
            value, lang = lexical_or_value.split("@")
            return ObjConstLiteralPlainLiteral(value, lang)
        elif datatype is not None:
            if str(datatype) == str(_RIF.iri):
                return ObjConstIRI(Literal(lexical_or_value, datatype=XSD.anyURI))
            elif str(datatype) == str(_RIF.local):
                return ObjConstLocal(lexical_or_value)
            elif str(datatype) == str(XSD.string):
                return ObjConstLiteralString(lexical_or_value)
            else:
                return ObjConstLiteralDatatype(lexical_or_value, datatype)
        else:
            return ObjConstLiteralString(lexical_or_value)

class ObjConstLiteralString(ObjConstLiteral):
    value: str
    datatype: URIRef = XSD.string
    def __init__(self, value: str):
        super().__init__()
        self.value = value

    def as_node(self):
        return Literal(self.value, datatype=self.datatype)

    @classmethod
    def parse_rifpl(cls, **kwargs):
        raise NotImplementedError("use ObjConstLiteral instead")

    def add_to_global_graph(self, g: Optional[Graph] = None):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Const, g)
        val = Literal(self.value, datatype=self.datatype)
        add_to_global_graph(self.idnode, _RIF.value, val, g)

class ObjConstLiteralPlainLiteral(ObjConstLiteral):
    value: str
    lang: str
    datatype: URIRef = RDF.PlainLiteral
    def __init__(self, value: str, lang: Optional[str] = None):
        super().__init__()
        self.value = value
        self.lang = "" if lang is None else lang

    def as_node(self):
        return Literal(self.value, lang=self.lang)

    @classmethod
    def parse_rifpl(cls, **kwargs):
        raise NotImplementedError("use ObjConstLiteral instead")

    def add_to_global_graph(self, g: Optional[Graph] = None):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Const, g)
        val = Literal(self.value, lang=self.lang)
        add_to_global_graph(self.idnode, _RIF.value, val, g)

class ObjConstLiteralDatatype(ObjConstLiteral):
    """
    :TODO: value will be a Literal, but it should be a str.
    """
    value: str
    datatype: URIRef
    def __init__(self, value: str, datatype: str):
        super().__init__()
        self.value = value
        self.datatype = URIRef(datatype)

    def as_node(self):
        if self.datatype == XSD.boolean:
            return Literal(str(self.value), datatype =XSD.boolean)
        else:
            return Literal(self.value, datatype=self.datatype)

    @classmethod
    def parse_rifpl(cls, **kwargs):
        raise NotImplementedError("use ObjConstLiteral instead")

    __builtin_types = [
            XSD.base64Binary, XSD.hexBinary,
            XSD.integer, XSD.anyURI, XSD.decimal,
            XSD.boolean,
            XSD.language, URIRef(str(XSD)+"lang"), XSD.Name, XSD.NCName, XSD.NMTOKEN,
            RDF.XMLLiteral,
            ]
    def add_to_global_graph(self, g: Optional[Graph] = None):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Const, g)
        val = self.as_node()
        add_to_global_graph(self.idnode, _RIF.value, val, g)


class ObjConstIRI(ObjConstLiteral):
    constIRI: str
    def __init__(self, constIRI: str):
        super().__init__()
        self.constIRI = constIRI

    def as_node(self):
        return URIRef(self.constIRI)

    @classmethod
    def parse_rifpl(cls, constIRI: List[URIRef]):
        if isinstance(constIRI, list):
            constIRI, = constIRI
        return cls(constIRI)

    def add_to_global_graph(self, g: Optional[Graph] = None):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Const, g)
        value = Literal(self.constIRI, datatype=_XSD.anyURI)
        add_to_global_graph(self.idnode, _RIF.constIRI, value, g)

class ObjConstBlankNode(ObjConst):
    constname: str
    def __init__(self, constname: str):
        super().__init__()
        self.constname = constname

    def as_node(self):
        global localiriToNode
        return localiriToNode.setdefault(self.constname, BNode())

    @classmethod
    def parse_rifpl(cls, constname: str):
        return cls(constname)

    def add_to_global_graph(self, g: Optional[Graph] = None):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Const, g)
        name = Literal(self.constname)
        add_to_global_graph(self.idnode, _RIF.constname, name, g)

class ObjConstLocal(ObjConst):
    constname: str
    def __init__(self, constname: str):
        super().__init__()
        self.constname = constname

    def as_node(self):
        raise NotImplementedError()
        #global localiriToNode
        #return localiriToNode.setdefault(self.constname, BNode())

    @classmethod
    def parse_rifpl(cls, constname: str):
        raise NotImplementedError("use ObjConstLiteral instead")

    def add_to_global_graph(self, g: Optional[Graph] = None):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Const, g)
        name = Literal(self.constname)
        add_to_global_graph(self.idnode, _RIF.constname, name, g)


class ObjNew(PyParse_Parser, RDFSObject):
    @classmethod
    def parse_rifpl(cls):
        return cls()

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.New, g)


class ObjFrame(PyParse_Parser, RDFSObject):
    object_: ObjConst
    slots: List[ObjSlot]
    def __init__(self, object_, slots: Iterable[Node]):
        super().__init__()
        self.object_ = object_
        self.slots = list(slots)

    def to_triples(self) -> Iterable[Tuple[Node, Node, Node]]:
        subj = self.object_.as_node()
        for slt in self.slots:
            pred = slt.slotkey.as_node()
            obj = slt.slotvalue.as_node()
            yield (subj, pred, obj)

    @classmethod
    def parse_rifpl(cls, object_, slots: List[Node]):
        return cls(object_, slots)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Frame, g)
        add_to_global_graph(self.idnode, _RIF.object, self.object_, g)
        add_collection_to_global_graph(self.idnode, _RIF.slots, self.slots, g)

class ObjAndFormula(PyParse_Parser, RDFSObject):
    formulas: List[Node]
    def __init__(self, formulas: Iterable[Node]):
        super().__init__()
        self.formulas = list(formulas)

    @classmethod
    def parse_rifpl(cls, formulas):
        return cls(formulas)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.And, g)
        add_collection_to_global_graph(self.idnode, _RIF.formulas, self.formulas, g)

class ObjExternal(PyParse_Parser, RDFSObject):
    content: Node
    def __init__(self, content):
        super().__init__()
        self.content = content

    @classmethod
    def parse_rifpl(cls, content):
        return cls(content)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.External, g)
        add_to_global_graph(self.idnode, _RIF.content, self.content, g)

class ObjExists(PyParse_Parser, RDFSObject):
    def __init__(self, vars_, formula):
        super().__init__()
        self.vars_ = list(vars_)
        self.formula = formula

    @classmethod
    def parse_rifpl(cls, vars_, formula):
        return cls(vars_, formula)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Exists, g)
        add_to_global_graph(self.idnode, _RIF.formula, self.formula, g)
        add_collection_to_global_graph(self.idnode, _RIF.vars, self.vars_, g)

class _ObjAction(PyParse_Parser, RDFSObject):
    def __init__(self, target):
        super().__init__()
        self.target = target

    @classmethod
    def parse_rifpl(cls, target):
        return cls(target)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, _RIF.target, self.target, g)

class _ObjRetractSlot(PyParse_Parser, RDFSObject):
    def __init__(self, first, second):
        super().__init__()
        self.first = first
        self.second = second

    @classmethod
    def parse_rifpl(cls, first, second):
        return cls(first, second)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        first = self.idnode
        second = BNode()
        add_to_global_graph(first, RDF.first, self.first, g)
        add_to_global_graph(first, RDF.rest, second, g)
        add_to_global_graph(second, RDF.first, self.second, g)
        add_to_global_graph(second, RDF.rest, RDF.nil, g)

class _ObjList(PyParse_Parser, RDFSObject):
    def __init__(self, items: List[RDFSObject],
                 rest: Optional[RDFSObject]):
        super().__init__()
        self.items = list(items)
        self.rest = rest

    @classmethod
    def parse_rifpl(cls, items: Optional[List[RDFSObject]] = [],
                    rest: Optional[RDFSObject] = None):
        items = [] if items is None else items
        return cls(items, rest)

    def add_to_global_graph(self, g: Graph):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.List, g)
        add_collection_to_global_graph(self.idnode, _RIF.items, self.items, g)
        if self.rest is not None:
            add_to_global_graph(self.idnode, _RIF.rest, self.rest, g)


class ObjRetract(_ObjAction):
    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Retract, g)

class ObjAssert(_ObjAction):
    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Assert, g)

class ObjModify(_ObjAction):
    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Modify, g)

class ObjExecute(_ObjAction):
    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Execute, g)

class ObjDo(PyParse_Parser, RDFSObject):
    def __init__(self, actionVar=[], actions=[]):
        super().__init__()
        self.actionVar = list(actionVar)
        self.actions = list(actions)

    @classmethod
    def parse_rifpl(cls, actionVar, actions):
        return cls(actionVar, actions)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Do, g)
        if len(self.actionVar) > 0:
            add_collection_to_global_graph(self.idnode, _RIF.actionVar, self.actionVar, g)
        add_collection_to_global_graph(self.idnode, _RIF.actions, self.actions, g)

class ObjAndAction(PyParse_Parser, RDFSObject):
    def __init__(self, actions=[]):
        super().__init__()
        self.actions = list(actions)

    @classmethod
    def parse_rifpl(cls, actions=[]):
        return cls(actions)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.And, g)
        add_collection_to_global_graph(self.idnode, _RIF.actions, self.actions, g)

class ObjNotFormula(PyParse_Parser, RDFSObject):
    def __init__(self, formula):
        super().__init__()
        self.formula = formula

    @classmethod
    def parse_rifpl(cls, formula):
        return cls(formula)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.INeg, g)
        add_to_global_graph(self.idnode, _RIF.formula, self.formula, g)

class ObjOrFormula(PyParse_Parser, RDFSObject):
    formulas: List[Node]
    def __init__(self, formulas: Iterable[Node]):
        super().__init__()
        self.formulas = list(formulas)

    @classmethod
    def parse_rifpl(cls, formulas):
        return cls(formulas)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Or, g)
        add_collection_to_global_graph(self.idnode, _RIF.formulas, self.formulas, g)

class ObjVar(PyParse_Parser, RDFSObject):
    text: str
    def __init__(self, text: str):
        super().__init__()
        self.text = text

    @classmethod
    def parse_rifpl(cls, text: str):
        return cls(text)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Var, g)
        add_to_global_graph(self.idnode, _RIF.varname, Literal(self.text), g)

class ObjName(PyParse_Parser, RDFSObject):
    text: str
    def __init__(self, text: str):
        super().__init__()
        self.text = text

    @classmethod
    def parse_rifpl(cls, text: str):
        return cls(text)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Name, g)
        add_to_global_graph(self.idnode, _RIF.name, Literal(self.text), g)

class ObjMember(PyParse_Parser, RDFSObject):
    def __init__(self, instance, class_):
        super().__init__()
        self.instance = instance
        self.class_ = class_

    @classmethod
    def parse_rifpl(cls, instance, class_):
        return cls(instance, class_)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Member, g)
        add_to_global_graph(self.idnode, _RIF["class"], self.class_, g)
        add_to_global_graph(self.idnode, _RIF.instance, self.instance, g)

class ObjSubclass(PyParse_Parser, RDFSObject):
    def __init__(self, sub, super_):
        super().__init__()
        self.sub = sub
        self.super_ = super_

    @classmethod
    def parse_rifpl(cls, sub, super_):
        return cls(sub, super_)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Subclass, g)
        add_to_global_graph(self.idnode, _RIF.sub, self.sub, g)
        add_to_global_graph(self.idnode, _RIF.super, self.super_, g)

class ObjEqual(PyParse_Parser, RDFSObject):
    def __init__(self, left, right):
        super().__init__()
        self.left = left
        self.right = right

    @classmethod
    def parse_rifpl(cls, left, right):
        return cls(left, right)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Equal, g)
        add_to_global_graph(self.idnode, _RIF.left, self.left, g)
        add_to_global_graph(self.idnode, _RIF.right, self.right, g)

class ObjAtom(PyParse_Parser, RDFSObject):
    @classmethod
    def parse_rifpl(cls, op, slots=None, args=None, **kwargs):
        if slots is None:
            return ObjAtomArgs(op, args)
        elif args is None:
            return ObjAtomSlots(op, slots)
        else:
            raise Exception(op, slots, args)

class ObjAtomSlots(ObjAtom):
    def __init__(self, op, slots):
        super().__init__()
        self.op = op
        self.slots = list(slots)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Atom, g)
        add_to_global_graph(self.idnode, _RIF.op, self.op, g)
        add_collection_to_global_graph(self.idnode, _RIF.slots, self.slots, g)


class ObjAtomArgs(ObjAtom):
    def __init__(self, op, args=[]):
        super().__init__()
        self.op = op
        self.args = list(args)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Atom, g)
        add_to_global_graph(self.idnode, _RIF.op, self.op,g )
        #TODO: This might be a problem from rif parser and this if is wrong
        if len(self.args) > 0:
            add_collection_to_global_graph(self.idnode, _RIF.args, self.args, g)


class ObjExpr(PyParse_Parser, RDFSObject):
    @classmethod
    def parse_rifpl(cls, op, slots=None, args=None, **kwargs):
        if slots is None:
            return ObjExprArgs(op, args)
        elif args is None:
            return ObjExprSlots(op, slots)
        else:
            raise Exception(op, slots, args)


class ObjExprSlots(ObjExpr):
    def __init__(self, op, slots):
        super().__init__()
        self.op = op
        self.slots = list(slots)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Expr, g)
        add_to_global_graph(self.idnode, _RIF.op, self.op, g)
        add_collection_to_global_graph(self.idnode, _RIF.slots, self.slots, g)

class ObjExprArgs(ObjExpr):
    def __init__(self, op, args=[]):
        super().__init__()
        self.op = op
        self.args = list(args)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Expr, g)
        add_to_global_graph(self.idnode, _RIF.op, self.op, g)
        add_collection_to_global_graph(self.idnode, _RIF.args, self.args, g)

@dataclass
class ObjPrefix(PyParse_Parser):
    shortcut: "str"
    iri: "str"
    old_iri: "Optional[URIRef]"

    @classmethod
    def parse_rifpl(cls, shortcut, iri):
        global global_graph
        old = None
        for x, y in global_graph.namespaces():
            if x == shortcut:
                old = y
                break
        self = cls(shortcut, iri, old)
        self.register()
        return self

    def register(self):
        global global_graph
        global_graph.namespace_manager.bind(self.shortcut, self.iri)

    def unregister(self):
        global global_graph
        if self.old_iri is not None:
            global_graph.namespace_manager.bind(self.shortcut, self.old_iri)


class _ObjImplies(PyParse_Parser, RDFSObject):
    def __init__(self, if_):
        super().__init__()
        self.if_ = if_

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Implies, g)
        add_to_global_graph(self.idnode, _RIF["if"], self.if_, g)


class ObjImpliesPRD(_ObjImplies):
    def __init__(self, if_, then: List[Node]):
        super().__init__(if_)
        self.then = then

    @classmethod
    def parse_rifpl(cls, if_: RDFSObject, then: RDFSObject):
        return cls(if_, then)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, _RIF.then, self.then, g)


class ObjImpliesCore(_ObjImplies):
    def __init__(self, if_, then: List[Node]):
        super().__init__(if_)
        self.if_ = if_
        self.then = then

    @classmethod
    def parse_rifpl(cls, if_: RDFSObject, then: List[RDFSObject]):
        return cls(if_, then)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        if len(self.then) == 1:
            add_to_global_graph(self.idnode, _RIF.then, self.then[0], g)
        else:
            add_collection_to_global_graph(self.idnode, _RIF.then, self.then, g)


class ObjForall(PyParse_Parser, RDFSObject):
    def __init__(self, vars_, formula, pattern = None):
        super().__init__()
        self.vars_ = list(vars_)
        self.pattern = pattern
        self.formula = formula

    @classmethod
    def parse_rifpl(cls, vars_: List[Node], formula: Node,
                    pattern: List[Node] = None):
        if pattern is not None:
            pattern, = pattern
        return cls(vars_, formula, pattern)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Forall, g)
        add_collection_to_global_graph(self.idnode, _RIF["vars"], self.vars_, g)
        if self.pattern is not None:
            add_to_global_graph(self.idnode, _RIF.pattern, self.pattern, g)
        add_to_global_graph(self.idnode, _RIF.formula, self.formula, g)

class ObjGroup(PyParse_Parser, RDFSObject):
    def __init__(self, sentences: List[Node]):
        super().__init__()
        self.sentences = list(sentences)

    @classmethod
    def parse_rifpl(cls, sentences: List[Node]):
        return cls(sentences)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Group, g)
        add_collection_to_global_graph(self.idnode, _RIF.sentences,
                                       self.sentences, g)

class ObjImport(PyParse_Parser, RDFSObject):
    def __init__(self, location: str, profile: Optional[str] = None):
        super().__init__()
        self.location = location
        self.profile = profile

    @classmethod
    def parse_rifpl(cls, location, profile=None):
        if profile is None:
            return cls(str(location))
        else:
            return cls(str(location), str(profile))

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Import, g)
        add_to_global_graph(self.idnode, _RIF.location, Literal(self.location), g)
        if self.profile is not None:
            add_to_global_graph(self.idnode, _RIF.profile, Literal(self.profile), g)

class ObjDocument(PyParse_Parser, RDFSObject):
    def __init__(self, payload: Node, directive):
        super().__init__()
        self.payload = payload
        self.directive = list(directive)

    @classmethod
    def parse_rifpl(cls, payload=None, Prefixes: List["ObjPrefix"] = [],
              directive=[]):
        for pfx in Prefixes:
            pfx.unregister()
        return cls(payload, directive)

    def add_to_global_graph(self, g):
        super().add_to_global_graph(g)
        add_to_global_graph(self.idnode, RDF.type, _RIF.Document, g)
        if self.payload is not None:
            add_to_global_graph(self.idnode, _RIF.payload, self.payload, g)
        if (len(self.directive) > 0):
            add_collection_to_global_graph(self.idnode, _RIF.directives, self.directive, g)
