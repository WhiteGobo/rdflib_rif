from urllib.parse import urldefrag, urljoin
import urllib.parse
import xml.sax
import xml.sax.handler
from xml.sax import handler, make_parser, xmlreader
from xml.sax.handler import ErrorHandler
from xml.sax.saxutils import escape, quoteattr
    
import rdflib.parser
from rdflib.exceptions import Error, ParserError
from rdflib.namespace import is_ncname
from rdflib.plugins.parsers.RDFVOC import RDFVOC
from rdflib.term import BNode, Literal, URIRef

import typing as typ
import abc
import rdflib
import re

import rdflib

_RDF = rdflib.Namespace(rdflib.RDF)
_XSDNS = rdflib.Namespace("http://www.w3.org/2001/XMLSchema#")
_XML = rdflib.Namespace("http://www.w3.org/XML/1998/namespace")
class _XMLNS:
    xs = "http://www.w3.org/2001/XMLSchema"
    xml = "http://www.w3.org/XML/1998/namespacebase"

class _RIF:
    _base = rdflib.URIRef("http://www.w3.org/2007/rif#")
    Document = _base+"Document"
    payload = _base+"payload"
    Group = _base+"Group"
    id = _base+"id"
    Const = _base+"Const"
    meta = _base+"meta"
    directive = _base + "directive"
    Exists = _base + "Exists"
    directives = _base + "directives"
    """Term only in rdf/rif. See :term:`table 3`"""
    vars = _base + "vars"
    """Term only in rdf/rif. See :term:`table 3`"""
    sentences = _base + "sentences"
    """Term only in rdf/rif. See :term:`table 3`"""
    formulas = _base + "formulas"
    """Term only in rdf/rif. See :term:`table 3`"""
    sentence = _base + "sentence"
    Forall = _base + "Forall"
    Then = _base + "Then"
    declare = _base + "declare"
    Var = _base + "Var"
    Frame = _base + "Frame"
    formula = _base + "formula"
    Implies = _base + "Implies"
    if_ = _base + "if"
    then = _base + "then"
    And = _base + "And"
    Or = _base + "Or"
    Atom = _base + "Atom"
    op = _base + "op"
    args = _base + "args"
    Equal = _base + "Equal"
    left = _base + "left"
    right = _base + "right"
    External = _base + "External"
    content = _base + "content"
    Expr = _base + "Expr"
    varname = _base + "varname"
    constIRI = _base + "constIRI"
    constname = _base + "constname"
    value = _base + "value"
    type = _base + "type"
    iri = _base + "iri"
    local = _base + "local"
    slot = _base + "slot"
    """Term only in xml/rif. See :term:`table 3`"""
    Slot = _base + "Slot"
    """Term only in rdf/rif. See :term:`table 3`"""
    slots = _base + "slots"
    """Term only in rdf/rif. See :term:`table 3`"""
    slotkey = _base + "slotkey"
    """Term only in rdf/rif. See :term:`table 3`"""
    slotvalue = _base + "slotvalue"
    """Term only in rdf/rif. See :term:`table 3`"""

    @classmethod
    def __str__(cls):
        return cls._base

class _createnode_mixin:
    def create_nodestate(self, trans, attrs):
        kwargs = {"parentnode":self, "attrs":attrs, "typeof":trans}
        if str(trans) == str(_RIF.Var):
            nextstate = _Var(**kwargs)
        elif str(trans) == str(_RIF.Const):
            nextstate = _Const(**kwargs)
        elif str(trans) == str(_RIF.Document):
            nextstate = _Document(**kwargs)
        else:
            nextstate = _default_node(**kwargs)
        return nextstate

class _state(abc.ABC):
    """State of handler"""
    parentnode: typ.Any
    _buffer: list
    def __init__(self, parentnode, attrs):
        self.parentnode = parentnode
        self._buffer = []
        self.attrs = dict(attrs)
        try:
            self.namespace_base\
                    = self.attrs['http://www.w3.org/XML/1998/namespace',
                                 'base']
        except KeyError:
            self.namespace_base = parentnode.namespace_base

    @property
    def content(self):
        return "".join(self._buffer)

    def characters(self, content):
        """Use this to deciver content"""
        self._buffer.append(content)

    def close(self):
        return self.parentnode

    @abc.abstractmethod
    def transition(self, trans: str, attrs: typ.Mapping) -> "_state":
        ...


class _state_with_axioms(_state):
    """
    :TODO: change append_axiom because this doesnt work with how 
        properties of class 2 work.
    """
    def __init__(self, parentnode, attrs):
        super().__init__(parentnode=parentnode, attrs=attrs)
        self._axioms = rdflib.Graph()

    def append_axiom(self, ax):
        #self._axioms.append(ax)
        self._axioms.add(ax)

    def close(self):
        for ax in self._axioms:
            self.parentnode.append_axiom(ax)
        return super().close()


class _start(_state_with_axioms, _createnode_mixin):
    """Startstate"""
    def __init__(self):
        super().__init__(\
                parentnode = None,\
                attrs = {('http://www.w3.org/XML/1998/namespace', 'base'):""},\
                )

    def close(self):
        return _end(self._axioms)

    def transition(self, trans: str, attrs: typ.Mapping) -> "_state":
        return self.create_nodestate(trans=trans, attrs=attrs)


class _end:
    def __init__(self, axioms):
        self.axioms = axioms

    def transition(self, *args, **kwargs):
        raise Exception()



class _default_node(_state_with_axioms):
    _subend = re.compile("\s*$")
    _substart = re.compile("^\s*")
    slot: typ.List[rdflib.IdentifiedNode]
    """Focusnodes of slots as specified with :term:`rif:slot`"""
    def __init__(self, typeof, **kwargs):
        super().__init__(**kwargs)
        self.typeof = rdflib.URIRef(typeof)
        self._properties = []
        self.id = None
        self.slots = []

    def close(self):
        tmp = self._subend.sub("", self.content)
        tmp = self._substart.sub("", tmp)
        if self.id is None:
            self.id = rdflib.BNode()
        for prop, obj in self._properties:
            self.parentnode.append_axiom((self.id, prop, obj))
        self.parentnode.append_axiom((self.id, _RDF.type, self.typeof))
        if self.slots:
            tmpg = rdflib.Graph()
            slots_id = rdflib.BNode()
            slots = rdflib.collection.Collection(tmpg, slots_id)
            for slot in self.slots:
                slots.append(slot)
            for ax in tmpg:
                self.parentnode.append_axiom(ax)
            self.parentnode.append_axiom((self.id, rdflib.URIRef(_RIF.slots),
                                          slots_id))
        return super().close()

    def append_property(self, prop, obj):
        self._properties.append((prop, obj))

    def transition(self, trans: str, attrs: typ.Mapping) -> "_state":
        if str(trans) == "http://www.w3.org/2007/rif#id":
            return _id(parentnode=self,
                       attrs=attrs)
        else:
            return _default_property(parentnode=self,
                                     attrs=attrs,
                                     typeof=trans)

class _Var(_default_node):
    def close(self):
        q = super().close()
        pred = rdflib.URIRef(_RIF.varname)
        value = rdflib.Literal(self.content)
        self.parentnode.append_axiom((self.id, pred, value))
        return q

class _Const(_default_node):
    """
    :TODO: work over lang and datatype when, creating a literal
    """
    def close(self):
        """
        :TODO: const_type should understand shortened IRIs as well
        """
        q = super().close()
        const_type = self.attrs[None, 'type']
        if str(const_type) in (str(_RIF.iri), "rif:iri"):
            pred = rdflib.URIRef(_RIF.constIRI)
            parts = urllib.parse.urlsplit(self.content)
            if parts.scheme:
                content = self.content
            else:
                content = self.namespace_base + self.content
            value = rdflib.Literal(content, datatype=_XSDNS.anyURI )
        elif str(const_type) in (str(_RIF.local), "rif:local"):
            pred = rdflib.URIRef(_RIF.constname)
            value = rdflib.Literal(self.content, datatype=_XSDNS.string)
        elif str(const_type) == str(_RDF.PlainLiteral):
            pred = rdflib.URIRef(_RIF.value)
            value = rdflib.Literal(self.content)
        else:
            pred = rdflib.URIRef(_RIF.value)
            value = rdflib.Literal(self.content, datatype=const_type)
        self.parentnode.append_axiom((self.id, pred, value))
        return q

class _default_property(_state_with_axioms, _createnode_mixin):
    def __init__(self, typeof, parentnode, attrs):
        super().__init__(parentnode=parentnode, attrs=attrs)
        self.typeof = rdflib.URIRef(typeof)
        self._targets = []
        try:
            self.ordered = (self.attrs[None, "ordered"].lower()[0] == "y")
        except KeyError:
            self.ordered = False

    def transition(self, trans, attrs):
        nextstate = super().transition(trans, attrs)
        self._targets.append(nextstate)
        return nextstate

    def close(self, property_type: int = None):
        """
        :param property_type: See :term:`table 3` and property thingies.
            Can be overwritten by subclass.
        """
        p, s = self.parentnode.typeof, self.typeof
        if (p, s) == (_RIF.Document, _RIF.directive):
            prop_type = _RIF.directives
            property_type = 2
        elif (p, s) == (_RIF.Group, _RIF.sentence):
            prop_type = _RIF.sentences
            property_type = 2
        elif p in (_RIF.Forall, _RIF.Exists) and s == _RIF.declare:
            prop_type = _RIF.vars
            property_type = 2
        elif p in (_RIF.And, _RIF.Or) and s == _RIF.formula:
            prop_type = _RIF.formulas
            property_type = 2
        elif self.parentnode.typeof == _RIF.Frame\
                and self.typeof == _RIF.slot:
            prop_type = _RIF.slots
            property_type = 3
        elif self.parentnode.typeof in (_RIF.Atom, _RIF.Expr)\
                and self.typeof == _RIF.slot:
            prop_type = _RIF.namedargs
            property_type = 3
        elif self.ordered:
            prop_type = self.typeof
            property_type = 2
        else:
            prop_type = self.typeof
            property_type = 1
        if property_type == 2:
            #TODO _properties should not be used from here
            try:
                _, queue_id = filter(lambda prop_obj: prop_obj[0] == prop_type,
                                     self.parentnode._properties).__next__()
            except StopIteration:
                queue_id = rdflib.BNode()
            tmpg = self.parentnode._axioms
            queue = rdflib.collection.Collection(tmpg, queue_id)
            for obj in self._targets:
                queue.append(obj.id)
            #for ax in tmpg:
            #    self.parentnode.append_axiom((ax))
            self.parentnode.append_property(prop_type, queue_id)
        elif property_type == 3:
            slot_id = rdflib.BNode()
            self.parentnode.slots.append(slot_id)
            key, value = self._targets
            self.parentnode.append_axiom((slot_id, _RDF.type, _RIF.Slot))
            self.parentnode.append_axiom((slot_id, _RIF.slotkey, key.id))
            self.parentnode.append_axiom((slot_id, _RIF.slotvalue, value.id))
        else:
            for obj in self._targets:
                self.parentnode.append_property(prop_type, obj.id)
        return super().close()

    def transition(self, trans: str, attrs: typ.Mapping) -> "_state":
        nextstate = self.create_nodestate(trans=trans, attrs=attrs)
        self._targets.append(nextstate)
        return nextstate

class _id(_state, _createnode_mixin):
    target: typ.Any
    def append_axiom(self, *args, **kwargs):
        pass

    def transition(self, trans, attrs):
        self.target = self.create_nodestate(trans=trans, attrs=attrs)
        return self.target

    def close(self):
        self.parentnode.id = rdflib.URIRef(self.target.content)
        return super().close()


_table_3 = {
    ("Document", "directive") : ("directives", 2),
    ("Group", "sentence") : ("sentences", 2),
    ("Forall", "declare") : ("vars", 2),
    ("Exists", "declare") : ("vars", 2),
    ("And", "formula") : ("formulas", 2),
    ("Or", "formula") : ("formulas", 2),
    ("Frame", "slot") : ("slots", 3),
    ("Atom", "slot") : ("namedargs", 3),
    ("Expr", "slot") : ("namedargs", 3),
}

class _Document(_default_node):
    """directives are missing"""
    def __init__(self, typeof, **kwargs):
        super().__init__(typeof, **kwargs)
        self.directives = []
    def close(self):
        previousstate = super().close()
        
        if self.directives:
            tmpg = rdflib.Graph()
            directives_id = rdflib.BNode()
            directives = rdflib.collection.Collection(tmpg, directives_id)
            for directive in self.directives:
                directives.append(directive.id)
            for ax in tmpg:
                self.parentnode.append_axiom(ax)
            self.parentnode.append_axiom((self.id,
                                          rdflib.URIRef(_RIF.directives),
                                          directives_id))
        else:
            #This is because of a bug in rdflib.collection.Collection
            #self.parentnode.append_axiom((self.id,
            #                              rdflib.URIRef(_RIF.directives),
            #                              _RDF.nil))
            pass

        return previousstate


class RIFXMLHandler(xml.sax.handler.ContentHandler):
    startingstate = _start
    def __init__(self, store):
        self.store = store
        self.preserve_bnode_ids = False
        self.reset()

    def reset(self):
        pass

    def setDocumentLocator(self, locator):
        self.locator = locator

    def startDocument(self):
        self.state = self.startingstate()
        
    def parse(self, *args):
        raise Exception()

    #def feed(self, buffer):
    #    raise Exception(buffer)
    #    super().feed()

    def endDocument(self):
        endstate = self.state.close()
        for ax in endstate.axioms:
            self.store.add(ax)

    def startElement(self, name, attrs):
        raise NotImplementedError("Please set xml.sax.handler"
                                  ".feature_namespaces\n Add something like"
                                  "parser.setFeature(xml.sax.handler"
                                  ".feature_namespaces, 1")

    def endElement(self, name, qname):
        raise NotImplementedError()

    def startElementNS(self, name, qname, attrs):
        transsymbol = "".join(name)
        assert qname is None, "not implemented because dont know"
        self.state = self.state.transition(transsymbol, attrs)

    def endElementNS(self, name, qname):
        """We assume, that the RIF form is sound, so there is no checking
        if the correct thing was closed.
        """
        self.state = self.state.close()

    def characters(self, content):
        self.state.characters(content)

    def ignorableWhitespace(self, content):
        pass

