"""This module supplies all the classes, that are equivalent to classes 
in rdf/rif or in xml/rif.
"""
import abc
import rdflib
import xml.etree.ElementTree as ET

from .container_classes import MetaContainer, rif_element, TextContainer, prefix_transporter, MissingPrefix

_RIF = rdflib.Namespace("http://www.w3.org/2007/rif#")
_XSD = rdflib.Namespace("http://www.w3.org/2001/XMLSchema#")

class Document(rif_element, MetaContainer):
    type_suffix = "Document"
    attr_to_suffix = {
            "Base": "Base",
            "directive": "directive",
            "payload": "payload",
            }
    #xml.etree.ElementTree.register_namespace(prefix, uri)
    def as_xml(self, *args, **kwargs):
        root = super().as_xml(*args, **kwargs)
        #root.attrib["xmlns"] = str(_RIF)
        return root

class Group(rif_element, MetaContainer):
    """
    :TODO: Put priority and strategy into an extra subelement named behavior
    """
    type_suffix = "Group"
    attr_to_suffix = {
            "sentence": "sentence",
            "Priority": "behaviorPriority",
            "Strategy": "behaviorStrategy",
            }


class Forall(rif_element, MetaContainer):
    type_suffix = "Forall"
    attr_to_suffix = {
            "declare": "declare",
            "pattern": "pattern",
            "formula": "formula",
            }


class Exists(rif_element, MetaContainer):
    type_suffix = "Exists"
    attr_to_suffix = {
            "Vars": "declare",
            "Formulas": "formula",
            }



class Implies(rif_element, MetaContainer):
    type_suffix = "Implies"
    attr_to_suffix = {
            "Formula": "if",
            "Actionblock": "then",
            }


class And_formula(rif_element):
    type_suffix = "And"
    attr_to_suffix = {
            "Formulas": "formula",
            }
class Or_formula(rif_element):
    type_suffix = "Or"
    attr_to_suffix = {
            "Formulas": "formula",
            }

class Member(rif_element, MetaContainer):
    type_suffix = "Member"
    attr_to_suffix = {
            "instance": "instance",
            "class": "class",
            }

class Subclass(rif_element, MetaContainer):
    type_suffix = "Subclass"
    attr_to_suffix = {
            "sub": "sub",
            "super": "super",
            }


class Frame(rif_element, MetaContainer):
    type_suffix = "Frame"
    attr_to_suffix = {
            "object": "object",
            "slot": ""
            }

class Import(rif_element, MetaContainer):
    type_suffix = "Import"
    attr_to_suffix = {}
    def __init__(self, Location, Profile=None, **kwargs):
        super().__init__(**kwargs)
        self.Location = Location
        self.Profile = Profile

    def as_xml(self, **kwargs):
        root = super().as_xml(**kwargs)
        location_n = ET.SubElement(root, "location")
        location_n.text = str(self.Location)
        if self.Profile is not None:
            profile_n = ET.SubElement(root, "profile")
            profile_n.text = str(self.Profile)
        return root


class Slot(rif_element):
    type_suffix = "slot"
    attr_to_suffix = {}
    def __init__(self, first, second, **kwargs):
        super().__init__(**kwargs)
        self.first = first
        self.second = second

    def __repr__(self):
        name = type(self).__name__
        return f"<{name}:{self.first}:{self.second}>"

    @classmethod
    def _parse(cls, ParserResults):
        first = ParserResults[0]
        second = ParserResults[1]
        return cls(first, second)

    def as_xml(self, **kwargs):
        """
        :TODO: im not sure why i have to use transport_prefixes here.
            theoreticly the definition of __iter__ should be sufficient
        """
        root = super().as_xml(**kwargs)
        root.attrib["ordered"] = "yes"
        self.first._transport_prefix(self._prefixes)
        self.second._transport_prefix(self._prefixes)
        self.first.as_xml(parent=root, update_prefixes=False)
        self.second.as_xml(parent=root, update_prefixes=False)
        return root

    def __iter__(self):
        for x in super().__iter__():
            yield x
        yield first
        yield second


class Const(rif_element, abc.ABC):
    type_suffix = "Const"
    attr_to_suffix = {}

    @property
    @abc.abstractmethod
    def value(self):
        ...

    @property
    @abc.abstractmethod
    def consttype(self):
        ...

    def as_xml(self, **kwargs):
        root = super().as_xml(**kwargs)
        root.text = self.value
        root.attrib["type"] = self.consttype
        return root

    def __repr__(self):
        name = type(self).__name__
        return f"<{name}:{self.iri}>"


class Const_local(Const):
    consttype = str(_RIF.local)
    def __init__(self, value):
        super().__init__()
        self.value = value

    @property
    def value(self):
        ...


class literal(Const):
    """
    :TODO: Here are some problems, because of self.datatype. Also there is 
        a possibilty to return rif:local as type but i dont know how it is
        implemented.
    """
    def __init__(self, literal, datatype=_XSD.string, lang=None):
        super().__init__()
        #assert datatype is not None
        self.literal = literal
        self.datatype = datatype
        self.lang = lang

    def __repr__(self):
        name = type(self).__name__
        return f"<{name}:{self.literal}^^{self.datatype}@{self.lang}>"

    @classmethod
    def _parse(cls, ParserResults):
        myliteral = ParserResults[0]
        try:
            string = myliteral.string
        except AttributeError:
            string = str(myliteral.value)
        extraargs = {}
        datatype = myliteral.datatype or _XSD.string
        if datatype is not None:
            if "prefix" in datatype:
                datatype = Const_shortenediri(datatype["prefix"], datatype["localname"])
        lang = getattr(myliteral, "lang", None)
        if lang is not None:
            extraargs["lang"] = lang
        return cls(string, datatype, **extraargs)

    def _transport_prefix(self, *args, **kwargs):
        super()._transport_prefix(*args, **kwargs)
        try:
            self.datatype._transport_prefix(self._prefixes)
        except AttributeError:
            pass
        try:
            self.lang._transport_prefix(self._prefixes)
        except AttributeError:
            pass

    @property
    def value(self):
        return self.literal

    @property
    def consttype(self):
        """
        :TODO: im not sure, where i got that RIF.local from. Please 
            look into this. Currently self.datatype cant be None
        """
        self._transport_prefix()
        if self.datatype is None:
            #return str(_XSD.string)
            return str(_RIF.local)
        else:
            if isinstance(self.datatype, str):
                return self.datatype
            else:
                return self.datatype.iri

    def as_xml(self, **kwargs):
        """
        Setting currently RDF.PlainLiteral as value if lang is given
        :TODO: Check difference RDF.PlainLiteral and xsd.string.
            Because xsd.string is standard for literals for rdflib
            and in rif-test files rdf.plainliteral is used
        """
        root = super().as_xml(**kwargs)
        if self.lang is not None:
            root.text = "@".join((root.text, self.lang))
            root.attrib["type"] = rdflib.RDF.PlainLiteral
        return root


class Const_withlang(Const):
    def __init__(self, value=None, lang=None, **kwargs):
        super().__init__()
        raise Exception(value, lang)
        self.value = value
        self.consttype = lang

    @property
    def value(self):
        ...

    @property
    def consttype(self):
        ...

class Const_longiri(Const):
    consttype = str(_RIF.iri)
    def __init__(self, iri):
        super().__init__()
        self.iri = iri
        
    @property
    def value(self):
        return self.iri

class Const_shortenediri(Const):
    """
    Dont use textcontainer because text will be created automaticly from
    prefix and suffix.
    """
    consttype = str(_RIF.iri)
    def __init__(self, prefix, suffix):
        super().__init__()
        self.prefix = prefix
        self.suffix = suffix

    @property
    def value(self):
        return self.iri

    @property
    def iri(self):
        try:
            pre = self._prefixes[self.prefix]
        except KeyError as err:
            raise MissingPrefix("Either transport prefixes wasnt used yet, "
                                f"or missing prefix '{self.prefix}': \n%s\n%s"
                                %(self, self._prefixes)) from err
        return "".join((pre, self.suffix))

    @classmethod
    def _parse(cls, ParserResults):
        """
        :TODO: hotfix for if iri is uriref
        """
        iri = ParserResults[0]
        if isinstance(iri, rdflib.URIRef):
            return Const_longiri(iri)
        return cls(iri.prefix, iri.localname)

    def __repr__(self):
        name = type(self).__name__
        return f"<{name}:{self.prefix}:{self.suffix}>"


class External_Const_longiri(Const_longiri):
    consttype = str(_RIF.iri)
    def as_xml(self, *args, **kwargs):
        root = super().as_xml(*args, **kwargs)
        root.attrib["type"] = self.consttype
        return root

class External_Const_shortenediri(Const_shortenediri):
    consttype = str(_RIF.iri)
    @classmethod
    def _parse(cls, ParserResults):
        """
        :TODO: hotfix for if iri is uriref
        """
        iri = ParserResults[0]
        if isinstance(iri, rdflib.URIRef):
            return External_Const_longiri(iri)
        return cls(iri.prefix, iri.localname)

    def as_xml(self, *args, **kwargs):
        root = super().as_xml(*args, **kwargs)
        root.attrib["type"] = self.consttype
        return root

class Var(rif_element, TextContainer, MetaContainer):
    type_suffix = "Var"
    attr_to_suffix = {}
    def __repr__(self):
        name = type(self).__name__
        return f"<{name}:{self.text}>"

class LocalIri(rif_element, TextContainer):
    type_suffix = "Const"
    attr_to_suffix = {}
    def __repr__(self):
        name = type(self).__name__
        return f"<{name}:{self.text}>"
    def as_xml(self, **kwargs):
        root = super().as_xml(**kwargs)
        root.attrib["type"] = _RIF.local
        return root

class External_formula(rif_element, MetaContainer):
    type_suffix = "External"
    attr_to_suffix = {
            "Content": "content",
            }

    @classmethod
    def _parse(cls, parseresults):
        """
        :TODO: Cant make expr using result_name Content.
        """
        Content, = parseresults
        return cls(Content = Content)


class Uniterm(rif_element, MetaContainer):
    attr_to_suffix = {
            "Op": "op",
            "Args": "args",
            }
    attr_is_list = ["Args"]

class Atom(Uniterm):
    type_suffix = "Atom"

class Expr(Uniterm):
    type_suffix = "Expr"

class Do_action(rif_element, MetaContainer):
    type_suffix = "Do"
    attr_to_suffix = {
            "Vars": "",
            "Actions": "actions",
            }
    attr_is_list = ["Actions"]

class Modify(rif_element):
    type_suffix = "Modify"
    attr_to_suffix = {
            "Target": "target",
            }

class Retract(rif_element, MetaContainer):
    type_suffix = "Retract"
    attr_to_suffix = {
            "Target": "target",
            }
    attr_is_list = ["Target"]

    def as_xml(self, *args, **kwargs):
        root = super().as_xml(*args, **kwargs)
        targets = root.find("target")
        if len(targets) == 1:
            targets.attrib.pop("ordered")
        else:
            targets.attrib["ordered"] = "yes"
        return root

class Execute(rif_element, MetaContainer):
    type_suffix = "Execute"
    attr_to_suffix = {
            "Target": "target",
            }

class Assert(rif_element, MetaContainer):
    type_suffix = "Assert"
    attr_to_suffix = {
            "Target": "target",
            }
    @classmethod
    def _parse(cls, ParserResults):
        """
        :TODO: set_results_name for targets didnt work
        """
        Target, = ParserResults
        return cls(Target=Target)

class New(prefix_transporter):
    type_suffix = "New"

    def as_xml(self, parent, **kwargs):
        root = ET.SubElement(parent, self.type_suffix)
        return root

    def __iter__(self):
        return iter([])

class Var_init_slot(rif_element):
    type_suffix = "actionVar"
    def __init__(self, first, second, **kwargs):
        super().__init__(**kwargs)
        self.first = first
        self.second = second

    @classmethod
    def _parse(cls, parseresults):
        first, second = parseresults
        return cls(first, second)

    #def as_xml(self, parent, **kwargs):
    #    self.first._transport_prefix(self._prefixes)
    #    self.second._transport_prefix(self._prefixes)
    #    self.first.as_xml(parent=parent, update_prefixes=False)
    #    self.second.as_xml(parent=parent, update_prefixes=False)
    def as_xml(self, **kwargs):
        """
        :TODO: im not sure why i have to use transport_prefixes here.
            theoreticly the definition of __iter__ should be sufficient
        """
        root = super().as_xml(**kwargs)
        root.attrib["ordered"] = "yes"
        self.first._transport_prefix(self._prefixes)
        self.second._transport_prefix(self._prefixes)
        self.first.as_xml(parent=root, update_prefixes=False)
        self.second.as_xml(parent=root, update_prefixes=False)
        return root

    def __iter__(self):
        yield self.first
        yield self.second

    def __repr__(self):
        name = type(self).__name__
        return f"<{name}:{self.first}:{self.second}>"

class negatedformula(rif_element, MetaContainer):
    type_suffix = "INeg"
    attr_to_suffix = {
            "Formula": "formula",
            }

class Equal(rif_element, MetaContainer):
    type_suffix = "Equal"
    attr_to_suffix = {
            "Left": "left",
            "Right": "right",
            }

class List(rif_element, MetaContainer):
    type_suffix = "List"
    attr_to_suffix = {
            "Items": "items",
            }
    attr_is_list = ["Items"]

def notImpl(*args, **kwargs):
    raise NotImplementedError(*args)
