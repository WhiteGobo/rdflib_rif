"""Baseclasses for parsing from rifpl to rif(xml)
"""
import abc
import typing as typ
import rdflib
import xml.etree.ElementTree as ET
import logging
logger = logging.getLogger(__name__)

_RIF = rdflib.Namespace("http://www.w3.org/2007/rif#")
_RIF_builtin = rdflib.Namespace("http://www.w3.org/2007/rif-builtin-action#")
_RIF_pred = rdflib.Namespace("http://www.w3.org/2007/rif-builtin-predicate#")
_RIF_func = rdflib.Namespace("http://www.w3.org/2007/rif-builtin-function#")
_XS = rdflib.Namespace("http://www.w3.org/2001/XMLSchema#")

STANDARDPREFIXES = {"act": _RIF_builtin,
                    "pred": _RIF_pred,
                    "func": _RIF_func,
                    "rif": _RIF,
                    "rdf": str(rdflib.RDF),
                    "rdfs": str(rdflib.RDFS),
                    "xs": _XS,
                    }
"""These prefixes are standardly known as prefixes. So you dont need to 
specify in a Document.

This is because of the :term:`official testcases`.
"""


class MissingPrefix(SyntaxError):
    pass


class prefix_transporter(abc.ABC):
    """Layered design

    Note that "http://www.w3.org/2007/rif-builtin-action"_ is builtin as 
    default prefix.
    """
    _prefixes: typ.Dict[str, rdflib.URIRef]
    """Used prefixes"""
    def __init__(self, Prefixes = {}, **kwargs):
        super().__init__(**kwargs)
        self._prefixes = dict(STANDARDPREFIXES)
        self._prefixes.update(Prefixes)
        self.__working = False

    def _transport_prefix(self, prefixes: typ.Dict[str, rdflib.URIRef] = {}):
        """Transports prefixes successive through layers.

        :param successive: Determines if lower layers will also
            transport their prefixes further down
        """
        self._prefixes.update(prefixes)
        self.__working = True
        for x in self:
            try:
                x._transport_prefix(self._prefixes)
            except Exception:
                pass
        self.__working = False

    def __iter__(self) -> typ.Iterable["prefix_transporter"]:
        """Get each item of the layer below this"""
        ...

class _xml_root_generator:
    def as_xml(self, parent=None, update_prefixes=True):
        if update_prefixes:
            self._transport_prefix()
        if parent is None:
            root = ET.Element(self.type_suffix)
        else:
            root = ET.SubElement(parent, self.type_suffix)
        return root

    def __iter__(self):
        """Happens to fit here best. Needed for childs"""
        return []

class node(_xml_root_generator, abc.ABC):
    type_suffix: str
    """This is the type of a node within a rif document"""
    attr_to_suffix: typ.Dict[str, str] = {}
    """This is a mapping of given keyword attributes to the used 'iri' within
    a rif document.
    """
    attr_is_list: typ.Container[str] = []
    """See
    "https://www.w3.org/TR/2013/NOTE-rif-in-rdf-20130205/#General_Mapping"_
    for more information how subelements are ordered.
    """
    type_prefix = _RIF
    """Standard value for xmlns"""
    def __init__(self, **kwargs):
        for attr_name in self.attr_to_suffix:
            try:
                x = kwargs.pop(attr_name)
                setattr(self, attr_name, x)
            except KeyError:
                pass
        try:
            super().__init__(**kwargs)
        except TypeError as err:
            raise Exception(f"Left {kwargs} for class {type(self)}") from err

    def __iter__(self):
        for x in super().__iter__():
            yield x
        for attr_name in self.attr_to_suffix:
            try:
                q = getattr(self, attr_name)
            except AttributeError:
                continue
            if type(q) == list:
                for x in q:
                    yield x
            else:
                yield q

    def __attr_as_iterator(self, attr_name):
        try:
            x = getattr(self, attr_name)
        except AttributeError:
            return
        if type(x) == list:
            for q in x:
                yield q
        else:
            yield x

    def as_xml(self, current_base_prefix=None, **kwargs):
        root = super().as_xml(**kwargs)
        if current_base_prefix is None:
            root.attrib["xmlns"] = self.type_prefix
            current_base_prefix = self.type_prefix
        extraargs = {"current_base_prefix": current_base_prefix,
                     "update_prefixes": False}
        try:
            for attr_name, property_name in self.attr_to_suffix.items():
                if attr_name in self.attr_is_list:
                    prop = ET.Element(property_name)
                    prop.attrib["ordered"] = "yes"
                    for x in self.__attr_as_iterator(attr_name):
                        x.as_xml(parent=prop, **extraargs)
                    if len(prop) > 0:
                        root.append(prop)
                else:
                    for x in self.__attr_as_iterator(attr_name):
                        if property_name:
                            prop = ET.SubElement(root, property_name)
                            x.as_xml(parent=prop, **extraargs)
                        else:
                            x.as_xml(parent=root, **extraargs)
        except MissingPrefix:
            raise
        return root

    def __repr__(self):
        name = type(self).__name__
        q = {}
        for x, y in self.attr_to_suffix.items():
            try:
                q[y] = getattr(self, x)
            except AttributeError:
                pass
        return f"<{name}:{q}>"

class rif_element(node, prefix_transporter):
    @classmethod
    def _parse(cls, parseresults):
        keywords = parseresults.asDict()
        return cls(**keywords)

class MetaContainer(_xml_root_generator):
    """as_xml appends id
    """
    def __init__(self, iri=None, config=None, **kwargs):
        super().__init__(**kwargs)
        self.iri = iri
        self.config = config

    def as_xml(self, **kwargs):
        root = super().as_xml(**kwargs)
        if self.iri is not None:
            elem_id = ET.SubElement(root, "id")
            self.iri.as_xml(parent=elem_id)
        if self.config is not None:
            meta_id = ET.SubElement(root, "meta")
            self.config.as_xml(parent=meta_id)
        return root

    def __iter__(self):
        for x in super().__iter__():
            yield x
        if self.iri is not None:
            yield self.iri
        if self.config is not None:
            yield self.config


class TextContainer(node):
    def __init__(self, text, **kwargs):
        super().__init__(**kwargs)
        self.text = text

    def as_xml(self, **kwargs):
        root = super().as_xml(**kwargs)
        root.text = self.text
        return root

