#from urllib.parse import urldefrag, urljoin
import xml.sax
import rdflib.parser

from . import parser
import xml.etree.ElementTree as ET
from ..parser import _parsercreator, RIFXMLHandler
import logging
logger = logging.getLogger(__name__)
import io

try:
    from xml.dom import minidom as _minidom
except ModuleNotFoundError:
    _minidom = None

class RIFMarkupParser(rdflib.parser.Parser):
    def rif_to_ttl(self, xml_source, sink):
        parser = xml.sax.make_parser()
        parser.setFeature(xml.sax.handler.feature_namespaces, 1)
        q = RIFXMLHandler(sink)
        #self.setDocumentLocator(target)
        parser.setContentHandler(q)
        parser.setErrorHandler(xml.sax.handler.ErrorHandler())

        parser.parse(xml_source)

    def parse(self, source, sink, pretty_logging=False):
        sink.bind("rif", "http://www.w3.org/2007/rif#", override=False)
        stream = source.getCharacterStream()  # try to get str stream first
        if not stream:
            # fallback to get the bytes stream
            stream = source.getByteStream()
        inputstr = stream.read()
        if isinstance(inputstr, bytes):
            inputstr = inputstr.decode("utf8")
        graph = parser.parse_rifpl(inputstr)
        #q = parser.parse_rifpl(stream)
        for ax in graph:
            sink.add(ax)
        return
        #xmlbytes = ET.tostring(q.as_xml())
        #if pretty_logging:
        #    pretty = _minidom.parseString(xmlbytes).toprettyxml(indent='  ')
        #    logger.debug("Created rif from rifps:\n%s" %(pretty))
        #else:
        #    logger.debug("Created rif from rifps: %s" %(xmlbytes))
        #rif_stream = io.BytesIO(xmlbytes)
        #try:
        #    self.rif_to_ttl(rif_stream, sink)
        #except Exception:
        #    if _minidom is not None:
        #        xmlstr = ET.tostring(q.as_xml())
        #        pretty = _minidom.parseString(xmlstr).toprettyxml(indent='  ')
        #        logger.warning(pretty)
        #    raise
