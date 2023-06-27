import rdflib_rif

import unittest
import logging
logger = logging.getLogger(__name__)

import rdflib
import rdflib.serializer
import rdflib.parser
import rdflib.plugin
import rdflib.compare
import urllib.parse

from rdflib_rif.rifpl import ebnf as Parsing

import pathlib
import os.path
import importlib.resources
from . import data
TestCases_info = rdflib.Namespace(\
        os.path.join(importlib.resources.files(data).as_uri(), ""))
#RIFTest = rdflib.Namespace("https://www.w3.org/TR/2013/NOTE-rif-test-20130205/#")
RIFTest = rdflib.Namespace("http://www.w3.org/2009/10/rif-test#")

g = rdflib.Graph(identifier=TestCases_info)
g.parse(getattr(TestCases_info, "RIF_testcases.ttl"))
g.bind("rif", RIFTest)
PositiveEntailmentTests = list(g.subjects(object=RIFTest.Positive_Entailment_Tests))
PositiveSyntaxTests = list(g.subjects(object=RIFTest.Positive_Syntax_Tests))
NegativeEntailmentTests = list(g.subjects(object=RIFTest.Negative_Entailment_Tests))
NegativeSyntaxTests = list(g.subjects(object=RIFTest.Negative_Syntax_Tests))
ImportRejectionTests = list(g.subjects(object=RIFTest.Import_Rejection_Tests))

import xml.etree.ElementTree as ET
class _doc:
    def __init__(self, syntax, name=None, remote=None, data=None, path=None):
        self.syntax = syntax
        self.remote = remote
        self.name = name
        self.data = data
        self.path = path
        if name is not None and path is not None:
            self.local = os.path.join(path, name)
        else:
            self.local = None


    @classmethod
    def from_xml(cls, xml_elem, path=None):
        r = "{http://www.w3.org/2009/10/rif-test#}"
        syntax = xml_elem.attrib["syntax"]
        kwargs = {}
        name_elem = xml_elem.find(r + "name")
        try:
            kwargs["remote"] = xml_elem.find(r + "remote").text
        except AttributeError:
            #fires if xml_elem.find return None
            pass
        try:
            kwargs["name"] = xml_elem.find(r + "name").text
        except AttributeError:
            #fires if xml_elem.find return None
            pass
        if xml_elem.tag == r + "Presentation":
            return cls(syntax, data=xml_elem.text, **kwargs)
        else:
            return cls(syntax, path=path, **kwargs)

    def as_parse_kwargs(self):
        kwargs = {"format": self.syntax}
        if self.data:
            kwargs["data"] = self.data
        elif self.local:
            kwargs["source"] = self.local
        elif self.remote:
            raise Exception("using remote instead of local", self.path, self.name)
            kwargs["source"] = self.remote
        else:
            raise Exception()
        return kwargs
class _Test:
    status: str
    dialect: str
    purpose: str
    description: str

    def __init__(self, status, dialect, purpose, description):
        self.status = status
        self.dialect = dialect
        self.purpose = purpose
        self.description = description

    @classmethod
    def from_xml(cls, root, path = None, **kwargs):
        r = "http://www.w3.org/2009/10/rif-test#"
        status = root.find("{%s}status" % r).text
        dialect = root.find("{%s}dialect" % r).text
        purpose = root.find("{%s}purpose" % r).text
        description = root.find("{%s}description" % r).text
        premiseDocs = []
        return cls(status=status, dialect=dialect, purpose=purpose,
                   description=description, **kwargs)

class NegativeSyntaxTest(_Test):
    """
    :TODO: Missing ImportedDocument
    """
    def __init__(self, inputDocuments, **kwargs):
        super().__init__(**kwargs)
        self.inputDocuments = inputDocuments

    @classmethod
    def from_xml(cls, root, path = None):
        r = "http://www.w3.org/2009/10/rif-test#"
        inputDocuments_n = root.find("{%s}InputDocument" % r)
        inputDocs = []
        kwargs = {}
        if path is not None:
            kwargs["path"] = path
        for e in inputDocuments_n:
            inputDocs.append(_doc.from_xml(e, **kwargs))
        return super().from_xml(root, **kwargs, inputDocuments=inputDocs)

class PositiveEntailmentTest(_Test):
    def __init__(self, premiseDocuments, conclusionDocuments, **kwargs):
        super().__init__(**kwargs)
        self.premiseDocuments = premiseDocuments
        self.conclusionDocuments = conclusionDocuments

    @classmethod
    def from_xml(cls, root, path = None):
        r = "http://www.w3.org/2009/10/rif-test#"
        premiseDocuments_n = root.find("{%s}PremiseDocument" % r)
        conclusionDocuments_n = root.find("{%s}ConclusionDocument" % r)
        premiseDocs = []
        kwargs = {}
        if path is not None:
            kwargs["path"] = path
        for e in premiseDocuments_n:
            premiseDocs.append(_doc.from_xml(e, **kwargs))
        conclusionDocs = []
        for e in conclusionDocuments_n:
            conclusionDocs.append(_doc.from_xml(e, **kwargs))
        return super().from_xml(root, **kwargs, premiseDocuments=premiseDocs,
                       conclusionDocuments=conclusionDocs)

class PositiveSyntaxTest(_Test):
    """
    :TODO: Missing ImportedDocument
    """
    def __init__(self, inputDocuments, **kwargs):
        super().__init__(**kwargs)
        self.inputDocuments = inputDocuments

    @classmethod
    def from_xml(cls, root, path = None):
        r = "http://www.w3.org/2009/10/rif-test#"
        inputDocuments_n = root.find("{%s}InputDocument" % r)
        inputDocs = []
        kwargs = {}
        if path is not None:
            kwargs["path"] = path
        for e in inputDocuments_n:
            inputDocs.append(_doc.from_xml(e, **kwargs))
        return super().from_xml(root, **kwargs, inputDocuments=inputDocs)

class NegativeEntailmentTest(_Test):
    def __init__(self, premiseDocuments, nonConclusionDocuments, **kwargs):
        super().__init__(**kwargs)
        self.premiseDocuments = premiseDocuments
        self.nonConclusionDocuments = nonConclusionDocuments

    @classmethod
    def from_xml(cls, root, path = None):
        r = "http://www.w3.org/2009/10/rif-test#"
        premiseDocuments_n = root.find("{%s}PremiseDocument" % r)
        nonConclusionDocuments_n = root.find("{%s}NonConclusionDocument" % r)
        premiseDocs = []
        kwargs = {}
        if path is not None:
            kwargs["path"] = path
        for e in premiseDocuments_n:
            premiseDocs.append(_doc.from_xml(e, **kwargs))
        nonConclusionDocs = []
        for e in nonConclusionDocuments_n:
            nonConclusionDocs.append(_doc.from_xml(e, **kwargs))
        return super().from_xml(root, **kwargs, premiseDocuments=premiseDocs,
                       nonConclusionDocuments=nonConclusionDocs)



class ImportRejectionTest(_Test):
    def __init__(self, inputDocuments, importedDocuments, **kwargs):
        super().__init__(**kwargs)
        self.importedDocuments = importedDocuments
        self.inputDocuments = inputDocuments

    @classmethod
    def from_xml(cls, root, path = None):
        r = "http://www.w3.org/2009/10/rif-test#"
        importedDocuments_n = root.find("{%s}InputDocument" % r)
        inputDocuments_n = root.find("{%s}InputDocument" % r)
        importedDocs = []
        inputDocs = []
        kwargs = {}
        if path is not None:
            kwargs["path"] = path
        for e in inputDocuments_n:
            inputDocs.append(_doc.from_xml(e, **kwargs))
        for e in importedDocuments_n:
            importedDocs.append(_doc.from_xml(e, **kwargs))
        return super().from_xml(root, **kwargs, 
                                importedDocuments = importedDocs,
                                inputDocuments = inputDocs,
                                )

class TestOfficialTestCases(unittest.TestCase):
    def setUp(self):
        rdflib.plugin.register("rifps", rdflib.parser.Parser,
                               "rdflib_rif", "RIFMarkupParser")
        rdflib.plugin.register("RIFPRD-PS", rdflib.parser.Parser,
                               "rdflib_rif", "RIFMarkupParser")

        rdflib.plugin.register("rif", rdflib.parser.Parser,
                               "rdflib_rif", "RIFXMLParser")
        rdflib.plugin.register("RIF/XML", rdflib.parser.Parser,
                               "rdflib_rif", "RIFXMLParser")


    def test_consistencyTests_PositiveSyntaxTest(self):
        """Tests in all standard tests if all possible 'samely' documents are
        translated into rdf in the same way.
        """
        testdata = []
        for testdescription in PositiveSyntaxTests:
            q = urllib.parse.urlparse(testdescription)
            root = ET.parse(q.path).getroot()
            path = pathlib.Path(q.path).parent
            try:
                testdata.append(PositiveSyntaxTest.from_xml(root, path=path))
            except Exception:
                raise Exception("defect testcase: %s" % testdescription)

        for q in testdata:
            desc = "positive syntax test:\n%s\n%s" % (q.purpose, q.description)
            with self.subTest(desc):
                logger.debug("".join(("subtest: ", desc)))
                inputs = [doc.as_parse_kwargs() for doc in q.inputDocuments]
                self.compareRIFDocuments(inputs)
                doc = inputs[0]

    @unittest.skip("Not implemented yet")
    def test_consistencyTests_ImportRejectionTest(self):
        raise NotImplementedError()

    def test_consistencyTests_NegativeSyntaxTest(self):
        """Tests in all standard tests if all possible 'samely' documents are
        translated into rdf in the same way.
        """
        testdata = []
        for testdescription in NegativeSyntaxTests:
            q = urllib.parse.urlparse(testdescription)
            root = ET.parse(q.path).getroot()
            path = pathlib.Path(q.path).parent
            try:
                testdata.append(NegativeSyntaxTest.from_xml(root, path=path))
            except Exception:
                raise Exception("defect testcase: %s" % testdescription)

        for q in testdata:
            desc = "negative syntax test:\n%s\n%s" % (q.purpose, q.description)
            with self.subTest(desc):
                logger.debug("".join(("subtest: ", desc)))
                inputs = [doc.as_parse_kwargs() for doc in q.inputDocuments]
                self.compareRIFDocuments(inputs)
                doc = inputs[0]

    def test_consistencyTests_ImportRejectionTest(self):
        """Tests in all standard tests if all possible 'samely' documents are
        translated into rdf in the same way.
        """
        testdata = []
        for testdescription in ImportRejectionTests:
            q = urllib.parse.urlparse(testdescription)
            root = ET.parse(q.path).getroot()
            path = pathlib.Path(q.path).parent
            try:
                testdata.append(ImportRejectionTest.from_xml(root,
                                                                path=path))
            except Exception:
                raise Exception("defect testcase: %s" % testdescription)

        for q in testdata:
            desc = "importe rejection test:\n%s\n%s"\
                    % (q.purpose, q.description)
            with self.subTest(desc):
                logger.debug("".join(("subtest: ", desc)))
                inputs = [doc.as_parse_kwargs() for doc in q.inputDocuments]
                self.compareRIFDocuments(inputs)
                imps = [doc.as_parse_kwargs() for doc in q.importedDocuments]
                self.compareRIFDocuments(imps)

    def test_consistencyTests_NegativeEntailmentTest(self):
        """Tests in all standard tests if all possible 'samely' documents are
        translated into rdf in the same way.
        """
        testdata = []
        for testdescription in NegativeEntailmentTests:
            q = urllib.parse.urlparse(testdescription)
            root = ET.parse(q.path).getroot()
            path = pathlib.Path(q.path).parent
            try:
                testdata.append(NegativeEntailmentTest.from_xml(root,
                                                                path=path))
            except Exception:
                raise Exception("defect testcase: %s" % testdescription)

        for q in testdata:
            desc = "negative entailment test:\n%s\n%s"\
                    % (q.purpose, q.description)
            with self.subTest(desc):
                logger.debug("".join(("subtest: ", desc)))
                pres = [doc.as_parse_kwargs() for doc in q.premiseDocuments]
                self.compareRIFDocuments(pres)
                conc = [doc.as_parse_kwargs() for doc in q.nonConclusionDocuments]
                self.compareRIFDocuments(conc)
                pre = pres[0]
                conc = conc[0]

    def test_consistencyTests_PositiveEntailmentTest(self):
        """Tests in all standard tests if all possible 'samely' documents are
        translated into rdf in the same way.
        """
        testdata = []
        for testdescription in PositiveEntailmentTests:
            q = urllib.parse.urlparse(testdescription)
            root = ET.parse(q.path).getroot()
            path = pathlib.Path(q.path).parent
            try:
                testdata.append(PositiveEntailmentTest.from_xml(root,
                                                                path=path))
            except Exception:
                raise Exception("defect testcase: %s" % testdescription)

        for q in testdata:
            desc = "positive entailment test:\n%s\n%s"\
                    % (q.purpose, q.description)
            with self.subTest(desc):
                logger.debug("".join(("subtest: ", desc)))
                pres = [doc.as_parse_kwargs() for doc in q.premiseDocuments]
                self.compareRIFDocuments(pres)
                conc = [doc.as_parse_kwargs() for doc in q.conclusionDocuments]
                self.compareRIFDocuments(conc)
                pre = pres[0]
                conc = conc[0]

    def compareRIFDocuments(self, premises: list[dict], pretty_logging=True):
        premises = iter(premises)
        x = premises.__next__()
        g1 = rdflib.Graph()
        try:
            g1.parse(**x)
        except Exception as err:
            raise Exception(x) from err
        cg1 = rdflib.compare.to_isomorphic(g1)
        for alt in premises:
            g2 = rdflib.Graph()
            try:
                if alt.get("format", None) == "RIFPRD-PS":
                    g2.parse(**alt, pretty_logging=pretty_logging)
                else:
                    g2.parse(**alt)
            except rdflib.plugin.PluginException as err:
                logger.warning(f"Skip testing syntax parsing for graph with "
                               f"arguments {alt} with reason:\n{err}")
                continue
            except Exception as err:
                raise Exception("Got exception when produced graph "
                                "with args: %s" % alt) from err
            cg2 = rdflib.compare.to_isomorphic(g2)
            try:
                self.assertEqual(cg1, cg2)
            except AssertionError as err:
                shared, infirst, insecond = rdflib.compare.graph_diff(cg1, cg2)
                g1.bind("rif", "http://www.w3.org/2007/rif#")
                for x in (shared, infirst, insecond):
                    x.bind("rif", "http://www.w3.org/2007/rif#")
                logger.debug("more information for err: %s" %err)
                logger.debug("first graph:\n%s" % g1.serialize())
                logger.debug("second graph:\n%s" % g2.serialize())
                logger.debug("info only in first:\n%s"% infirst.serialize())
                logger.debug("info only in second:\n%s"% insecond.serialize())
                logger.debug("shared info:\n%s"%shared.serialize())
                raise err


if __name__=='__main__':
    logging.basicConfig( level=logging.DEBUG )
    #flowgraph_logger = logging.getLogger("find_generationpath.Flowgraph")
    #graphstate_logger = logging.getLogger("find_generationpath.Graphstate")
    #flowgraph_logger.setLevel(logging.DEBUG)
    #graphstate_logger.setLevel(logging.DEBUG)
    logger.setLevel(logging.DEBUG)
    unittest.main()
