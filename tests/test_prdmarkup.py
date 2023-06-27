import rdflib_rif

import unittest
import logging
logger = logging.getLogger(__name__)
import re

from rdflib_rif.rifpl import ebnf as Parsing

import importlib.resources
from . import testrif_documents
blueprint = importlib.resources.files(testrif_documents).joinpath("test.rifpl")


class TestMarkupParser(unittest.TestCase):
    def test_term_Name(self):
        p = Parsing.NCName.parse_string
        w = "variablename"
        q = p(w)
        self.assertTrue(q, q)

    def test_term_CURIE(self):
        p = Parsing.NCName.parse_string
        for w in ["ex:Customer", "_:Customer" ]:
            q = p(w)
            self.assertTrue(q, q)

    def test_term_Var(self):
        p = Parsing.Var.parse_string
        w = "?Var"
        p(w)
        p = Parsing.TERM.parse_string
        p(w)

    def test_Frame(self):
        p = Parsing.Frame.parse_string
        for w in [
                "ex1:asdf[ex1:value -> ex1:voucher]",
                "?s[ex1:value -> ex1:voucher]",
                "<http://example.org/example#John>[<http://example.org/example#status> -> 'unknown' <http://example.org/example#discount> -> '0']",
                ]:
            p(w)

    def test_External(self):
        w = "func:numeric-multiply(?val 0.90)"
        p = Parsing.Atom.parse_string
        p(w)
        p = Parsing.Expr.parse_string
        p(w)

        w = "External(func:numeric-multiply(?val 0.90))"
        p = Parsing.External_term.parse_string
        p(w)



    def test_Action(self):
        p = Parsing.ACTION.parse_string
        for w in [
                "Modify(?s[ex1:value -> External(func:numeric-multiply(?val 0.90))]))",
                "Retract(?customer ex1:voucher)",
                ]:
            with self.subTest(w):
                p(w)

    def test_term_ANGLEBRACKIRI(self):
        p = Parsing.ANGLEBRACKIRI.parse_string
        w = "<http://example.com/people#John>"
        q = p(w)
        self.assertTrue(q, q)

    def test_term_CONST(self):
        """tests parser for all kinds of literals
        """
        p = Parsing.Const.parse_string
        for w in ["<http://example.com/people#John>",
                  "ex:Customer",
                  '12',
                  ' "John"@en ',
                  ' "01"^^xs:integer ',
                  ]:
            with self.subTest(w):
                q = p(w)

    def test_formula(self):
        p = Parsing.FORMULA.parseString
        w = """And(?shoppingCart[ex1:value -> ?value]
                                        External(pred:numeric-greater-than-or-equal(?value 2000)))
        """
        p(w)
        w = """Exists ?value (And(?shoppingCart[ex1:value -> ?value]
                                        External(pred:numeric-greater-than-or-equal(?value 2000))))
                                        """
        p(w)
        #print(p(w))
        w = """?customer[ex1:shoppingCart -> ?shoppingCart]"""
        p(w)

    def test_actionblock(self):
        p = Parsing.ACTION_BLOCK.parseString
        for w in ["""Do(Modify(?customer[ex1:status -> "Gold"]))""",
                  #and action
                  #frame
                  #atom
                  ]:
            with self.subTest(w):
                p(w)

    def test_implies(self):
        p = Parsing.Implies.parseString
        w = """If Exists ?value (And(?shoppingCart[ex1:value -> ?value]
                                        External(pred:numeric-greater-than-or-equal(?value 2000))))
                  Then Do(Modify(?customer[ex1:status -> "Gold"]))
                  """
        p(w)
        w = """
      If Not(Exists ?status
                     (And(?customer[ex1:status -> ?status]
                          External(pred:list-contains(List("New" "Bronze" "Silver" "Gold") ?status)) )))
       Then Do( Execute(act:print(External(func:concat("New customer: " ?customer))))
                Assert(?customer[ex1:status -> "New"]))
                """
        p(w)

    def test_RULE(self):
        p = Parsing.RULE.parseString
        w = """Forall ?shoppingCart such that ?customer[ex1:shoppingCart -> ?shoppingCart]
                 (If Exists ?value (And(?shoppingCart[ex1:value -> ?value]
                                        External(pred:numeric-greater-than-or-equal(?value 2000))))
                  Then Do(Modify(?customer[ex1:status -> "Gold"])))
            """
        p(w)

    def test_Group(self):
        p = Parsing.Group.parseString
        w = """Group  (
            Forall ?customer such that And(?customer # ex1:Customer
                                           ?customer[ex1:status -> "Silver"])
              (Forall ?shoppingCart such that ?customer[ex1:shoppingCart -> ?shoppingCart]
                 (If Exists ?value (And(?shoppingCart[ex1:value -> ?value]
                                        External(pred:numeric-greater-than-or-equal(?value 2000))))
                  Then Do(Modify(?customer[ex1:status -> "Gold"])))))
          """
        p(w)

    def test_IRIMETA(self):
        p = Parsing.IRIMETA.parseString
        w = "(* ex1:CheckoutRuleset *)"
        p(w)


    def test_Document(self, rifpldocument = blueprint):
        p = Parsing.Document.parseFile

        q = p(rifpldocument)[0]
        import xml.etree.ElementTree as ET
        from xml.dom import minidom
        from rdflib import URIRef
        self.assertEqual(q._prefixes["ex1"],
                         URIRef("http://example.com/2009/prd2#"))
        q._transport_prefix()
        xmlstr = ET.tostring(q.as_xml())
        pretty_xml = minidom.parseString(xmlstr).toprettyxml(indent='  ')

        from rdflib_rif.rifxml_validation import validate_str

        from lxml.etree import DocumentInvalid
        try:
            validate_str(pretty_xml)
        except DocumentInvalid as err:
            def repl(m):
                repl.cnt+=1
                return f'{repl.cnt:03d}: '
            repl.cnt=0
            logger.warning(re.sub(r'(?m)^', repl, pretty_xml))
            raise Exception("failed to validate resulting document. See "
                            "logging for resulting xml-file") from err
        logger.debug("Created succesfully rifxml:\n%s" % pretty_xml)



if __name__=='__main__':
    logging.basicConfig( level=logging.WARNING )
    #flowgraph_logger = logging.getLogger("find_generationpath.Flowgraph")
    #graphstate_logger = logging.getLogger("find_generationpath.Graphstate")
    #flowgraph_logger.setLevel(logging.DEBUG)
    #graphstate_logger.setLevel(logging.DEBUG)
    logger.setLevel(logging.DEBUG)
    unittest.main()
