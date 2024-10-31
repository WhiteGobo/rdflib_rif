"""Pyparsing for EBNF of rif prd markup language.

:TODO: ATOMIC(from documentation `https://www.w3.org/TR/2013/REC-rif-core-20130205/`_)
    and ACTION_BLOCK seems to be the same. More documentation needed.

All basic building blocks are:  `ANGLEBRACKIRI`, `CURIE`, `Const`,
`CONSTSHORT`, `Name`, `NCName`.
"""

import pyparsing as pp
from pyparsing import pyparsing_common as _pp_common
import abc
import re
import rdflib
from rdflib import URIRef, Literal, RDF
from rdflib.compat import decodeUnicodeEscape
import typing as typ
from .container import complete_object, ObjVar, ObjNew, MetaObject,\
        ObjMember, ObjEqual, ObjSubclass, \
        ObjPrefix, ObjSlot, ObjName, ObjNamedArg,\
        ObjFrame, ObjAtom, ObjExpr, _ObjList,\
        ObjConstIRI, ObjConstValue, ObjConstLiteral, ObjConstBlankNode,\
        ObjExternal, ObjExists, ObjNotFormula,\
        ObjAssert, ObjRetract, ObjModify, ObjExecute, ObjDo, _ObjRetractSlot,\
        ObjAndAction,\
        ObjAndFormula, ObjOrFormula,\
        ObjImpliesCore, ObjImpliesPRD, ObjForall,\
        ObjDocument, ObjGroup, ObjImport,\
        createLocalIri, curieToIri

from ..rif_namespace import XSD

_DEBUG = False
"""bool: setting if debug is enabled. Currently only usable for development"""

def _mask(expr: "ParserElement"):
    return pp.MatchFirst([expr])

def _suppr(expr : "ParserElement | str"):
    return pp.Suppress(expr).set_name(str(expr))

class my_exc(pp.ParseFatalException):
    """Baseclass to enable easy access to exception throwing within 
    pyparsing
    """
    msg = "Not specified Error"
    def __init__(self, s, loc, msg):
        super().__init__(s, loc, f"{self.msg}: '{msg}'")

    @classmethod
    def _raise_this(cls, s, location, t):
        raise cls(s, location, t[0])

    @classmethod
    def raise_if(cls, identifier=pp.Regex(".+")):
        return identifier.setParseAction(cls._raise_this)

    @classmethod
    def capture_any(cls):
        return cls.raise_if(pp.Regex(".*"))

class _exc_endofgroup(my_exc):
    msg = "Expected group, rule or ')', got:"

class _exc_rifprd(my_exc):
    msg = "This doesnt look like a rif-prd document. It must start "\
            "with Document, got:"

class _exc_retract(my_exc):
    msg = "Retracts targets expect (atom| frame| (term+term)| term), got:"

class _exc_modify(my_exc):
    msg = "Modify target expect frame, got:"

class _exc_group(my_exc):
    msg = "Group expects here (Rule | Group), got something like:"

class _exc_implies1(my_exc):
    msg = "Implies expects here a formula, got something like:"

class _exc_implies2(my_exc):
    msg = "Implies expects here an Actionblock, got something like:"

class _exc_rule(my_exc):
    msg = "Forall expects here a Rule, got something like:"

class _exc_meta(my_exc):
    msg = "Meta expects (<iri>, And(...), '*)'), got:"

from rdflib.plugins.sparql.parser import IRIREF, String, VARNAME, String, LANGTAG
import rdflib.plugins.sparql.parser as _rdflib_sparql_parser

## BASIC building blocks

class RIFPSParser:
    def __init__(self, loglevel: int =0):
        self._parser = self.__create_parser()

    def parseString(self, inputstring: str):
        return self._parser.parseString(inputstring)

    def _create_NumericLiteral(self):
        #from rdflib.plugins.sparql.parser import NumericLiteral 
        INTEGER = pp.Regex(r"[+-]?[0-9]+")
        INTEGER.setParseAction(lambda x: rdflib.Literal(x[0], datatype=rdflib.XSD.integer))

        DECIMAL = pp.Regex(r"[+-]?[0-9]*\.[0-9]+")  # (?![eE])
        DECIMAL.setParseAction(lambda x: rdflib.Literal(x[0], datatype=rdflib.XSD.decimal))

        EXPONENT_re = "[eE][+-]?[0-9]+"
        DOUBLE = pp.Regex(r"[+-]?[0-9]+\.[0-9]*%(e)s|\.([0-9])+%(e)s|[0-9]+%(e)s" % {"e": EXPONENT_re})
        DOUBLE.setParseAction(lambda x: rdflib.Literal(x[0], datatype=rdflib.XSD.double))

        NumericLiteral = DOUBLE | DECIMAL | INTEGER
        return NumericLiteral.set_name("NumericLiteral")

    def _create_curie(self):
        PN_CHARS_BASE_re = "A-Za-z\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02FF\u0370-\u037D\u037F-\u1FFF\u200C-\u200D\u2070-\u218F\u2C00-\u2FEF\u3001-\uD7FF\uF900-\uFDCF\uFDF0-\uFFFD\U00010000-\U000EFFFF"
        PERCENT_re = "%[0-9a-fA-F]{2}"
        PN_LOCAL_ESC_re = "\\\\[_~\\.\\-!$&\"'()*+,;=/?#@%]"
        PLX_re = "(%s|%s)" % (PN_LOCAL_ESC_re, PERCENT_re)
        PN_CHARS_U_re = "_" + PN_CHARS_BASE_re
        PN_CHARS_re = "\\-0-9\u00B7\u0300-\u036F\u203F-\u2040" + PN_CHARS_U_re

        PN_PREFIX = pp.Regex(
                "[%s](?:[%s\\.]*[%s])?" % (PN_CHARS_BASE_re, PN_CHARS_re, PN_CHARS_re), flags=re.U
                )
        PNAME_NS = pp.Combine(pp.Optional(PN_PREFIX) + pp.Suppress(":"))\
                .set_name("PNAME_NS")
        start_char = f"([{PN_CHARS_U_re}s:0-9]|{PLX_re}s)"
        mid_char = f"([{PN_CHARS_re}s\\.:]|{PLX_re}s)"
        end_char = f"([{PN_CHARS_re}s:]|{PLX_re}s)".replace("\\-", "")

        PN_LOCAL = pp.Regex("%s(%s*%s)?" % (start_char, mid_char, end_char),
                            flags=re.X | re.UNICODE,
                            ).set_name("PNLOCAL")

        CURIE = (PNAME_NS + pp.Optional(PN_LOCAL))\
                .set_name("CURIE")\
                .set_parse_action(curieToIri)
        """: shortend iri (eg `ex:suffix`) """
        return CURIE

    def _create_anglebrackiri(self):
        body = pp.Regex(r'[^<>"{}|^`\\%s]*'
                        % "".join("\\x%02X" % i for i in range(33)))
        ANGLEBRACKIRI = pp.Combine(_suppr("<") - body + _suppr(">"))\
                .set_name("ANGLEBRACKIRI")\
                .set_parse_action(lambda x: rdflib.URIRef(x[0]))
        """: full iri. Same as `rdflib.plugins.sparql.parser.IRIREF`
        see `https://www.w3.org/TR/rdf-sparql-query/#rIRI_REF`_
        """
        return ANGLEBRACKIRI

    def _create_iriconst(self):
        return self._create_anglebrackiri() | self._create_curie()

    def _create_literal_parser(self):
        _tmp = lambda x: rdflib.Literal(decodeUnicodeEscape(x[0]))
        STRING_LITERAL_LONG1 = pp.Combine(
                _suppr("'''")\
                + pp.Regex("(?:(?:'|'')?(?:[^'\\\\]|\\\\['ntbrf\\\\]))*")\
                + _suppr("'''"))\
                .setParseAction(_tmp)

        STRING_LITERAL_LONG2 = pp.Combine(
                _suppr('"""')\
                + pp.Regex('(?:(?:"|"")?(?:[^"\\\\]|\\\\["ntbrf\\\\]))*')\
                + _suppr('"""'))\
                .setParseAction(_tmp)

        STRING_LITERAL1 = pp.Combine(\
                _suppr("'")\
                + pp.Regex("(?:[^'\\n\\r\\\\]|\\\\['ntbrf\\\\])*", flags=re.U)\
                + _suppr("'"))\
                .setParseAction(_tmp)

        STRING_LITERAL2 = pp.Combine(\
                _suppr('"')\
                + pp.Regex('(?:[^"\\n\\r\\\\]|\\\\["ntbrf\\\\])*', flags=re.U)\
                + _suppr('"'))\
                .setParseAction(_tmp)

        LANGTAG = pp.Combine(_suppr("@")\
                + pp.Regex("[a-zA-Z]+(?:-[a-zA-Z0-9]+)*"))
        OPT_LANGTAG = pp.Optional(LANGTAG, default=None)
        TYPETAG = pp.Combine(_suppr("^^") + self._create_iriconst())\
                .set_results_name("datatype")
        OPT_TYPETAG = pp.Optional(TYPETAG, default=None)
                                 
        literal = STRING_LITERAL_LONG1 | STRING_LITERAL_LONG2\
                | STRING_LITERAL1 | STRING_LITERAL2
        return pp.Combine(literal.set_results_name("lexical_or_value")\
                + pp.Optional(LANGTAG.set_results_name("lang")
                              | TYPETAG.set_results_name("datatype")))\
                .set_parse_action(ObjConstLiteral._parse_pyparsing)\
                .set_name("LANGLITERAL")

    def __create_parser(self, loglevel: int = 0) -> "PyParser":
        ANGLEBRACKIRI = IRIREF.copy().set_name("ANGLEBRACKIRI")
        """: full iri. Same as `rdflib.plugins.sparql.parser.IRIREF`
        see `https://www.w3.org/TR/rdf-sparql-query/#rIRI_REF`_
        """
        ANGLEBRACKIRI.set_name("ANGLEBRACKIRI")

        CURIE = self._create_curie()

        NCName = VARNAME.copy().set_name("NCName")
        """:Namespace name
        see `https://www.w3.org/TR/2006/REC-xml-names11-20060816/#NT-NCName`_
        """

        Name = NCName.copy()
        """:arbitrary Name
        see `https://www.w3.org/TR/rif-core/#Terms_of_RIF-Core`_
        :TODO: missing '"' UNICODESTRING '"'
        """

        IRICONST = self._create_iriconst().set_name("IRICONST")\
                .set_results_name("constIRI")\
                .set_parse_action(ObjConstIRI._parse_pyparsing)
        """: Some iri identifier"""


        #_iri = _rdflib_sparql_parser.iri.copy()
        #_iri.add_parse_action(rif_container.Const_shortenediri._parse)
        _localiri = pp.Combine(_suppr('_')- Name.set_results_name("constname"))\
                .set_name("LocalIRI")\
                .set_parse_action(ObjConstBlankNode._parse_pyparsing)
        _literal = _mask(self._create_literal_parser())\
                .set_name("Literal")
        _NumericLiteral = self._create_NumericLiteral()\
                .set_name("NumericLiteral")
        _CONSTRSHORT_WITH_LANGTAG = pp.Combine(String + LANGTAG)
        #_CONSTRSHORT_WITH_LANGTAG.set_parse_action(rif_container.Const_withlang._parse)

        CONSTSHORT = pp.MatchFirst((
            IRICONST,
            _mask(_NumericLiteral).set_results_name("value").add_parse_action(ObjConstValue._parse_pyparsing),
            _localiri,
            _literal,
            ))
        """: Shortcut for different literals
        see `https://www.w3.org/TR/2013/REC-rif-dtb-20130205/#sec-shortcuts-constants`_
        """

        NCName.set_name("NCName")
        Name.set_name("Name")
        CONSTSHORT.set_name("CONSTSHORT")

        Const = _mask(CONSTSHORT)\
                .set_results_name("value").set_name("Const")\
                .set_debug(_DEBUG)
        """: All constant Literals and IRIs(no blank nodes)
        Parser returns object of type Node.

        :TODO: Missing  "asdf"^^<qwer>
        """

        ## rule language:

        IRIMETA = pp.Forward().set_name("IRIMETA")
        def _add_meta(expr: pp.ParseExpression, force=False):
            if force:
                meta = IRIMETA
            else:
                meta = pp.Optional(IRIMETA, default=None)
            with_meta = meta.set_results_name("meta")\
                    + _mask(expr).set_results_name("object")
            return with_meta.set_parse_action(complete_object).set_debug(_DEBUG)

        TERM = pp.Forward()

        _slot = (
                _mask(Name).set_results_name("argname")\
                + _suppr("->")
                + _add_meta(TERM).set_results_name("argvalue")
                ).set_parse_action(ObjNamedArg._parse_pyparsing)
        UNITERM = (
                _add_meta(Const).set_results_name("op")\
                + _suppr('(').set_debug(_DEBUG)\
                + (pp.OneOrMore(_slot, stop_on=_suppr(')')).set_results_name("slots")\
                | pp.ZeroOrMore(_add_meta(TERM), stop_on=_suppr(')')).set_results_name("args"))\
                + _suppr(')')\
                ).set_name("UNITERM")\
                .set_debug(_DEBUG)
        """: Building block for information snippets."""

        Expr = pp.MatchFirst([UNITERM])\
                .set_name("Expr")\
                .set_debug(_DEBUG)\
                .set_parse_action(ObjExpr._parse_pyparsing)
        """: Any part of an information piece that is representable by predicate+,
        so anything like `p(...)` used within an information piece like eg: `A = B`.

        An `Expr` might be a part of an `Atom`.
        """

        External_Expr = _suppr('External') + _suppr('(') + Expr + _suppr(')')
        """: External defined information part"""

        Var = (_suppr('?') - Name.set_results_name("text")\
                ).set_parse_action(ObjVar._parse_pyparsing)\
                .set_name("Var")
        """: Variable for a information piece.
        see `https://www.w3.org/TR/2013/REC-rif-bld-20130205/#EBNF_Grammar_for_the_Presentation_Syntax_of_RIF-BLD_.28Informative.29`_
        """

        List = (_suppr('List') - (_suppr('(')\
                + pp.Optional(\
                pp.OneOrMore(_add_meta(TERM), stop_on=_suppr('[|)]')).set_results_name("items")\
                + pp.Optional(_suppr('|') + _add_meta(TERM).set_results_name("rest"))\
                )\
                + _suppr(')')))\
                .set_parse_action(_ObjList._parse_pyparsing)
        """
        see `https://www.w3.org/TR/2013/REC-rif-bld-20130205/#EBNF_Grammar_for_the_Presentation_Syntax_of_RIF-BLD_.28Informative.29`_
        """

        External_term = (\
                _suppr('External') + _suppr('(')\
                - _add_meta(Expr).set_results_name("content")\
                + _suppr(')')\
                ).set_parse_action(ObjExternal._parse_pyparsing)\
                .set_debug(_DEBUG)\
                .set_name("External(term)")
        """: External defined part of information"""

        TERM <<= pp.MatchFirst((Var, List, External_term, Expr, Const))\
                .set_name("TERM")
        """: An information part used to construct a informatio piece."""

        _frame_slot = (
                _add_meta(TERM).set_results_name("slotkey")\
                + _suppr("->")
                + _add_meta(TERM).set_results_name("slotvalue")
                ).set_parse_action(ObjSlot._parse_pyparsing)
        Frame = (_add_meta(TERM).set_results_name("object_")\
                + _suppr('[')\
                - (pp.OneOrMore(_add_meta(_frame_slot), stop_on=_suppr(']')).set_results_name("slots")\
                + _suppr(']'))\
                ).set_name("Frame")\
                .set_parse_action(ObjFrame._parse_pyparsing)

                
        Meta_Frames = ((_suppr('And') - _suppr('(')\
                + pp.ZeroOrMore(Frame, stop_on=_suppr(')'))\
                +_suppr(')')))\
                .set_name("FRAMES")

        IRIMETA <<= (_suppr('(*')\
                - (pp.Optional(IRICONST).set_results_name("iri")\
                + (Meta_Frames.set_results_name("config_multi")\
                | pp.Optional(Frame).set_results_name("config_single")\
                )+ _suppr('*)'))\
                ).set_parse_action(MetaObject._parse_pyparsing)\
                .set_name("IRIMETA")
        """Metainformation like identifier or further information."""

        ### Formulas

        Atom = pp.MatchFirst([UNITERM])\
                .set_name("Atom")\
                .set_debug(_DEBUG)\
                .set_parse_action(ObjAtom._parse_pyparsing)
        """: Information piece."""


        External_Atom = _add_meta(_suppr('External') + _suppr('(')\
                + Atom.set_results_name("content")\
                + _suppr(')'))\
                .set_name("External Atom")\
                .set_parse_action(ObjExternal._parse_pyparsing)
        """: External defined information piece.
        :TODO: Where is this needed?
        """

        Equal = ( _add_meta(TERM).set_results_name("left") \
                + _suppr('=') - _add_meta(TERM).set_results_name("right")\
                ).set_parse_action(ObjEqual._parse_pyparsing)\
                .set_name("Equal(ATOMIC)")\
                .set_debug(_DEBUG)


        Member = ( _add_meta(TERM).set_results_name("instance")\
                + _suppr('#')\
                + _add_meta(TERM).set_results_name("class_")\
                ).set_parse_action(ObjMember._parse_pyparsing)\
                .set_name("Member(ATOMIC)")\
                .set_debug(_DEBUG)


        Subclass = ( _add_meta(TERM).set_results_name("sub")\
                + _suppr('##') - _add_meta(TERM).set_results_name("super_")\
                ).set_parse_action(ObjSubclass._parse_pyparsing)\
                .set_name("Subclass(ATOMIC)")\
                .set_debug(_DEBUG)#.set_parse_action()

        FORMULA = pp.Forward().set_name("FORMULA")
        And_formula = ( _suppr('And') - _suppr('(')\
                - pp.ZeroOrMore(_add_meta(FORMULA), stop_on=_suppr(')')).set_results_name("formulas")\
                - _suppr(')')\
                ).set_name("And(formula)")\
                .set_debug(_DEBUG)\
                .set_parse_action(ObjAndFormula._parse_pyparsing)

        Or_formula = ( _suppr('Or') - _suppr('(')\
                + pp.ZeroOrMore(_add_meta(FORMULA), stop_on=_suppr(')')).set_results_name("formulas")\
                + _suppr(')')\
                ).set_name("Or(formula)")\
                .set_debug(_DEBUG)\
                .set_parse_action(ObjOrFormula._parse_pyparsing)

        Exists = ( _suppr('Exists')\
                - pp.OneOrMore(_add_meta(Var)).set_results_name("vars_")\
                + _suppr('(') + _add_meta(FORMULA).set_results_name("formula")\
                + _suppr(')')\
                ).set_name("Exists(formula)")\
                .set_debug(_DEBUG)\
                .set_parse_action(ObjExists._parse_pyparsing)

        NEGATEDFORMULA = ( _suppr(pp.oneOf('Not', 'INEG'))\
                - _suppr('(') + _add_meta(FORMULA).set_results_name("formula")\
                + _suppr(')')\
                ).set_parse_action(ObjNotFormula._parse_pyparsing)\
                .set_debug(_DEBUG)\
                .set_name("Not(formula)")

        ATOMIC = pp.MatchFirst((Equal, Subclass, Member, Frame, Atom))\
                .set_name("ATOMIC")\
                .set_debug(_DEBUG)
        """: Any existing information piece constructed
        via a predicate+ (anything like `p(...)`)
        See 'https://www.w3.org/TR/2013/REC-rif-bld-20130205/#Formulas'_ for a definition
        """

        External_atom = ( _suppr('External') - _suppr('(')\
                + _add_meta(Atom).set_results_name("content")\
                + _suppr(')'))\
                .set_name("External(formula)")\
                .set_debug(_DEBUG)\
                .set_parse_action(ObjExternal._parse_pyparsing)

        #getting set_debug to work seems tricky. i dont understand.
        FORMULA <<= pp.MatchFirst((
            And_formula, Or_formula, Exists,
            NEGATEDFORMULA, ATOMIC, External_atom,
            ))
        """: Any atomic formula and conditions constructed from those atomic formulas
        See 'https://www.w3.org/TR/2013/REC-rif-bld-20130205/#Formulas'_ for a definition
        """

        ## Actions:

        Assert = ( _suppr('Assert') - _suppr('(')\
                + _add_meta( Atom | Frame | Member ).set_results_name("target")\
                + _suppr(')')\
                ).set_parse_action(ObjAssert._parse_pyparsing)\
                .set_name("Assert")
        """: Assert action
        see `https://www.w3.org/TR/2013/REC-rif-prd-20130205/`_
        """

        _SlotRetract = (_add_meta(TERM).set_results_name("first")\
                + _add_meta(TERM).set_results_name("second"))\
                .set_parse_action(_ObjRetractSlot._parse_pyparsing)
        Retract = ( _suppr('Retract') - _suppr('(')\
                + _add_meta(Atom ^ Frame ^ _SlotRetract ^ TERM).set_results_name("target")\
                .set_results_name("target")\
                + _suppr(')')\
                ).set_parse_action(ObjRetract._parse_pyparsing)\
                .set_name("Retract")
        """: Retract action
        see `https://www.w3.org/TR/2013/REC-rif-prd-20130205/`_
        """

        Modify = (_suppr('Modify') - _suppr('(')\
                + _add_meta(Frame).set_results_name("target")\
                + _suppr(')')\
                ).set_parse_action(ObjModify._parse_pyparsing)\
                .set_debug(_DEBUG)\
                .set_name("Modify")
        """: Modify action
        see `https://www.w3.org/TR/2013/REC-rif-prd-20130205/`_
        """

        Execute = (_suppr('Execute') - _suppr('(')\
                + _add_meta(Atom).set_results_name("target")\
                + _suppr(')')\
                ).set_parse_action(ObjExecute._parse_pyparsing)\
                .set_name("Execute")
        """: Execute action
        see `https://www.w3.org/TR/2013/REC-rif-prd-20130205/`_
        """

        ACTION = pp.MatchFirst((Assert, Retract, Modify, Execute))

        New = (_suppr("New") - _suppr("(") + _suppr(")")\
                ).set_name("New()").set_parse_action(ObjNew._parse_pyparsing)
        """: initialize variable as new
        """

        _VAR_INIT_SLOT = (_suppr('(') + _add_meta(Var)\
                + _add_meta(New | Frame) + _suppr(')'))
        _DO_ACTION = (_suppr("Do") - _suppr("(")\
                + pp.ZeroOrMore(_VAR_INIT_SLOT).set_results_name("actionVar")\
                + pp.OneOrMore(_add_meta(ACTION), stop_on=_suppr(')')).set_results_name("actions")\
                + _suppr(')')\
                ).set_name("DO")\
                .set_parse_action(ObjDo._parse_pyparsing)

        _AND_ACTION = (_suppr('And') - _suppr('(')\
                - pp.ZeroOrMore(_add_meta(Atom | Frame), stop_on=_suppr(')'))\
                .set_results_name("actions")\
                - _suppr(')')).set_name("And(Action)")\
                .set_parse_action(ObjAndAction._parse_pyparsing)
        ACTION_BLOCK = (_DO_ACTION | _AND_ACTION | Atom | Frame)\
                .set_debug(_DEBUG)\
                .set_name("ACTION_BLOCK")
        """: Complete action block
        see `https://www.w3.org/TR/2013/REC-rif-prd-20130205/`_
        """

        ## RULE Language

        LOCATOR = pp.MatchFirst([ANGLEBRACKIRI])
        """: Location of import directive"""

        PROFILE = pp.MatchFirst([ANGLEBRACKIRI])
        """: Profile of import directive"""

        Import = (_suppr('Import') - _suppr('(')\
                + LOCATOR.set_results_name("location")\
                + pp.Optional(PROFILE.set_results_name("profile"))\
                + _suppr(')')\
                ).set_name("Import")\
                .set_parse_action(ObjImport._parse_pyparsing)
        """: Complete import directive
        see``_
        """

        Implies_PRD = (_suppr('If')\
                - _add_meta(FORMULA).set_results_name("if_")\
                + _suppr('Then')\
                - _add_meta(_mask(ACTION_BLOCK)).set_results_name("then")\
                ).set_parse_action(ObjImpliesPRD._parse_pyparsing)\
                .set_debug(_DEBUG)\
                .set_name("Implies(PRD)")
        """: Implies in production rules
        see `https://www.w3.org/TR/2013/REC-rif-prd-20130205/`_
        """

        AND_ATOMIC = (pp.Or((ATOMIC)).set_parse_action(lambda x:list(x))\
                | (_suppr('And') + _suppr('(')\
                + (pp.ZeroOrMore(_add_meta(ATOMIC), stop_on=_suppr(')'))\
                + _suppr(')'))))\
                .set_name("And(ATOMIC)")
        Implies_Core = (AND_ATOMIC.set_results_name("then")\
                + _suppr(':-')\
                - _add_meta(FORMULA).set_results_name("if_")\
                ).set_parse_action(ObjImpliesCore._parse_pyparsing)\
                .set_debug(_DEBUG)\
                .set_name("Implies(Core)")
        """: Implies in core and bld
        see ``_
        """

        CLAUSE = (_DO_ACTION | _AND_ACTION | Implies_Core | Implies_PRD | ATOMIC)\
                .set_name("CLAUSE")\
                .set_debug(_DEBUG)
        """: Something to do"""

        RULE = pp.Forward()
        """: Rule"""

        such_that_FORMULA = _suppr('such that')\
                - pp.OneOrMore(_add_meta(FORMULA))
        _var_end = pp.Suppress('(') | pp.Suppress('such that')
        Forall = (_suppr('Forall')\
                - (pp.OneOrMore(_add_meta(Var), stop_on=_var_end).set_results_name("vars_")\
                + pp.Optional(such_that_FORMULA).set_results_name("pattern")\
                + _suppr('(')\
                + _add_meta(RULE).set_results_name("formula")\
                + _suppr(')'))\
                ).set_name("Forall")\
                .set_debug(_DEBUG)\
                .set_parse_action(ObjForall._parse_pyparsing)

        RULE <<= (Forall | CLAUSE).set_debug(_DEBUG).set_name("RULE")

        Strategy = pp.MatchFirst([Const])
        """: Strategy of group(prd)"""

        Priority = pp.MatchFirst([Const])
        """: Priority of group(prd)"""

        Group = pp.Forward()
        Group <<= (_suppr('Group')\
                -( pp.Optional(Strategy).set_results_name("Strategy")\
                + pp.Optional(Priority).set_results_name("Priority")\
                + _suppr('(')\
                + pp.ZeroOrMore(_add_meta(Group | RULE),
                                stop_on=_suppr(')'),
                                ).set_results_name("sentences")\
                + _suppr(')')\
                )).set_name("Group")\
                .set_debug(_DEBUG)\
                .set_parse_action(ObjGroup._parse_pyparsing)
        """: Rule group"""

        Base = _suppr('Base') - _suppr('(') - ANGLEBRACKIRI\
                - _suppr(')')
        """: Im not sure"""

        Prefix = (_suppr('Prefix') - _suppr('(')\
                - Name.set_results_name("shortcut")\
                - ANGLEBRACKIRI.set_results_name("iri")\
                - _suppr(')')\
                ).set_parse_action(ObjPrefix._parse_pyparsing)
        """: Prefix shortcuts available in Document"""

        Document = (_suppr('Document')\
                - _suppr('(')\
                + pp.Optional(Base).set_results_name("Base")\
                + pp.ZeroOrMore(Prefix).set_results_name("Prefixes")\
                + pp.ZeroOrMore(_add_meta(Import)).set_results_name("directive")\
                + pp.Optional(_add_meta(Group).set_results_name("payload"))\
                + _suppr(')'\
                ))\
                .set_name("Document")\
                .set_parse_action(ObjDocument._parse_pyparsing)
        """: Complete Document"""

        RIFPRD_PS = _add_meta(Document | Group | FORMULA | RULE)
        """This should contain all possible Things with metadata. It is used, when
        parsing arbitrary data in RIFPRD-PS.
        """

        return RIFPRD_PS.set_debug(_DEBUG)
