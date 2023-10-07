"""Pyparsing for EBNF of rif prd markup language.
"""

import pyparsing as pp
from pyparsing import pyparsing_common as _pp_common
import abc
import typing as typ
from . import container as rif_container

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
    def raise_if(cls, identifier):
        return identifier.setParseAction(cls._raise_this)

    @classmethod
    def capture_any(cls):
        return cls.raise_if(pp.Regex(".*"))

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

## rule language:

#ANGLEBRACKIRI ::= IRI_REF
#CURIE         ::= PNAME_LN | PNAME_NS
#CONSTSHORT    ::= ANGLEBRACKIRI // shortcut for "..."^^rif:iri
#              | CURIE          // shortcut for "..."^^rif:iri
#              | '"' UNICODESTRING '"'// shortcut for "..."^^xs:string
#              | NumericLiteral // shortcut for "..."^^xs:integer,xs:decimal,xs:double
#              | '_' NCName     // shortcut for "..."^^rif:local
#              | '"' UNICODESTRING '"' '@' langtag  // shortcut for "...@..."^^rdf:PlainLiteral
from rdflib.plugins.sparql.parser import PNAME_LN, PNAME_NS, IRIREF, LANGTAG, String, VARNAME, String
import rdflib.plugins.sparql.parser as _rdflib_sparql_parser
iri = _rdflib_sparql_parser.iri.copy()
iri.add_parse_action(rif_container.Const_shortenediri._parse)
localiri = pp.Forward()

_external_iri = _rdflib_sparql_parser.iri.copy()
_external_iri.add_parse_action(rif_container.External_Const_shortenediri._parse)
literal = _rdflib_sparql_parser.RDFLiteral.copy()
literal.add_parse_action(rif_container.literal._parse)
#literal.add_parse_action(rif_container.Const_withlang._parse)
NumericLiteral = _rdflib_sparql_parser.NumericLiteral.copy()
NumericLiteral.add_parse_action(rif_container.literal._parse)

ANGLEBRACKIRI = IRIREF
CURIE = PNAME_LN | PNAME_NS
#UNICODESTRING = # something like "asdf" not "'asdf'"
#_CONSTSHORT_WTIH_LANGTAG = "asdf"@en
_CONSTRSHORT_WITH_LANGTAG = pp.Combine(String + LANGTAG)
_CONSTRSHORT_WITH_LANGTAG.set_parse_action(rif_container.Const_withlang._parse)
CONSTSHORT = iri | NumericLiteral | localiri | literal#_CONSTRSHORT_WITH_LANGTAG
"""
:TODO: Im not sure why '_'. It might be representative for local iris(BNode).
"""
NCName = VARNAME

_IRI = pp.Regex(r'[^<>"{}|^`\\%s]*'
                % "".join("\\x%02X" % i for i in range(33)))
IRICONST = pp.Combine(pp.Suppress("\"") + _IRI + pp.Suppress("\""))\
        | pp.Combine(pp.Suppress("\'") + _IRI + pp.Suppress("\'"))
""" copied from rdflib.plugins.sparql.parser.IRIREF"""
Const = CONSTSHORT

IRIMETA = pp.Forward()
Base = pp.Forward()
Prefix = pp.Forward()
Import = pp.Forward()
Group = pp.Forward()
Name = NCName
LOCATOR = pp.Forward()
PROFILE = pp.Forward()
Strategy = pp.Forward()
Priority = pp.Forward()
RULE = pp.Forward()
Var = pp.Forward()
Implies_PRD = pp.Forward()
Implies_Core = pp.Forward()
ACTION_BLOCK = pp.Forward()
Assert = pp.Forward()
Retract = pp.Forward()
Modify = pp.Forward()
Execute = pp.Forward()
Atom = pp.Forward()
Frame = pp.Forward()
Member = pp.Forward()

List = pp.Forward()
External_term = pp.Forward()
TERM = Const | Var | List | External_term

NEGATEDFORMULA = pp.Forward()
Equal = pp.Forward()
Subclass = pp.Forward()
GROUNDTERM = pp.Forward()
Expr = pp.Forward()
External_formula = pp.Forward()
New = pp.Optional(IRIMETA) + pp.Suppress("New") - pp.Suppress("(")\
        + pp.Suppress(")")
New.set_parse_action(rif_container.New)
FORMULA = pp.Forward()

#Document       ::= IRIMETA? 'Document' '(' Base? Prefix* Import* Group? ')'
Document = pp.Optional(IRIMETA).set_results_name("Meta")\
        + pp.Suppress('Document')\
        - pp.Suppress('(')\
        + pp.Optional(Base).set_results_name("Base")\
        + pp.ZeroOrMore(Prefix).set_results_name("Prefixes")\
        + pp.ZeroOrMore(Import).set_results_name("directive")\
        + pp.Optional(Group).set_results_name("payload")\
        + pp.Suppress(')')
Document.set_parse_action(rif_container.Document._parse)
#Base           ::= 'Base' '(' ANGLEBRACKIRI ')'
Base << pp.Suppress('Base') - pp.Suppress('(') - ANGLEBRACKIRI\
        - pp.Suppress(')')
#Prefix         ::= 'Prefix' '(' Name ANGLEBRACKIRI ')'
Prefix << pp.Suppress('Prefix') - pp.Suppress('(')\
        - Name - ANGLEBRACKIRI  - pp.Suppress(')')
Prefix.set_parse_action( lambda x: tuple(x) )
#Import         ::= IRIMETA? 'Import' '(' LOCATOR PROFILE? ')'
Import << pp.Optional(IRIMETA) + pp.Suppress('Import') + pp.Suppress('(')\
        + LOCATOR.set_results_name("Location")\
        + pp.Optional(PROFILE).set_results_name("Profile") + pp.Suppress(')')
Import.set_parse_action(rif_container.Import._parse)
#Group          ::= IRIMETA? 'Group' Strategy? Priority? '(' (RULE | Group)* ')'
Group <<= pp.Optional(IRIMETA) + pp.Suppress('Group')\
        + pp.Optional(Strategy).set_results_name("Strategy")\
        + pp.Optional(Priority).set_results_name("Priority")\
        - pp.Suppress('(')\
        - pp.ZeroOrMore(RULE | Group).set_results_name("sentence")\
        - pp.Suppress(')')
Group.set_parse_action(rif_container.Group._parse)
#Strategy       ::= Const
Strategy << Const
#Priority       ::= Const
Priority << Const
Forall = pp.Optional(IRIMETA) + pp.Suppress('Forall')\
        - (pp.OneOrMore(Var).set_results_name("declare")\
        - pp.Optional(pp.Suppress('such that')\
        - pp.OneOrMore(FORMULA)).set_results_name("pattern")\
        - pp.Suppress('(')\
        - RULE.set_results_name("formula")\
        - pp.Suppress(')'))
Forall.set_parse_action(rif_container.Forall._parse)
RULE << pp.MatchFirst((Forall, Implies_PRD, Implies_Core, ACTION_BLOCK))

#Implies_PRD        ::= IRIMETA? 'If' FORMULA 'Then' ACTION_BLOCK
Implies_PRD << pp.Optional(IRIMETA) + pp.Suppress('If')\
        - FORMULA.set_results_name("Formula") - pp.Suppress('Then')\
        - ACTION_BLOCK.set_results_name("Actionblock")
Implies_PRD.set_parse_action(rif_container.Implies._parse)
Implies_Core << pp.Optional(IRIMETA)\
        + ACTION_BLOCK.set_results_name("Actionblock") + pp.Suppress(':-')\
        - FORMULA.set_results_name("Formula")
Implies_Core.set_parse_action(rif_container.Implies._parse)
#LOCATOR        ::= ANGLEBRACKIRI
LOCATOR << ANGLEBRACKIRI
#PROFILE        ::= ANGLEBRACKIRI
PROFILE << ANGLEBRACKIRI

#Action Language:

#ACTION  ::= IRIMETA? (Assert | Retract | Modify | Execute )
ACTION = Assert | Retract | Modify | Execute
#Assert         ::= 'Assert' '(' IRIMETA? (Atom | Frame | Member) ')'
Assert << pp.Optional(IRIMETA) + pp.Suppress('Assert') - pp.Suppress('(')\
        - (Atom | Frame | Member ) - pp.Suppress(')')
Assert.set_parse_action(rif_container.Assert._parse)
#Retract        ::= 'Retract' '(' ( IRIMETA? (Atom | Frame) | TERM | TERM TERM ) ')'
Retract << pp.Optional(IRIMETA) + pp.Suppress('Retract') - pp.Suppress('(')\
        - pp.MatchFirst([Atom,
                         Frame,
                         TERM + TERM,
                         TERM,
                         _exc_retract.raise_if(pp.Regex(".+")),
                         ]).set_results_name("Target") - pp.Suppress(')')
Retract.set_parse_action(rif_container.Retract._parse)
#Modify         ::= 'Modify'  '(' IRIMETA? Frame ')'
Modify << pp.Optional(IRIMETA) + pp.Suppress('Modify') - pp.Suppress('(')\
        - (Frame | _exc_modify.raise_if(pp.Regex(".+")))\
        .set_results_name("Target")\
        - pp.Suppress(')')
Modify.set_parse_action(rif_container.Modify._parse)
#Execute        ::= 'Execute' '(' IRIMETA? Atom ')' 
Execute << pp.Optional(IRIMETA) + pp.Suppress('Execute') - pp.Suppress('(')\
        - Atom.set_results_name("Target") - pp.Suppress(')')
Execute.set_parse_action(rif_container.Execute._parse)
#Execute.set_parse_action(rif_container.notImpl)
#ACTION_BLOCK   ::= IRIMETA? ('Do (' ('(' IRIMETA? Var IRIMETA? (Frame | 'New()') ')')* ACTION+  ')' |
#                 'And (' ( IRIMETA? (Atom | Frame) )* ')' | Atom | Frame)
_VAR_INIT_SLOT = pp.Suppress('(') + Var + (Frame | New)\
        + pp.Suppress(')')
_VAR_INIT_SLOT.set_parse_action(rif_container.Var_init_slot._parse)
_DO_ACTION = pp.Optional(IRIMETA) + pp.Suppress("Do") + pp.Suppress("(")\
        - pp.ZeroOrMore(_VAR_INIT_SLOT).set_results_name("Vars")\
        - pp.OneOrMore(ACTION).set_results_name("Actions")\
        + pp.Suppress(')')
_DO_ACTION.set_parse_action(rif_container.Do_action._parse)
"""
:TODO: Why is irimeta possible before atom and frame in this context
"""
_AND_ACTION = pp.Optional(IRIMETA) + pp.Suppress('And') - pp.Suppress('(')\
        - pp.ZeroOrMore((Atom | Frame))\
        - pp.Suppress(')')
ACTION_BLOCK << pp.MatchFirst((_DO_ACTION, _AND_ACTION, Atom, Frame))

##Condition Language:

And_formula = pp.Optional(IRIMETA) + pp.Suppress('And') - pp.Suppress('(')\
        - pp.ZeroOrMore(FORMULA).set_results_name("Formulas")\
        - pp.Suppress(')')
And_formula.set_parse_action(rif_container.And_formula._parse)
Or = pp.Optional(IRIMETA) + pp.Suppress('Or') + pp.Suppress('(')\
        + pp.ZeroOrMore(FORMULA).set_results_name("Formulas")\
        + pp.Suppress(')')
Or.set_parse_action(rif_container.Or_formula._parse)
Exists = pp.Optional(IRIMETA) + pp.Suppress('Exists')\
        + pp.OneOrMore(Var).set_results_name("Vars")\
        + pp.Suppress('(') + FORMULA.set_results_name("Formulas")\
        + pp.Suppress(')')
Exists.set_parse_action(rif_container.Exists._parse)
External_formula << pp.Optional(IRIMETA) + pp.Suppress('External')\
        + pp.Suppress('(')\
        + Atom\
        + pp.Suppress(')')
External_formula.set_parse_action(rif_container.External_formula._parse)
#UNITERM        ::= (IRIMETA? Const) '(' (TERM* ')'
UNITERM = pp.Optional(IRIMETA) + Const.set_results_name("Op")\
        + pp.Suppress('(') + pp.ZeroOrMore(TERM).set_results_name("Args")\
        + pp.Suppress(')')
"""
:TODO: This might be incorrect. For :term:`external defined atomic formulas`
    this is correct, but for :term:`atomic formulas` it might be valid
    with exactly one arg
"""
#Atom           ::= UNITERM
Atom << UNITERM
Atom.set_parse_action(rif_container.Atom._parse)
Expr << pp.Optional(IRIMETA) + _external_iri.set_results_name("Op")\
        - pp.Suppress('(') - pp.ZeroOrMore(TERM).set_results_name("Args")\
        - pp.Suppress(')')
Expr.set_parse_action(rif_container.Expr._parse)
#GROUNDUNITERM  ::= (IRIMETA? Const) '(' (GROUNDTERM* ')'
GROUNDUNITERM = (pp.Optional(IRIMETA) + Const) + pp.Suppress('(')\
        + pp.ZeroOrMore(GROUNDTERM) + pp.Suppress(')')
#NEGATEDFORMULA ::= 'Not' '(' FORMULA ')' | 'INeg' '(' FORMULA ')' 
NEGATEDFORMULA << pp.Optional(IRIMETA) + pp.Suppress(pp.oneOf('Not', 'INEG'))\
        - pp.Suppress('(') - FORMULA.set_results_name("Formula")\
        - pp.Suppress(')')
NEGATEDFORMULA.set_parse_action(rif_container.negatedformula._parse)
#Equal          ::= TERM '=' TERM
Equal = pp.Optional(IRIMETA) + TERM.set_results_name("Left")\
        + pp.Suppress('=') - TERM.set_results_name("Right")
Equal.set_parse_action(rif_container.Equal._parse)
#Member         ::= TERM '#' TERM
Member << pp.Optional(IRIMETA) + TERM.set_results_name("instance")\
        + pp.Suppress('#') - TERM.set_results_name("class")
Member.set_parse_action(rif_container.Member._parse)
#Subclass       ::= TERM '##' TERM
Subclass << pp.Optional(IRIMETA) + TERM.set_results_name("sub")\
        + pp.Suppress('##') - TERM.set_results_name("super")
Subclass.set_parse_action(rif_container.Subclass._parse)
#Frame          ::= TERM '[' (TERM '->' TERM)* ']'
_Frame_slot = TERM + pp.Suppress('->') + TERM
_Frame_slot.set_parse_action(rif_container.Slot._parse)
Frame << pp.Optional(IRIMETA) + TERM.set_results_name("object")\
        + pp.Suppress('[')\
        - pp.OneOrMore(_Frame_slot).set_results_name("slot")\
        - pp.Suppress(']')
Frame.set_parse_action(rif_container.Frame._parse)
Frame_nometa = TERM.set_results_name("object")\
        + pp.Suppress('[')\
        - pp.OneOrMore(_Frame_slot).set_results_name("slot")\
        - pp.Suppress(']')
Frame_nometa.set_parse_action(rif_container.Frame._parse)
#TERM           ::= IRIMETA? (Const | Var | List | 'External' '(' Expr ')')
External_term <<= pp.Optional(IRIMETA) + pp.Suppress('External')\
        - pp.Suppress('(')\
        - Expr\
        - pp.Suppress(')')
External_term.set_parse_action(rif_container.External_formula._parse)
External_groundterm = pp.Optional(IRIMETA) + pp.Suppress('External')\
        - pp.Suppress('(')\
        - GROUNDUNITERM.set_results_name("Content")\
        - pp.Suppress(')')
GROUNDTERM = Const | List | External_groundterm
List << pp.Optional(IRIMETA) + pp.Suppress('List') - pp.Suppress('(')\
        - pp.ZeroOrMore(GROUNDTERM).set_results_name("Items")\
        - pp.Suppress(')')
List.set_parse_action(rif_container.List._parse)
Var << pp.Optional(IRIMETA)\
        + pp.Combine(pp.Suppress('?') - Name.set_results_name("text"))
Var.set_parse_action(rif_container.Var._parse)
localiri <<= pp.Combine(pp.Suppress('_') - Name.set_results_name("text"))
localiri.set_parse_action(rif_container.LocalIri._parse)

##Annotations:

#IRIMETA        ::= '(*' IRICONST? (Frame | 'And' '(' Frame* ')')? '*)'
And_frame = pp.Suppress('And') - pp.Suppress('(') - pp.ZeroOrMore(Frame)\
        - pp.Suppress(')')
IRIMETA << pp.Suppress('(*')\
        + pp.Optional(iri).set_results_name("iri")\
        + pp.Optional(pp.MatchFirst((And_frame, Frame_nometa))).set_results_name("config")\
        - pp.Suppress('*)')
"""
:TODO: needed to replace iriconst with rdflib...iri . Please check
:TODO: This doesnt work (* ex:frame[] *)
"""

RIFPRD_PS = Document | Group | Forall | Implies_PRD | Implies_Core\
        | Assert | Retract\
        | Modify | And_formula | Exists\
        | Equal\
        | Subclass | Atom | Frame | Member
"""This should contain all possible Things with metadata. It is used, when
parsing arbitrary data in RIFPRD-PS.
"""

FORMULA <<= pp.MatchFirst((And_formula,
                          Or,
                          Exists,
                          NEGATEDFORMULA,
                          Equal,
                          External_formula,
                          Atom,
                          Frame,
                          Member,
                          Subclass,
                          ))
