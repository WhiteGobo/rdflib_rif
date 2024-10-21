"""Pyparsing for EBNF of rif prd markup language.

:TODO: ATOMIC(from documentation `https://www.w3.org/TR/2013/REC-rif-core-20130205/`_)
    and ACTION_BLOCK seems to be the same. More documentation needed.
"""

import pyparsing as pp
from pyparsing import pyparsing_common as _pp_common
import abc
import typing as typ
from . import container as rif_container

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
iri.set_name("iri")
iri.add_parse_action(rif_container.Const_shortenediri._parse)
localiri = pp.Forward()
localiri.set_name("localiri")

External_iri = _rdflib_sparql_parser.iri.copy()
External_iri.add_parse_action(rif_container.External_Const_shortenediri._parse)
External_iri.set_name("external_iri")
literal = _rdflib_sparql_parser.RDFLiteral.copy()
literal.set_name("Literal")
literal.add_parse_action(rif_container.literal._parse)
#literal.add_parse_action(rif_container.Const_withlang._parse)
NumericLiteral = _rdflib_sparql_parser.NumericLiteral.copy()
NumericLiteral.add_parse_action(rif_container.literal._parse)
NumericLiteral.set_name("NumericLiteral")

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
NCName = VARNAME.copy().set_name("NCName")

_IRI = pp.Regex(r'[^<>"{}|^`\\%s]*'
                % "".join("\\x%02X" % i for i in range(33)))
IRICONST = pp.Combine(_suppr("\"") + _IRI + _suppr("\""))\
        | pp.Combine(_suppr("\'") + _IRI + _suppr("\'"))
IRICONST.set_name("IRI")
""" copied from rdflib.plugins.sparql.parser.IRIREF"""
Const = CONSTSHORT.copy().set_name("Const")

IRIMETA = pp.Forward()
IRIMETA.set_name("IRIMETA ::= '(*' iri? {And_frame | Frame_nometa}?'*)'")
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
Atomic = pp.Forward()
Var = pp.Forward()
Var.set_name("Var")
Forall = pp.Forward().set_name("Forall")
Implies_PRD = pp.Forward()
Implies_PRD.set_name("Implies(PRD)")
Implies_Core = pp.Forward()
Implies_Core.set_name("Implies(Core)")
ACTION_BLOCK = pp.Forward()
ATOMIC = pp.Forward()
ATOMIC.set_name("ATOMIC")

Assert = pp.Forward()
Retract = pp.Forward()
Modify = pp.Forward()
Execute = pp.Forward()
Frame = pp.Forward()
Member = pp.Forward()
Member.set_name("Member")

List = pp.Forward()
External_term = pp.Forward()
Expr = pp.Forward()
Expr.set_name("Expr ::= IRIMETA? External_iri '(' TERM* ')'")
TERM = pp.MatchFirst((Expr, Const, Var, List, External_term))
TERM.set_name("TERM ::= Expr | Const | Var | List | External_term")

# UNITERM        ::= (IRIMETA? Const) '(' (TERM* ')'
#UNITERM = pp.Optional(IRIMETA) + Const.set_results_name("Op")\
#        + _suppr('(') + (pp.ZeroOrMore(named_arg) | pp.ZeroOrMore(TERM).set_results_name("Args"))\
#        + _suppr(')')
#UNITERM.set_name("Const (TERM* | {Name -> TERM}*)")
Atom_singles = pp.Optional(IRIMETA) + Const.set_results_name("Op")\
        + _suppr('(') + pp.ZeroOrMore(TERM).set_results_name("Args")\
        + _suppr(')')
named_arg = Name + _suppr("->") + TERM
named_arg.set_parse_action(rif_container.Slot._parse)
Atom_slots = pp.Optional(IRIMETA) + Const.set_results_name("Op")\
        + _suppr('(') + pp.ZeroOrMore(named_arg).set_results_name("slot")\
        + _suppr(')')

Atom = Atom_singles ^ Atom_slots
Atom.set_name("Atom")
Atom_singles.set_parse_action(rif_container.Atom_singles._parse)
Atom_slots.set_parse_action(rif_container.Atom_slots._parse)

NEGATEDFORMULA = pp.Forward()
NEGATEDFORMULA.set_name("NEGATEDFORMULA")
Equal = pp.Forward()
Equal.set_name("Equal")
Subclass = pp.Forward()
GROUNDTERM = pp.Forward()
External_formula = pp.Forward()
External_formula.set_name("External_formula")
New = pp.Optional(IRIMETA) + _suppr("New") - _suppr("(")\
        + _suppr(")")
New.set_parse_action(rif_container.New)
FORMULA = pp.Forward()

# Document       ::= IRIMETA? 'Document' '(' Base? Prefix* Import* Group? ')'
Document = pp.Optional(IRIMETA).set_results_name("Meta")\
        + _suppr('Document')\
        - _suppr('(')\
        + pp.Optional(Base).set_results_name("Base")\
        + pp.ZeroOrMore(Prefix).set_results_name("Prefixes")\
        + pp.ZeroOrMore(Import).set_results_name("directive")\
        + pp.Optional(Group).set_results_name("payload")\
        + _suppr(')')
Document.set_parse_action(rif_container.Document._parse)

# Base           ::= 'Base' '(' ANGLEBRACKIRI ')'
Base <<= _suppr('Base') - _suppr('(') - ANGLEBRACKIRI\
        - _suppr(')')

# Prefix         ::= 'Prefix' '(' Name ANGLEBRACKIRI ')'
Prefix <<= _suppr('Prefix') - _suppr('(')\
        - Name - ANGLEBRACKIRI  - _suppr(')')
Prefix.set_parse_action( lambda x: tuple(x) )

# Import ::= IRIMETA? 'Import' '(' LOCATOR PROFILE? ')'
Import <<= pp.Optional(IRIMETA) + _suppr('Import') + _suppr('(')\
        + LOCATOR.set_results_name("Location")\
        + pp.Optional(PROFILE).set_results_name("Profile") + _suppr(')')
Import.set_parse_action(rif_container.Import._parse)

# Group ::= IRIMETA? 'Group' Strategy? Priority? '(' (RULE | Group)* ')'
Group <<= pp.Optional(IRIMETA) + _suppr('Group')\
        + pp.Optional(Strategy).set_results_name("Strategy")\
        + pp.Optional(Priority).set_results_name("Priority")\
        - _suppr('(')\
        - pp.ZeroOrMore(RULE | Group).set_results_name("sentence")\
        - (_suppr(')') | _exc_endofgroup.raise_if(pp.Regex("[^)]+")))
Group.set_parse_action(rif_container.Group._parse)

# Strategy ::= Const
Strategy <<= Const

#Priority       ::= Const
Priority <<= Const
such_that_FORMULA = _suppr('such that') - pp.OneOrMore(FORMULA)
Forall <<= pp.Optional(IRIMETA) + _suppr('Forall')\
        - (pp.OneOrMore(Var).set_results_name("declare")\
        + pp.Optional(such_that_FORMULA).set_results_name("pattern")\
        + _suppr('(')\
        - RULE.set_results_name("formula")\
        + _suppr(')'))
Forall.set_parse_action(rif_container.Forall._parse)
RULE <<= pp.MatchFirst((Forall, Implies_PRD, Implies_Core, ACTION_BLOCK, ATOMIC))

#Implies_PRD        ::= IRIMETA? 'If' FORMULA 'Then' ACTION_BLOCK
Implies_PRD <<= pp.Optional(IRIMETA) + _suppr('If')\
        - FORMULA.set_results_name("Formula") - _suppr('Then')\
        - ACTION_BLOCK.set_results_name("Actionblock")
Implies_PRD.set_parse_action(rif_container.Implies._parse)

#Implies_Core ::= IRIMETA? (ATOMIC | 'And' '(' ATOMIC* ')') ':-' FORMULA
Implies_Core <<= pp.Optional(IRIMETA)\
        + ACTION_BLOCK.set_results_name("Actionblock")\
        + _suppr(':-')\
        - FORMULA.set_results_name("Formula")

Implies_Core.set_parse_action(rif_container.Implies._parse)
#LOCATOR        ::= ANGLEBRACKIRI
LOCATOR <<= ANGLEBRACKIRI
#PROFILE        ::= ANGLEBRACKIRI
PROFILE <<= ANGLEBRACKIRI

#Action Language:

#ACTION  ::= IRIMETA? (Assert | Retract | Modify | Execute )
ACTION = Assert | Retract | Modify | Execute

# Assert ::= 'Assert' '(' IRIMETA? (Atom | Frame | Member) ')'
Assert <<= pp.Optional(IRIMETA) + _suppr('Assert') - _suppr('(')\
        - (Atom | Frame | Member ) - _suppr(')')
Assert.set_parse_action(rif_container.Assert._parse)

# Retract ::= 'Retract' '(' ( IRIMETA? (Atom | Frame) | TERM | TERM TERM ) ')'
Retract <<= pp.Optional(IRIMETA) + _suppr('Retract') - _suppr('(')\
        - pp.MatchFirst([Atom,
                         Frame,
                         TERM + TERM,
                         TERM,
                         _exc_retract.raise_if(pp.Regex(".+")),
                         ]).set_results_name("Target") - _suppr(')')
Retract.set_parse_action(rif_container.Retract._parse)

#Modify         ::= 'Modify'  '(' IRIMETA? Frame ')'
Modify <<= pp.Optional(IRIMETA) + _suppr('Modify') - _suppr('(')\
        - (Frame | _exc_modify.raise_if(pp.Regex(".+")))\
        .set_results_name("Target")\
        - _suppr(')')
Modify.set_parse_action(rif_container.Modify._parse)
#Execute        ::= 'Execute' '(' IRIMETA? Atom ')' 
Execute <<= pp.Optional(IRIMETA) + _suppr('Execute') - _suppr('(')\
        - Atom.set_results_name("Target") - _suppr(')')
Execute.set_parse_action(rif_container.Execute._parse)
#Execute.set_parse_action(rif_container.notImpl)
#ACTION_BLOCK   ::= IRIMETA? ('Do (' ('(' IRIMETA? Var IRIMETA? (Frame | 'New()') ')')* ACTION+  ')' |
#                 'And (' ( IRIMETA? (Atom | Frame) )* ')' | Atom | Frame)
_VAR_INIT_SLOT = _suppr('(') + Var + (Frame | New)\
        + _suppr(')')
_VAR_INIT_SLOT.set_parse_action(rif_container.Var_init_slot._parse)
_DO_ACTION = pp.Optional(IRIMETA) + _suppr("Do") + _suppr("(")\
        - pp.ZeroOrMore(_VAR_INIT_SLOT).set_results_name("Vars")\
        - pp.OneOrMore(ACTION).set_results_name("Actions")\
        + _suppr(')')
_DO_ACTION.set_parse_action(rif_container.Do_action._parse)
_DO_ACTION.set_name("DO")

"""
:TODO: Why is irimeta possible before atom and frame in this context
"""
_AND_ACTION = pp.Optional(IRIMETA) + _suppr('And') - _suppr('(')\
        - pp.ZeroOrMore((Atom | Frame))\
        - _suppr(')')
ACTION_BLOCK <<= pp.MatchFirst((_DO_ACTION, _AND_ACTION, ATOMIC, Atom, Frame))
ACTION_BLOCK.set_name("ACTION_BLOCK")

# ATOMIC ::= IRIMETA? (Atom | Equal | Member | Subclass | Frame)
ATOMIC <<= pp.Optional(IRIMETA)\
        + pp.Or((Equal, Subclass | Member, Frame, Atom))

# Condition Language:

And_formula = pp.Optional(IRIMETA) + _suppr('And') - _suppr('(')\
        - pp.ZeroOrMore(FORMULA).set_results_name("Formulas")\
        - _suppr(')')
And_formula.set_parse_action(rif_container.And_formula._parse)
And_formula.set_name("And(formula)")
Or = pp.Optional(IRIMETA) + _suppr('Or') + _suppr('(')\
        + pp.ZeroOrMore(FORMULA).set_results_name("Formulas")\
        + _suppr(')')
Or.set_parse_action(rif_container.Or_formula._parse)
Or.set_name("Or")
Exists = pp.Optional(IRIMETA) + _suppr('Exists')\
        + pp.OneOrMore(Var).set_results_name("Vars")\
        + _suppr('(') + FORMULA.set_results_name("Formulas")\
        + _suppr(')')
Exists.set_parse_action(rif_container.Exists._parse)
Exists.set_name("Exists")
External_formula <<= pp.Optional(IRIMETA) + _suppr('External')\
        + _suppr('(')\
        + Atom\
        + _suppr(')')
External_formula.set_parse_action(rif_container.External_formula._parse)
"""
:TODO: This might be incorrect. For :term:`external defined atomic formulas`
    this is correct, but for :term:`atomic formulas` it might be valid
    with exactly one arg
"""
Expr <<= pp.Optional(IRIMETA) + External_iri.set_results_name("Op")\
        + _suppr('(') - pp.ZeroOrMore(TERM).set_results_name("Args")\
        - _suppr(')')
Expr.set_parse_action(rif_container.Expr._parse)

#GROUNDUNITERM  ::= (IRIMETA? Const) '(' (GROUNDTERM* ')'
GROUNDUNITERM = (pp.Optional(IRIMETA) + Const) + _suppr('(')\
        + pp.ZeroOrMore(GROUNDTERM) + _suppr(')')

#NEGATEDFORMULA ::= 'Not' '(' FORMULA ')' | 'INeg' '(' FORMULA ')' 
NEGATEDFORMULA <<= pp.Optional(IRIMETA) + _suppr(pp.oneOf('Not', 'INEG'))\
        - _suppr('(') - FORMULA.set_results_name("Formula")\
        - _suppr(')')
NEGATEDFORMULA.set_parse_action(rif_container.negatedformula._parse)

#Equal          ::= TERM '=' TERM
Equal <<= pp.Optional(IRIMETA) + TERM.set_results_name("Left") \
        + _suppr('=') - TERM.set_results_name("Right")
Equal.set_parse_action(rif_container.Equal._parse)

#Member         ::= TERM '#' TERM
Member <<= pp.Optional(IRIMETA) + TERM.set_results_name("instance")\
        + _suppr('#') - TERM.set_results_name("class")
Member.set_parse_action(rif_container.Member._parse)

#Subclass       ::= TERM '##' TERM
Subclass <<= pp.Optional(IRIMETA) + TERM.set_results_name("sub")\
        + _suppr('##') - TERM.set_results_name("super")
Subclass.set_parse_action(rif_container.Subclass._parse)
Subclass.set_name("Subclass")

#Frame          ::= TERM '[' (TERM '->' TERM)* ']'
_Frame_slot = TERM + _suppr('->') + TERM
_Frame_slot.set_parse_action(rif_container.Slot._parse)
Frame <<= pp.Optional(IRIMETA) + TERM.set_results_name("object")\
        + _suppr('[')\
        - pp.OneOrMore(_Frame_slot).set_results_name("slot")\
        - _suppr(']')
Frame.set_parse_action(rif_container.Frame._parse)
Frame.set_name("Frame")
Frame_nometa = TERM.set_results_name("object")\
        + _suppr('[')\
        - pp.OneOrMore(_Frame_slot).set_results_name("slot")\
        - _suppr(']')
Frame_nometa.set_parse_action(rif_container.Frame._parse)

#TERM           ::= IRIMETA? (Const | Var | List | 'External' '(' Expr ')')
External_term <<= pp.Optional(IRIMETA) + pp.MatchFirst((Const, Var, List, _suppr('External')))\
        + _suppr('(')\
        - Expr\
        - _suppr(')')
External_term.set_parse_action(rif_container.External_formula._parse)
External_groundterm = pp.Optional(IRIMETA) + _suppr('External')\
        - _suppr('(')\
        - GROUNDUNITERM.set_results_name("Content")\
        - _suppr(')')
GROUNDTERM = Const | List | External_groundterm
List <<= pp.Optional(IRIMETA) + _suppr('List') - _suppr('(')\
        - pp.ZeroOrMore(GROUNDTERM).set_results_name("Items")\
        - _suppr(')')
List.set_parse_action(rif_container.List._parse)
List.set_name("List ::= IRIMETA? 'List(' GROUNDTERM* ')'")
Var <<= pp.Optional(IRIMETA)\
        + pp.Combine(_suppr('?') - Name.set_results_name("text"))
Var.set_parse_action(rif_container.Var._parse)

localiri <<= pp.Combine(_suppr('_') - Name.set_results_name("text"))
localiri.set_parse_action(rif_container.LocalIri._parse)

##Annotations:

#IRIMETA        ::= '(*' IRICONST? (Frame | 'And' '(' Frame* ')')? '*)'
And_frame = _suppr('And') - _suppr('(') - pp.ZeroOrMore(Frame)\
        - _suppr(')')
And_frame.set_name("And(frame)")
IRIMETA <<= _suppr('(*')\
        + pp.Optional(iri).set_results_name("iri")\
        + pp.Optional(pp.MatchFirst((And_frame, Frame_nometa))).set_results_name("config")\
        - _suppr('*)')
"""
:TODO: needed to replace iriconst with rdflib...iri . Please check
:TODO: This doesnt work (* ex:frame[] *)
"""

RIFPRD_PS = Document | Group | Forall | Implies_PRD | Implies_Core\
        | Assert | Retract\
        | Modify | And_formula | Exists\
        | Equal\
        | Subclass | Atom | Frame | Member\
        | _exc_rifprd.raise_if(pp.Regex(".+"))
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


"""Enable messaging for debugging.
Traces Matcher.
"""
def set_rifps_debug():
    for x in [
        Assert, Retract, Modify, Execute, Frame, Member,
        TERM, Atom, Subclass, External_formula, New,
        GROUNDTERM,
        IRIMETA, iri, And_frame, Frame_nometa,
        External_iri,
        Expr, Const, Var, List, External_term,
        NEGATEDFORMULA, FORMULA,
        Equal,
        Document, Prefix, Import, Group, Strategy,
        Forall, Implies_Core, Implies_PRD,
        ]:
        x.set_debug()
