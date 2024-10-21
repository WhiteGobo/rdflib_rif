import typing as typ
from . import ebnf
import pyparsing as pp

def parse_rifpl(stream: typ.Union[str]):
    #q = ebnf.Document.parse_file(stream)[0]

    try:
        q = ebnf.RIFPRD_PS.parseString(stream)[0]
    except (pp.ParseException, pp.ParseSyntaxException) as err:
        raise Exception("\n"+err.explain()) from err
    return q
