import typing as typ
from . import ebnf

def parse_rifpl(stream: typ.Union[str]):
    #q = ebnf.Document.parse_file(stream)[0]

    q = ebnf.RIFPRD_PS.parseString(stream)[0]
    return q
