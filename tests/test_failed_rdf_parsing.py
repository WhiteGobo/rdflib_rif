from rdflib import Graph
from pytest import xfail
from rdflib_rif import BadSyntax

data="""
<rdf:RDF
    xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
    xmlns:rif="http://www.w3.org/2007/rif#"
    xmlns:xs="http://www.w3.org/2001/XMLSchema#"
    xmlns:ex="http://example.org/example#" > 
  <rdf:Description rdf:about="http://example.org/example#a">
    <ex:p>this is a plain literal</ex:p>
  </rdf:Description>
</rdf:RDF>
"""


def test_failed_RDF_parsing(register_rif_format):
    """Ensure, that RDF/xml are not parsed by mistake
    """
    err = None
    try:
        g = Graph().parse(data=data, format="rif")
    except BadSyntax as err_:
        err = err_
    assert err is not None
    #assert isinstance(err, rdflib.pareser.exception)
