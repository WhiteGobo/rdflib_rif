from .validate_rifxml import rif_schema
from lxml import etree

class InvalidRif(Exception):
    pass

def validate_str(xml_str: str):
    """Validates given str as valid rif-format. Raises Exception on failure
    :raises: InvalidRif
    """
    doc = etree.fromstring(xml_str)
    rif_schema.assertValid(doc)
    return True
