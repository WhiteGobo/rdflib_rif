#!/usr/bin/env python
import lxml
from lxml import etree
from lxml.etree import DocumentInvalid

import importlib.resources
from . import schema
_rif_schema_file = importlib.resources.files(schema).joinpath("PRD.xsd")

with open(_rif_schema_file, "r") as f:
    schema_doc = etree.parse(f)
rif_schema: lxml.etree.XMLSchema = etree.XMLSchema(schema_doc)
"""Validater
"""

def main(xml_filepath):
    with open(xml_filepath, "r") as f:
        doc = etree.parse(f)
    print(rif_schema.validate(doc))
    rif_schema.assertValid(doc)

if __name__ == "__main__":
    raise Exception()
    xml_filepath = parse_input()
    main(xml_filepath)
