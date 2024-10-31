import setuptools

setuptools.setup(
    name='rdflib_rif',
    version='0.3.0',
    description='Parser for rif as plugin for rdflib',
    long_description="""This module should allow rdflib to load rif.""",
    long_description_content_type="text/markdown",

    # url="https://example.com/rif-parser-rdflib",
    project_urls={
        "Homepage": "https://github.com/WhiteGobo/rdflib_rif",
    },
    
    author='Richard Focke Fechner',
    author_email='richardfechner@posteo.net',

    py_modules=['rdflib_rif'],
    #scripts = ['rif_parser.py',],

    packages=setuptools.find_packages(),
    install_requires=['rdflib'],
    
    # Classifiers allow your Package to be categorized based on functionality
    classifiers = [
        "Programming Language :: Python :: 3",
        "Operating System :: OS Independent",
    ],

    # Entry points speficy, what is the functionability in rdflib
    # Also this sepicifies, how the plugin is reached
    entry_points = {
        'rdf.plugins.parser': [
            'rif = rdflib_rif:RIFXMLParser',
            'rif/xml = rdflib_rif:RIFXMLParser',
            'rifps = rdflib_rif:RIFMarkupParser',
            'RIFPRD-PS = rdflib_rif:RIFMarkupParser',
        ],
    },

    extras_require = {
        'rifxml validation': ['lxml'],
        'test':  ['lxml', 're', 'xml'],
    },
)
