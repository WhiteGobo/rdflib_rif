
from os import getcwd
from os.path import join, split

path, init_file = split(__file__)
_tmp = "RDF_Combination_Constant_Equivalence_3-premise%s"
format_to_file = {
        #"ttl": join(path, _tmp % ".ttl"),
        "rif": join(path, _tmp % ".rif"),
        "rifps": join(path, _tmp % ".rifps"),
        }
