from os import getcwd
from os.path import join, split

path, init_file = split(__file__)
_tmp = "DoNew%s"
format_to_file = {
        #"rif": join(path, _tmp % ".rif"),
        "rifps": join(path, _tmp % ".rifps"),
        "ttl": join(path, _tmp % ".ttl"),
        }
