from os import getcwd
from os.path import join, split

path, init_file = split(__file__)
_tmp = "Named_Argument_Uniterms_non-polymorphic-premise%s"
format_to_file = {
        #"ttl": join(path, _tmp % ".ttl"),
        "rif": join(path, _tmp % ".rif"),
        "rifps": join(path, _tmp % ".rifps"),
        }
