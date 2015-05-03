"""
Faker

It's not a solution to be used if have availability to install python modules.
It manages to provide an alternative for python implemenetations with
multiple calls overridden, decorators not working and real old... like WLST.
In case you are stranded writing code for a messed up old jython embedded WLST,
have a python module and wanna fake calls within the unittests.
Cuz your WLST's python is old, patched up andd overridden.
"""

import os
import sys


fake_map ={}
fake_stack = []


def fake_it(module_funk_name, fake_obj):
    module_name = ".".join(module_funk_name.split(".")[0:-1])
    funk_name = module_funk_name.split(".")[-1]

    module_obj = sys.modules[module_name.split(".")[0]]
    if len(module_name.split(".")) > 1:
        for _sub_module in module_name.split(".")[1:]:
            module_obj = getattr(module_obj, _sub_module)

    if module_name not in fake_map.keys():
        fake_map[module_name] = {}
    fake_map[module_name][funk_name] = getattr(module_obj, funk_name)

    setattr(module_obj, funk_name, fake_obj)


def real_it(module_funk_name):
    module_name = ".".join(module_funk_name.split(".")[0:-1])
    funk_name = module_funk_name.split(".")[-1]

    module_obj = sys.modules[module_name.split(".")[0]]
    if len(module_name.split(".")) > 1:
        for _sub_module in module_name.split(".")[1:]:
            module_obj = getattr(module_obj, _sub_module)

    setattr(module_obj, funk_name, fake_map[module_name][funk_name])


def _fake_call_detail_(call_stack, args, kwargs):
    arg_str = ",".join([str(x) for x in args])
    kwarg_str = ",".join(["%s:%s" % (x, kwargs[x]) for x in kwargs])
    return "%s:%s:%s" % (call_stack, arg_str, kwarg_str)


def _fake_calls_(args, kwargs):
    call_stack = "%s,%s,%s" % (sys._getframe(1).f_code.co_name,
                       sys._getframe(2).f_code.co_name,
                       sys._getframe(3).f_code.co_name)
    return _fake_call_detail_(call_stack, args, kwargs)


def return_pass(*args, **kwargs):
    fake_stack.append(_fake_calls_(args, kwargs))


def return_true(*args, **kwargs):
    fake_stack.append(_fake_calls_(args, kwargs))
    return True


def return_false(*args, **kwargs):
    fake_stack.append(_fake_calls_(args, kwargs))
    return False


def get_fake_call(call_stack, *args, **kwargs):
    entry = _fake_call_detail_(call_stack, args, kwargs)
    fake_counts = fake_stack.count(entry)
    while(fake_stack.count(entry) > 0):
        fake_stack.remove(entry)
    return fake_counts


if __name__ == "__main__":
    print "ERROR: It's not supposed to be used directly."
    sys.exit(1)

