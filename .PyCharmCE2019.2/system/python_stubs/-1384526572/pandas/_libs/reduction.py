# encoding: utf-8
# module pandas._libs.reduction
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pandas\_libs\reduction.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
from pandas._libs.lib import maybe_convert_objects

import distutils.version as __distutils_version


# functions

def apply_frame_axis0(*args, **kwargs): # real signature unknown
    pass

def reduce(*args, **kwargs): # real signature unknown
    """
    Parameters
        -----------
        arr : NDFrame object
        f : function
        axis : integer axis
        dummy : type of reduced output (series)
        labels : Index or None
    """
    pass

def __pyx_unpickle_Reducer(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_SeriesBinGrouper(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_SeriesGrouper(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Slider(*args, **kwargs): # real signature unknown
    pass

# classes

class BlockSlider(object):
    """ Only capable of sliding on axis=0 """
    def move(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    blocks = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    dummy = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    frame = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    idx_slider = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    index = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    nblocks = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0ACFE3F8>'


class InvalidApply(Exception):
    # no doc
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    __weakref__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """list of weak references to the object (if defined)"""



class LooseVersion(__distutils_version.Version):
    """
    Version numbering for anarchists and software realists.
        Implements the standard interface for version number classes as
        described above.  A version number consists of a series of numbers,
        separated by either periods or strings of letters.  When comparing
        version numbers, the numeric components will be compared
        numerically, and the alphabetic components lexically.  The following
        are all valid version numbers, in no particular order:
    
            1.5.1
            1.5.2b2
            161
            3.10a
            8.02
            3.4j
            1996.07.12
            3.2.pl0
            3.1.1.6
            2g6
            11g
            0.960923
            2.2beta29
            1.13++
            5.5.kw
            2.0b1pl0
    
        In fact, there is no such thing as an invalid version number under
        this scheme; the rules for comparison are simple and predictable,
        but may not always give the results you want (for some definition
        of "want").
    """
    def parse(self, vstring): # reliably restored by inspect
        # no doc
        pass

    def _cmp(self, other): # reliably restored by inspect
        # no doc
        pass

    def __init__(self, vstring=None): # reliably restored by inspect
        # no doc
        pass

    def __repr__(self): # reliably restored by inspect
        # no doc
        pass

    def __str__(self): # reliably restored by inspect
        # no doc
        pass

    component_re = re.compile('(\\d+ | [a-z]+ | \\.)', re.VERBOSE)


class Reducer(object):
    """
    Performs generic reduction operation on a C or Fortran-contiguous ndarray
        while avoiding ndarray construction overhead
    """
    def get_result(self, *args, **kwargs): # real signature unknown
        pass

    def _check_dummy(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass


class SeriesBinGrouper(object):
    """ Performs grouping operation according to bin edges, rather than labels """
    def get_result(self, *args, **kwargs): # real signature unknown
        pass

    def _check_dummy(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    arr = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    bins = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    dummy_arr = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    dummy_index = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    f = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    index = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    ityp = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    name = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    typ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    values = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default



class SeriesGrouper(object):
    """
    Performs generic grouping operation while avoiding ndarray construction
        overhead
    """
    def get_result(self, *args, **kwargs): # real signature unknown
        pass

    def _check_dummy(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    arr = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    dummy_arr = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    dummy_index = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    f = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    index = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    ityp = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    labels = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    name = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    typ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    values = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default



class Slider(object):
    """ Only handles contiguous data for now """
    def advance(self, *args, **kwargs): # real signature unknown
        pass

    def reset(self, *args, **kwargs): # real signature unknown
        pass

    def set_length(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0ACFE3E0>'


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0AD039D0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pandas._libs.reduction', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0AD039D0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pandas\\\\_libs\\\\reduction.cp37-win32.pyd')"

__test__ = {}

