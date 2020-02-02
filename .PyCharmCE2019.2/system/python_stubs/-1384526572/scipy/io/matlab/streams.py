# encoding: utf-8
# module scipy.io.matlab.streams
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\io\matlab\streams.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import sys as sys # <module 'sys' (built-in)>
import zlib as zlib # <module 'zlib' (built-in)>

# functions

def make_stream(*args, **kwargs): # real signature unknown
    """ Make stream of correct type for file-like `fobj` """
    pass

def _read_into(*args, **kwargs): # real signature unknown
    pass

def _read_string(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_cStringStream(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_GenericStream(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_ZlibInputStream(*args, **kwargs): # real signature unknown
    pass

# classes

class GenericStream(object):
    # no doc
    def read(self, *args, **kwargs): # real signature unknown
        pass

    def seek(self, *args, **kwargs): # real signature unknown
        pass

    def tell(self, *args, **kwargs): # real signature unknown
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09E6BFB0>'


class cStringStream(GenericStream):
    # no doc
    def seek(self, *args, **kwargs): # real signature unknown
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09E9F020>'


class FileStream(GenericStream):
    # no doc
    def seek(self, *args, **kwargs): # real signature unknown
        pass

    def tell(self, *args, **kwargs): # real signature unknown
        pass

    def __del__(self, *args, **kwargs): # real signature unknown
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09E9F050>'


class ZlibInputStream(GenericStream):
    """
    File-like object uncompressing bytes from a zlib compressed stream.
    
        Parameters
        ----------
        stream : file-like
            Stream to read compressed data from.
        max_length : int
            Maximum number of bytes to read from the stream.
    
        Notes
        -----
        Some matlab files contain zlib streams without valid Z_STREAM_END
        termination.  To get round this, we use the decompressobj object, that
        allows you to decode an incomplete stream.  See discussion at
        https://bugs.python.org/issue8672
    """
    def all_data_read(self, *args, **kwargs): # real signature unknown
        pass

    def read(self, *args, **kwargs): # real signature unknown
        pass

    def seek(self, *args, **kwargs): # real signature unknown
        pass

    def tell(self, *args, **kwargs): # real signature unknown
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09E6BFC8>'


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x09E9AA90>'

__pyx_capi__ = {
    'make_stream': None, # (!) real value is '<capsule object "struct __pyx_obj_5scipy_2io_6matlab_7streams_GenericStream *(PyObject *, int __pyx_skip_dispatch)" at 0x09E6BF50>'
}

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.io.matlab.streams', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x09E9AA90>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\io\\\\matlab\\\\streams.cp37-win32.pyd')"

__test__ = {}

