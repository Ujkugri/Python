# encoding: utf-8
# module pygame.newbuffer
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\newbuffer.cp37-win32.pyd
# by generator 1.147
""" exports BufferMixin, add a new buffer interface to a class """
# no imports

# Variables with simple values

PyBUFFER_SIZEOF = 44

PyBUF_ANY_CONTIGUOUS = 152

PyBUF_CONTIG = 9

PyBUF_CONTIG_RO = 8

PyBUF_C_CONTIGUOUS = 56

PyBUF_FORMAT = 4
PyBUF_FULL = 285

PyBUF_FULL_RO = 284

PyBUF_F_CONTIGUOUS = 88

PyBUF_INDIRECT = 280
PyBUF_ND = 8
PyBUF_RECORDS = 29

PyBUF_RECORDS_RO = 28

PyBUF_SIMPLE = 0
PyBUF_STRIDED = 25

PyBUF_STRIDED_RO = 24

PyBUF_STRIDES = 24
PyBUF_WRITABLE = 1

# no functions
# classes

class BufferMixin(object):
    """ Python level new buffer protocol exporter """
    def _get_buffer(self, *args, **kwargs): # real signature unknown
        """ new buffer protocol default bf_getbuffer handler """
        pass

    def _release_buffer(self, *args, **kwargs): # real signature unknown
        """ new buffer protocol default bf_releasebuffer handler """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass


class Py_buffer(object):
    """ Python level Py_buffer struct wrapper """
    def get_buffer(self, *args, **kwargs): # real signature unknown
        """ fill in Py_buffer fields with a PyObject_GetBuffer call """
        pass

    def release_buffer(self, *args, **kwargs): # real signature unknown
        """ release the Py_buffer with a PyBuffer_Release call """
        pass

    def __bool__(self, *args, **kwargs): # real signature unknown
        """ self != 0 """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    buf = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    format = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    internal = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    itemsize = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    len = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    ndim = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    obj = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    readonly = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    shape = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    strides = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    suboffsets = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default



# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x039070B0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.newbuffer', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x039070B0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\newbuffer.cp37-win32.pyd')"

