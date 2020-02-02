# encoding: utf-8
# module pygame.bufferproxy
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\bufferproxy.cp37-win32.pyd
# by generator 1.147
"""
BufferProxy(<parent>) -> BufferProxy
pygame object to export a surface buffer through an array protocol
"""
# no imports

# no functions
# classes

class BufferProxy(object):
    """
    BufferProxy(<parent>) -> BufferProxy
    pygame object to export a surface buffer through an array protocol
    """
    def write(self, buffer, offset=0): # real signature unknown; restored from __doc__
        """
        write(buffer, offset=0)
        Write raw bytes to object buffer.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    length = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """length -> int
The size, in bytes, of the exported buffer."""

    parent = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """parent -> Surface
parent -> <parent>
Return wrapped exporting object."""

    raw = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """raw -> bytes
A copy of the exported buffer as a single block of bytes."""

    __array_interface__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """Version 3 array interface, Python level"""

    __array_struct__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """Version 3 array interface, C level"""


    __dict__ = None # (!) real value is "mappingproxy({'__repr__': <slot wrapper '__repr__' of 'pygame.BufferProxy' objects>, '__new__': <built-in method __new__ of type object at 0x6F8344E0>, 'write': <method 'write' of 'pygame.BufferProxy' objects>, '__array_struct__': <attribute '__array_struct__' of 'pygame.BufferProxy' objects>, '__array_interface__': <attribute '__array_interface__' of 'pygame.BufferProxy' objects>, 'parent': <attribute 'parent' of 'pygame.BufferProxy' objects>, '__dict__': <attribute '__dict__' of 'pygame.BufferProxy' objects>, 'raw': <attribute 'raw' of 'pygame.BufferProxy' objects>, 'length': <attribute 'length' of 'pygame.BufferProxy' objects>, '__doc__': 'BufferProxy(<parent>) -> BufferProxy\\npygame object to export a surface buffer through an array protocol'})"


# variables with complex values

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.bufferproxy._PYGAME_C_API" at 0x03754590>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x03869410>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.bufferproxy', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x03869410>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\bufferproxy.cp37-win32.pyd')"

