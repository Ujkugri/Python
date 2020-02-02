# encoding: utf-8
# module pygame.base
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\base.cp37-win32.pyd
# by generator 1.147
# no doc
# no imports

# Variables with simple values

HAVE_NEWBUF = 1

# functions

def get_array_interface(*args, **kwargs): # real signature unknown
    """ return an array struct interface as an interface dictionary """
    pass

def get_error(): # real signature unknown; restored from __doc__
    """
    get_error() -> errorstr
    get the current error message
    """
    pass

def get_init(): # real signature unknown; restored from __doc__
    """
    get_init() -> bool
    returns True if pygame is currently initialized
    """
    return False

def get_sdl_byteorder(): # real signature unknown; restored from __doc__
    """
    get_sdl_byteorder() -> int
    get the byte order of SDL
    """
    return 0

def get_sdl_version(): # real signature unknown; restored from __doc__
    """
    get_sdl_version() -> major, minor, patch
    get the version number of SDL
    """
    pass

def init(): # real signature unknown; restored from __doc__
    """
    init() -> (numpass, numfail)
    initialize all imported pygame modules
    """
    pass

def quit(): # real signature unknown; restored from __doc__
    """
    quit() -> None
    uninitialize all pygame modules
    """
    pass

def register_quit(callable): # real signature unknown; restored from __doc__
    """
    register_quit(callable) -> None
    register a function to be called when pygame quits
    """
    pass

def segfault(*args, **kwargs): # real signature unknown
    """ crash """
    pass

def set_error(error_msg): # real signature unknown; restored from __doc__
    """
    set_error(error_msg) -> None
    set the current error message
    """
    pass

# classes

class BufferError(BufferError):
    # no doc
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    __weakref__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """list of weak references to the object (if defined)"""



class error(RuntimeError):
    # no doc
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    __weakref__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """list of weak references to the object (if defined)"""



# variables with complex values

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.base._PYGAME_C_API" at 0x03742608>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x031B9F70>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.base', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x031B9F70>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\base.cp37-win32.pyd')"

