# encoding: utf-8
# module pygame.font
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\font.cp37-win32.pyd
# by generator 1.147
""" pygame module for loading and rendering fonts """
# no imports

# functions

def get_default_font(): # real signature unknown; restored from __doc__
    """
    get_default_font() -> string
    get the filename of the default font
    """
    return ""

def get_fonts(): # reliably restored by inspect
    """
    pygame.font.get_fonts() -> list
           get a list of system font names
    
           Returns the list of all found system fonts. Note that
           the names of the fonts will be all lowercase with spaces
           removed. This is how pygame internally stores the font
           names for matching.
    """
    pass

def get_init(): # real signature unknown; restored from __doc__
    """
    get_init() -> bool
    true if the font module is initialized
    """
    return False

def init(): # real signature unknown; restored from __doc__
    """
    init() -> None
    initialize the font module
    """
    pass

def match_font(name, bold=0, italic=0): # reliably restored by inspect
    """
    pygame.font.match_font(name, bold=0, italic=0) -> name
           find the filename for the named system font
    
           This performs the same font search as the SysFont()
           function, only it returns the path to the TTF file
           that would be loaded. The font name can be a comma
           separated list of font names to try.
    
           If no match is found, None is returned.
    """
    pass

def quit(): # real signature unknown; restored from __doc__
    """
    quit() -> None
    uninitialize the font module
    """
    pass

def SysFont(name, size, bold=False, italic=False, constructor=None): # reliably restored by inspect
    """
    pygame.font.SysFont(name, size, bold=False, italic=False, constructor=None) -> Font
           create a pygame Font from system font resources
    
           This will search the system fonts for the given font
           name. You can also enable bold or italic styles, and
           the appropriate system font will be selected if available.
    
           This will always return a valid Font object, and will
           fallback on the builtin pygame font if the given font
           is not found.
    
           Name can also be a comma separated list of names, in
           which case set of names will be searched in order. Pygame
           uses a small set of common font aliases, if the specific
           font you ask for is not available, a reasonable alternative
           may be used.
    
           if optional contructor is provided, it must be a function with
           signature constructor(fontpath, size, bold, italic) which returns
           a Font instance. If None, a pygame.font.Font object is created.
    """
    pass

def __PYGAMEinit__(*args, **kwargs): # real signature unknown
    """ auto initialize function for font """
    pass

# classes

class FontType(object):
    """
    Font(filename, size) -> Font
    Font(object, size) -> Font
    create a new Font object from a file
    """
    def get_ascent(self): # real signature unknown; restored from __doc__
        """
        get_ascent() -> int
        get the ascent of the font
        """
        return 0

    def get_bold(self): # real signature unknown; restored from __doc__
        """
        get_bold() -> bool
        check if text will be rendered bold
        """
        return False

    def get_descent(self): # real signature unknown; restored from __doc__
        """
        get_descent() -> int
        get the descent of the font
        """
        return 0

    def get_height(self): # real signature unknown; restored from __doc__
        """
        get_height() -> int
        get the height of the font
        """
        return 0

    def get_italic(self): # real signature unknown; restored from __doc__
        """
        get_italic() -> bool
        check if the text will be rendered italic
        """
        return False

    def get_linesize(self): # real signature unknown; restored from __doc__
        """
        get_linesize() -> int
        get the line space of the font text
        """
        return 0

    def get_underline(self): # real signature unknown; restored from __doc__
        """
        get_underline() -> bool
        check if text will be rendered with an underline
        """
        return False

    def metrics(self, text): # real signature unknown; restored from __doc__
        """
        metrics(text) -> list
        gets the metrics for each character in the passed string
        """
        return []

    def render(self, text, antialias, color, background=None): # real signature unknown; restored from __doc__
        """
        render(text, antialias, color, background=None) -> Surface
        draw text on a new Surface
        """
        pass

    def set_bold(self, bool): # real signature unknown; restored from __doc__
        """
        set_bold(bool) -> None
        enable fake rendering of bold text
        """
        pass

    def set_italic(self, bool): # real signature unknown; restored from __doc__
        """
        set_italic(bool) -> None
        enable fake rendering of italic text
        """
        pass

    def set_underline(self, bool): # real signature unknown; restored from __doc__
        """
        set_underline(bool) -> None
        control if text is rendered with an underline
        """
        pass

    def size(self, text): # real signature unknown; restored from __doc__
        """
        size(text) -> (width, height)
        determine the amount of space needed to render text
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass


Font = FontType


# variables with complex values

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.font._PYGAME_C_API" at 0x02F3FDE8>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x02FCF970>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.font', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x02FCF970>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\font.cp37-win32.pyd')"

