# encoding: utf-8
# module pygame._freetype
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\_freetype.cp37-win32.pyd
# by generator 1.147
""" Enhanced pygame module for loading and rendering computer fonts """
# no imports

# Variables with simple values

BBOX_EXACT = 0

BBOX_EXACT_GRIDFIT = 1

BBOX_PIXEL = 2

BBOX_PIXEL_GRIDFIT = 3

STYLE_DEFAULT = 255
STYLE_NORMAL = 0
STYLE_OBLIQUE = 2
STYLE_STRONG = 1
STYLE_UNDERLINE = 4
STYLE_WIDE = 8

# functions

def get_cache_size(): # real signature unknown; restored from __doc__
    """
    get_cache_size() -> long
    Return the glyph case size
    """
    return 0

def get_default_font(): # real signature unknown; restored from __doc__
    """
    get_default_font() -> string
    Get the filename of the default font
    """
    return ""

def get_default_resolution(): # real signature unknown; restored from __doc__
    """
    get_default_resolution() -> long
    Return the default pixel size in dots per inch
    """
    return 0

def get_error(): # real signature unknown; restored from __doc__
    """
    get_error() -> str
    Return the latest FreeType error
    """
    return ""

def get_init(): # real signature unknown; restored from __doc__
    """
    get_init() -> bool
    Returns True if the FreeType module is currently initialized.
    """
    return False

def get_version(): # real signature unknown; restored from __doc__
    """
    get_version() -> (int, int, int)
    Return the FreeType version
    """
    pass

def init(cache_size=64, resolution=72): # real signature unknown; restored from __doc__
    """
    init(cache_size=64, resolution=72)
    Initialize the underlying FreeType library.
    """
    pass

def quit(): # real signature unknown; restored from __doc__
    """
    quit()
    Shut down the underlying FreeType library.
    """
    pass

def set_default_resolution(resolution=None): # real signature unknown; restored from __doc__
    """
    set_default_resolution([resolution])
    Set the default pixel size in dots per inch for the module
    """
    pass

def was_init(): # real signature unknown; restored from __doc__
    """
    was_init() -> bool
    DEPRECATED: Use get_init() instead.
    """
    return False

def __PYGAMEinit__(*args, **kwargs): # real signature unknown
    """ auto initialize function for _freetype """
    pass

# classes

class Font(object):
    """
    Font(file, size=0, font_index=0, resolution=0, ucs4=False) -> Font
    Create a new Font instance from a supported font file.
    """
    def get_metrics(self, text, size=0): # real signature unknown; restored from __doc__
        """
        get_metrics(text, size=0) -> [(...), ...]
        Return the glyph metrics for the given text
        """
        pass

    def get_rect(self, text, style=None, rotation=0, size=0): # real signature unknown; restored from __doc__
        """
        get_rect(text, style=STYLE_DEFAULT, rotation=0, size=0) -> rect
        Return the size and offset of rendered text
        """
        pass

    def get_sized_ascender(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        get_sized_ascender(<size>=0) -> int
        The scaled ascent of the font in pixels
        """
        pass

    def get_sized_descender(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        get_sized_descender(<size>=0) -> int
        The scaled descent of the font in pixels
        """
        pass

    def get_sized_glyph_height(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        get_sized_glyph_height(<size>=0) -> int
        The scaled bounding box height of the font in pixels
        """
        pass

    def get_sized_height(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        get_sized_height(<size>=0) -> int
        The scaled height of the font in pixels
        """
        pass

    def get_sizes(self): # real signature unknown; restored from __doc__
        """
        get_sizes() -> [(int, int, int, float, float), ...]
        get_sizes() -> []
        return the available sizes of embedded bitmaps
        """
        pass

    def render(self, text, fgcolor=None, bgcolor=None, style=None, rotation=0, size=0): # real signature unknown; restored from __doc__
        """
        render(text, fgcolor=None, bgcolor=None, style=STYLE_DEFAULT, rotation=0, size=0) -> (Surface, Rect)
        Return rendered text as a surface
        """
        pass

    def render_raw(self, text, style=None, rotation=0, size=0, invert=False): # real signature unknown; restored from __doc__
        """
        render_raw(text, style=STYLE_DEFAULT, rotation=0, size=0, invert=False) -> (bytes, (int, int))
        Return rendered text as a string of bytes
        """
        pass

    def render_raw_to(self, array, text, dest=None, style=None, rotation=0, size=0, invert=False): # real signature unknown; restored from __doc__
        """
        render_raw_to(array, text, dest=None, style=STYLE_DEFAULT, rotation=0, size=0, invert=False) -> (int, int)
        Render text into an array of ints
        """
        pass

    def render_to(self, surf, dest, text, fgcolor=None, bgcolor=None, style=None, rotation=0, size=0): # real signature unknown; restored from __doc__
        """
        render_to(surf, dest, text, fgcolor=None, bgcolor=None, style=STYLE_DEFAULT, rotation=0, size=0) -> Rect
        Render text onto an existing surface
        """
        pass

    def __init__(self, file, size=0, font_index=0, resolution=0, ucs4=False): # real signature unknown; restored from __doc__
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    antialiased = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """antialiased -> bool
Font anti-aliasing mode"""

    ascender = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """ascender -> int
The unscaled ascent of the font in font units"""

    descender = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """ascender -> int
The unscaled ascent of the font in font units"""

    fgcolor = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """fgcolor -> Color
default foreground color"""

    fixed_sizes = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """fixed_sizes -> int
the number of available bitmap sizes for the font"""

    fixed_width = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """fixed_width -> bool
Gets whether the font is fixed-width"""

    height = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """height -> int
The unscaled height of the font in font units"""

    kerning = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """kerning -> bool
Character kerning mode"""

    name = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """name -> string
Proper font name."""

    oblique = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """oblique -> bool
The state of the font's oblique style flag"""

    origin = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """origin -> bool
Font render to text origin mode"""

    pad = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """pad -> bool
padded boundary mode"""

    path = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """path -> unicode
Font file path"""

    resolution = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """resolution -> int
Pixel resolution in dots per inch"""

    rotation = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """rotation -> int
text rotation in degrees counterclockwise"""

    scalable = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """scalable -> bool
Gets whether the font is scalable"""

    size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """size -> float
size -> (float, float)
The default point size used in rendering"""

    strength = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """strength -> float
The strength associated with the strong or wide font styles"""

    strong = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """strong -> bool
The state of the font's strong style flag"""

    style = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """style -> int
The font's style flags"""

    ucs4 = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """ucs4 -> bool
Enable UCS-4 mode"""

    underline = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """underline -> bool
The state of the font's underline style flag"""

    underline_adjustment = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """underline_adjustment -> float
Adjustment factor for the underline position"""

    use_bitmap_strikes = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """use_bitmap_strikes -> bool
allow the use of embedded bitmaps in an outline font file"""

    vertical = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """vertical -> bool
Font vertical mode"""

    wide = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """wide -> bool
The state of the font's wide style flag"""



# variables with complex values

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.freetype._PYGAME_C_API" at 0x02E06488>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x030470D0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame._freetype', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x030470D0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\_freetype.cp37-win32.pyd')"

