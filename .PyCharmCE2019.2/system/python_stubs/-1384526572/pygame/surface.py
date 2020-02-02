# encoding: utf-8
# module pygame.surface
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\surface.cp37-win32.pyd
# by generator 1.147
"""
Surface((width, height), flags=0, depth=0, masks=None) -> Surface
Surface((width, height), flags=0, Surface) -> Surface
pygame object for representing images
"""
# no imports

# no functions
# classes

class SurfaceType(object):
    """
    Surface((width, height), flags=0, depth=0, masks=None) -> Surface
    Surface((width, height), flags=0, Surface) -> Surface
    pygame object for representing images
    """
    def blit(self, source, dest, area=None, special_flags=0): # real signature unknown; restored from __doc__
        """
        blit(source, dest, area=None, special_flags=0) -> Rect
        draw one image onto another
        """
        pass

    def blits(self, blit_sequence, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        blits(blit_sequence=(source, dest), ...), doreturn=1) -> [Rect, ...] or None
        blits((source, dest, area), ...)) -> [Rect, ...]
        blits((source, dest, area, special_flags), ...)) -> [Rect, ...]
        draw many images onto another
        """
        pass

    def convert(self, Surface=None): # real signature unknown; restored from __doc__
        """
        convert(Surface=None) -> Surface
        convert(depth, flags=0) -> Surface
        convert(masks, flags=0) -> Surface
        change the pixel format of an image
        """
        return Surface

    def convert_alpha(self, Surface): # real signature unknown; restored from __doc__
        """
        convert_alpha(Surface) -> Surface
        convert_alpha() -> Surface
        change the pixel format of an image including per pixel alphas
        """
        return Surface

    def copy(self): # real signature unknown; restored from __doc__
        """
        copy() -> Surface
        create a new copy of a Surface
        """
        return Surface

    def fill(self, color, rect=None, special_flags=0): # real signature unknown; restored from __doc__
        """
        fill(color, rect=None, special_flags=0) -> Rect
        fill Surface with a solid color
        """
        pass

    def get_abs_offset(self): # real signature unknown; restored from __doc__
        """
        get_abs_offset() -> (x, y)
        find the absolute position of a child subsurface inside its top level parent
        """
        pass

    def get_abs_parent(self): # real signature unknown; restored from __doc__
        """
        get_abs_parent() -> Surface
        find the top level parent of a subsurface
        """
        return Surface

    def get_alpha(self): # real signature unknown; restored from __doc__
        """
        get_alpha() -> int_value
        get the current Surface transparency value
        """
        pass

    def get_at(self, (x, y)): # real signature unknown; restored from __doc__
        """
        get_at((x, y)) -> Color
        get the color value at a single pixel
        """
        pass

    def get_at_mapped(self, (x, y)): # real signature unknown; restored from __doc__
        """
        get_at_mapped((x, y)) -> Color
        get the mapped color value at a single pixel
        """
        pass

    def get_bitsize(self): # real signature unknown; restored from __doc__
        """
        get_bitsize() -> int
        get the bit depth of the Surface pixel format
        """
        return 0

    def get_bounding_rect(self, min_alpha=1): # real signature unknown; restored from __doc__
        """
        get_bounding_rect(min_alpha = 1) -> Rect
        find the smallest rect containing data
        """
        pass

    def get_buffer(self): # real signature unknown; restored from __doc__
        """
        get_buffer() -> BufferProxy
        acquires a buffer object for the pixels of the Surface.
        """
        pass

    def get_bytesize(self): # real signature unknown; restored from __doc__
        """
        get_bytesize() -> int
        get the bytes used per Surface pixel
        """
        return 0

    def get_clip(self): # real signature unknown; restored from __doc__
        """
        get_clip() -> Rect
        get the current clipping area of the Surface
        """
        pass

    def get_colorkey(self): # real signature unknown; restored from __doc__
        """
        get_colorkey() -> RGB or None
        Get the current transparent colorkey
        """
        pass

    def get_flags(self): # real signature unknown; restored from __doc__
        """
        get_flags() -> int
        get the additional flags used for the Surface
        """
        return 0

    def get_height(self): # real signature unknown; restored from __doc__
        """
        get_height() -> height
        get the height of the Surface
        """
        pass

    def get_locked(self): # real signature unknown; restored from __doc__
        """
        get_locked() -> bool
        test if the Surface is current locked
        """
        return False

    def get_locks(self): # real signature unknown; restored from __doc__
        """
        get_locks() -> tuple
        Gets the locks for the Surface
        """
        return ()

    def get_losses(self): # real signature unknown; restored from __doc__
        """
        get_losses() -> (R, G, B, A)
        the significant bits used to convert between a color and a mapped integer
        """
        pass

    def get_masks(self): # real signature unknown; restored from __doc__
        """
        get_masks() -> (R, G, B, A)
        the bitmasks needed to convert between a color and a mapped integer
        """
        pass

    def get_offset(self): # real signature unknown; restored from __doc__
        """
        get_offset() -> (x, y)
        find the position of a child subsurface inside a parent
        """
        pass

    def get_palette(self): # real signature unknown; restored from __doc__
        """
        get_palette() -> [RGB, RGB, RGB, ...]
        get the color index palette for an 8-bit Surface
        """
        pass

    def get_palette_at(self, index): # real signature unknown; restored from __doc__
        """
        get_palette_at(index) -> RGB
        get the color for a single entry in a palette
        """
        pass

    def get_parent(self): # real signature unknown; restored from __doc__
        """
        get_parent() -> Surface
        find the parent of a subsurface
        """
        return Surface

    def get_pitch(self): # real signature unknown; restored from __doc__
        """
        get_pitch() -> int
        get the number of bytes used per Surface row
        """
        return 0

    def get_rect(self, **kwargs): # real signature unknown; restored from __doc__
        """
        get_rect(**kwargs) -> Rect
        get the rectangular area of the Surface
        """
        pass

    def get_shifts(self): # real signature unknown; restored from __doc__
        """
        get_shifts() -> (R, G, B, A)
        the bit shifts needed to convert between a color and a mapped integer
        """
        pass

    def get_size(self): # real signature unknown; restored from __doc__
        """
        get_size() -> (width, height)
        get the dimensions of the Surface
        """
        pass

    def get_view(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        get_view(<kind>='2') -> BufferProxy
        return a buffer view of the Surface's pixels.
        """
        pass

    def get_width(self): # real signature unknown; restored from __doc__
        """
        get_width() -> width
        get the width of the Surface
        """
        pass

    def lock(self): # real signature unknown; restored from __doc__
        """
        lock() -> None
        lock the Surface memory for pixel access
        """
        pass

    def map_rgb(self, Color): # real signature unknown; restored from __doc__
        """
        map_rgb(Color) -> mapped_int
        convert a color into a mapped color value
        """
        pass

    def mustlock(self): # real signature unknown; restored from __doc__
        """
        mustlock() -> bool
        test if the Surface requires locking
        """
        return False

    def scroll(self, dx=0, dy=0): # real signature unknown; restored from __doc__
        """
        scroll(dx=0, dy=0) -> None
        Shift the surface image in place
        """
        pass

    def set_alpha(self, value, flags=0): # real signature unknown; restored from __doc__
        """
        set_alpha(value, flags=0) -> None
        set_alpha(None) -> None
        set the alpha value for the full Surface image
        """
        pass

    def set_at(self, (x, y), Color): # real signature unknown; restored from __doc__
        """
        set_at((x, y), Color) -> None
        set the color value for a single pixel
        """
        pass

    def set_clip(self, rect): # real signature unknown; restored from __doc__
        """
        set_clip(rect) -> None
        set_clip(None) -> None
        set the current clipping area of the Surface
        """
        pass

    def set_colorkey(self, Color, flags=0): # real signature unknown; restored from __doc__
        """
        set_colorkey(Color, flags=0) -> None
        set_colorkey(None) -> None
        Set the transparent colorkey
        """
        pass

    def set_masks(self, (r, g, b, a)): # real signature unknown; restored from __doc__
        """
        set_masks((r,g,b,a)) -> None
        set the bitmasks needed to convert between a color and a mapped integer
        """
        pass

    def set_palette(self, *RGB): # real signature unknown; restored from __doc__
        """
        set_palette([RGB, RGB, RGB, ...]) -> None
        set the color palette for an 8-bit Surface
        """
        pass

    def set_palette_at(self, index, RGB): # real signature unknown; restored from __doc__
        """
        set_palette_at(index, RGB) -> None
        set the color for a single index in an 8-bit Surface palette
        """
        pass

    def set_shifts(self, (r, g, b, a)): # real signature unknown; restored from __doc__
        """
        set_shifts((r,g,b,a)) -> None
        sets the bit shifts needed to convert between a color and a mapped integer
        """
        pass

    def subsurface(self, Rect): # real signature unknown; restored from __doc__
        """
        subsurface(Rect) -> Surface
        create a new surface that references its parent
        """
        return Surface

    def unlock(self): # real signature unknown; restored from __doc__
        """
        unlock() -> None
        unlock the Surface memory from pixel access
        """
        pass

    def unmap_rgb(self, mapped_int): # real signature unknown; restored from __doc__
        """
        unmap_rgb(mapped_int) -> Color
        convert a mapped integer color value into a Color
        """
        pass

    def __copy__(self, *args, **kwargs): # real signature unknown
        """
        copy() -> Surface
        create a new copy of a Surface
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    _pixels_address = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """pixel buffer address (readonly)"""



Surface = SurfaceType


# variables with complex values

_dict = {
    '__copy__': None, # (!) real value is "<method '__copy__' of 'pygame.Surface' objects>"
    '__doc__': 'Surface((width, height), flags=0, depth=0, masks=None) -> Surface\nSurface((width, height), flags=0, Surface) -> Surface\npygame object for representing images',
    '__init__': None, # (!) real value is "<slot wrapper '__init__' of 'pygame.Surface' objects>"
    '__new__': None, # (!) real value is '<built-in method __new__ of type object at 0x6F7E9930>'
    '__repr__': None, # (!) real value is "<slot wrapper '__repr__' of 'pygame.Surface' objects>"
    '_pixels_address': None, # (!) real value is "<attribute '_pixels_address' of 'pygame.Surface' objects>"
    'blit': None, # (!) real value is "<method 'blit' of 'pygame.Surface' objects>"
    'blits': None, # (!) real value is "<method 'blits' of 'pygame.Surface' objects>"
    'convert': None, # (!) real value is "<method 'convert' of 'pygame.Surface' objects>"
    'convert_alpha': None, # (!) real value is "<method 'convert_alpha' of 'pygame.Surface' objects>"
    'copy': None, # (!) real value is "<method 'copy' of 'pygame.Surface' objects>"
    'fill': None, # (!) real value is "<method 'fill' of 'pygame.Surface' objects>"
    'get_abs_offset': None, # (!) real value is "<method 'get_abs_offset' of 'pygame.Surface' objects>"
    'get_abs_parent': None, # (!) real value is "<method 'get_abs_parent' of 'pygame.Surface' objects>"
    'get_alpha': None, # (!) real value is "<method 'get_alpha' of 'pygame.Surface' objects>"
    'get_at': None, # (!) real value is "<method 'get_at' of 'pygame.Surface' objects>"
    'get_at_mapped': None, # (!) real value is "<method 'get_at_mapped' of 'pygame.Surface' objects>"
    'get_bitsize': None, # (!) real value is "<method 'get_bitsize' of 'pygame.Surface' objects>"
    'get_bounding_rect': None, # (!) real value is "<method 'get_bounding_rect' of 'pygame.Surface' objects>"
    'get_buffer': None, # (!) real value is "<method 'get_buffer' of 'pygame.Surface' objects>"
    'get_bytesize': None, # (!) real value is "<method 'get_bytesize' of 'pygame.Surface' objects>"
    'get_clip': None, # (!) real value is "<method 'get_clip' of 'pygame.Surface' objects>"
    'get_colorkey': None, # (!) real value is "<method 'get_colorkey' of 'pygame.Surface' objects>"
    'get_flags': None, # (!) real value is "<method 'get_flags' of 'pygame.Surface' objects>"
    'get_height': None, # (!) real value is "<method 'get_height' of 'pygame.Surface' objects>"
    'get_locked': None, # (!) real value is "<method 'get_locked' of 'pygame.Surface' objects>"
    'get_locks': None, # (!) real value is "<method 'get_locks' of 'pygame.Surface' objects>"
    'get_losses': None, # (!) real value is "<method 'get_losses' of 'pygame.Surface' objects>"
    'get_masks': None, # (!) real value is "<method 'get_masks' of 'pygame.Surface' objects>"
    'get_offset': None, # (!) real value is "<method 'get_offset' of 'pygame.Surface' objects>"
    'get_palette': None, # (!) real value is "<method 'get_palette' of 'pygame.Surface' objects>"
    'get_palette_at': None, # (!) real value is "<method 'get_palette_at' of 'pygame.Surface' objects>"
    'get_parent': None, # (!) real value is "<method 'get_parent' of 'pygame.Surface' objects>"
    'get_pitch': None, # (!) real value is "<method 'get_pitch' of 'pygame.Surface' objects>"
    'get_rect': None, # (!) real value is "<method 'get_rect' of 'pygame.Surface' objects>"
    'get_shifts': None, # (!) real value is "<method 'get_shifts' of 'pygame.Surface' objects>"
    'get_size': None, # (!) real value is "<method 'get_size' of 'pygame.Surface' objects>"
    'get_view': None, # (!) real value is "<method 'get_view' of 'pygame.Surface' objects>"
    'get_width': None, # (!) real value is "<method 'get_width' of 'pygame.Surface' objects>"
    'lock': None, # (!) real value is "<method 'lock' of 'pygame.Surface' objects>"
    'map_rgb': None, # (!) real value is "<method 'map_rgb' of 'pygame.Surface' objects>"
    'mustlock': None, # (!) real value is "<method 'mustlock' of 'pygame.Surface' objects>"
    'scroll': None, # (!) real value is "<method 'scroll' of 'pygame.Surface' objects>"
    'set_alpha': None, # (!) real value is "<method 'set_alpha' of 'pygame.Surface' objects>"
    'set_at': None, # (!) real value is "<method 'set_at' of 'pygame.Surface' objects>"
    'set_clip': None, # (!) real value is "<method 'set_clip' of 'pygame.Surface' objects>"
    'set_colorkey': None, # (!) real value is "<method 'set_colorkey' of 'pygame.Surface' objects>"
    'set_masks': None, # (!) real value is "<method 'set_masks' of 'pygame.Surface' objects>"
    'set_palette': None, # (!) real value is "<method 'set_palette' of 'pygame.Surface' objects>"
    'set_palette_at': None, # (!) real value is "<method 'set_palette_at' of 'pygame.Surface' objects>"
    'set_shifts': None, # (!) real value is "<method 'set_shifts' of 'pygame.Surface' objects>"
    'subsurface': None, # (!) real value is "<method 'subsurface' of 'pygame.Surface' objects>"
    'unlock': None, # (!) real value is "<method 'unlock' of 'pygame.Surface' objects>"
    'unmap_rgb': None, # (!) real value is "<method 'unmap_rgb' of 'pygame.Surface' objects>"
}

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.surface._PYGAME_C_API" at 0x0301F290>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x030899B0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.surface', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x030899B0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\surface.cp37-win32.pyd')"

