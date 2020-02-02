# encoding: utf-8
# module pygame.transform
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\transform.cp37-win32.pyd
# by generator 1.147
""" pygame module to transform surfaces """
# no imports

# functions

def average_color(Surface, Rect=None): # real signature unknown; restored from __doc__
    """
    average_color(Surface, Rect = None) -> Color
    finds the average color of a surface
    """
    pass

def average_surfaces(Surfaces, DestSurface=None, palette_colors=1): # real signature unknown; restored from __doc__
    """
    average_surfaces(Surfaces, DestSurface = None, palette_colors = 1) -> Surface
    find the average surface from many surfaces.
    """
    pass

def chop(Surface, rect): # real signature unknown; restored from __doc__
    """
    chop(Surface, rect) -> Surface
    gets a copy of an image with an interior area removed
    """
    pass

def flip(Surface, xbool, ybool): # real signature unknown; restored from __doc__
    """
    flip(Surface, xbool, ybool) -> Surface
    flip vertically and horizontally
    """
    pass

def get_smoothscale_backend(): # real signature unknown; restored from __doc__
    """
    get_smoothscale_backend() -> String
    return smoothscale filter version in use: 'GENERIC', 'MMX', or 'SSE'
    """
    return ""

def laplacian(*args, **kwargs): # real signature unknown
    """
    threshold(dest_surf, surf, search_color, threshold=(0,0,0,0), set_color=(0,0,0,0), set_behavior=1, search_surf=None, inverse_set=False) -> num_threshold_pixels
    finds which, and how many pixels in a surface are within a threshold of a 'search_color' or a 'search_surf'.
    """
    pass

def rotate(Surface, angle): # real signature unknown; restored from __doc__
    """
    rotate(Surface, angle) -> Surface
    rotate an image
    """
    pass

def rotozoom(Surface, angle, scale): # real signature unknown; restored from __doc__
    """
    rotozoom(Surface, angle, scale) -> Surface
    filtered scale and rotation
    """
    pass

def scale(Surface, (width, height), DestSurface=None): # real signature unknown; restored from __doc__
    """
    scale(Surface, (width, height), DestSurface = None) -> Surface
    resize to new resolution
    """
    pass

def scale2x(Surface, DestSurface=None): # real signature unknown; restored from __doc__
    """
    scale2x(Surface, DestSurface = None) -> Surface
    specialized image doubler
    """
    pass

def set_smoothscale_backend(type): # real signature unknown; restored from __doc__
    """
    set_smoothscale_backend(type) -> None
    set smoothscale filter version to one of: 'GENERIC', 'MMX', or 'SSE'
    """
    pass

def smoothscale(Surface, (width, height), DestSurface=None): # real signature unknown; restored from __doc__
    """
    smoothscale(Surface, (width, height), DestSurface = None) -> Surface
    scale a surface to an arbitrary size smoothly
    """
    pass

def threshold(dest_surf, surf, search_color, threshold=(0,0,0,0), set_color=(0,0,0,0), set_behavior=1, search_surf=None, inverse_set=False): # real signature unknown; restored from __doc__
    """
    threshold(dest_surf, surf, search_color, threshold=(0,0,0,0), set_color=(0,0,0,0), set_behavior=1, search_surf=None, inverse_set=False) -> num_threshold_pixels
    finds which, and how many pixels in a surface are within a threshold of a 'search_color' or a 'search_surf'.
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x039D8910>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.transform', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x039D8910>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\transform.cp37-win32.pyd')"

