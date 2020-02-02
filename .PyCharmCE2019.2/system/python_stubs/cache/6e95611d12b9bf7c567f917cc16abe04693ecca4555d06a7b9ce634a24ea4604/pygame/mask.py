# encoding: utf-8
# module pygame.mask
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\mask.cp37-win32.pyd
# by generator 1.147
""" pygame module for image masks. """
# no imports

# functions

def from_surface(Surface, threshold=127): # real signature unknown; restored from __doc__
    """
    from_surface(Surface, threshold = 127) -> Mask
    Returns a Mask from the given surface.
    """
    pass

def from_threshold(Surface, color, threshold=None, othersurface=None, palette_colors=1): # real signature unknown; restored from __doc__
    """
    from_threshold(Surface, color, threshold = (0,0,0,255), othersurface = None, palette_colors = 1) -> Mask
    Creates a mask by thresholding Surfaces
    """
    pass

def Mask(size, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
    """
    Mask(size=(width, height)) -> Mask
    Mask(size=(width, height), fill=False) -> Mask
    pygame object for representing 2D bitmasks
    """
    pass

# classes

class MaskType(object):
    """
    Mask(size=(width, height)) -> Mask
    Mask(size=(width, height), fill=False) -> Mask
    pygame object for representing 2D bitmasks
    """
    def angle(self): # real signature unknown; restored from __doc__
        """
        angle() -> theta
        Returns the orientation of the pixels
        """
        pass

    def centroid(self): # real signature unknown; restored from __doc__
        """
        centroid() -> (x, y)
        Returns the centroid of the pixels in a Mask
        """
        pass

    def clear(self): # real signature unknown; restored from __doc__
        """
        clear() -> None
        Sets all bits to 0
        """
        pass

    def connected_component(self, (x, y), *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        connected_component((x,y) = None) -> Mask
        Returns a mask of a connected region of pixels.
        """
        pass

    def connected_components(self, min=0): # real signature unknown; restored from __doc__
        """
        connected_components(min = 0) -> [Masks]
        Returns a list of masks of connected regions of pixels.
        """
        pass

    def convolve(self, othermask): # real signature unknown; restored from __doc__
        """
        convolve(othermask) -> Mask
        convolve(othermask, outputmask=None, offset=(0,0)) -> Mask
        Return the convolution of self with another mask.
        """
        pass

    def count(self): # real signature unknown; restored from __doc__
        """
        count() -> pixels
        Returns the number of set pixels
        """
        pass

    def draw(self, othermask, offset): # real signature unknown; restored from __doc__
        """
        draw(othermask, offset) -> None
        Draws a mask onto another
        """
        pass

    def erase(self, othermask, offset): # real signature unknown; restored from __doc__
        """
        erase(othermask, offset) -> None
        Erases a mask from another
        """
        pass

    def fill(self): # real signature unknown; restored from __doc__
        """
        fill() -> None
        Sets all bits to 1
        """
        pass

    def get_at(self, (x, y)): # real signature unknown; restored from __doc__
        """
        get_at((x,y)) -> int
        Returns nonzero if the bit at (x,y) is set.
        """
        return 0

    def get_bounding_rects(self): # real signature unknown; restored from __doc__
        """
        get_bounding_rects() -> Rects
        Returns a list of bounding rects of regions of set pixels.
        """
        pass

    def get_size(self): # real signature unknown; restored from __doc__
        """
        get_size() -> width,height
        Returns the size of the mask.
        """
        pass

    def invert(self): # real signature unknown; restored from __doc__
        """
        invert() -> None
        Flips the bits in a Mask
        """
        pass

    def outline(self, every=1): # real signature unknown; restored from __doc__
        """
        outline(every = 1) -> [(x,y), (x,y) ...]
        list of points outlining an object
        """
        pass

    def overlap(self, othermask, offset): # real signature unknown; restored from __doc__
        """
        overlap(othermask, offset) -> x,y
        Returns the point of intersection if the masks overlap with the given offset - or None if it does not overlap.
        """
        pass

    def overlap_area(self, othermask, offset): # real signature unknown; restored from __doc__
        """
        overlap_area(othermask, offset) -> numpixels
        Returns the number of overlapping 'pixels'.
        """
        pass

    def overlap_mask(self, othermask, offset): # real signature unknown; restored from __doc__
        """
        overlap_mask(othermask, offset) -> Mask
        Returns a mask of the overlapping pixels
        """
        pass

    def scale(self, (x, y)): # real signature unknown; restored from __doc__
        """
        scale((x, y)) -> Mask
        Resizes a mask
        """
        pass

    def set_at(self, (x, y), value): # real signature unknown; restored from __doc__
        """
        set_at((x,y),value) -> None
        Sets the position in the mask given by x and y.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass


# variables with complex values

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.mask._PYGAME_C_API" at 0x037EFA70>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0386C650>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.mask', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0386C650>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\mask.cp37-win32.pyd')"

