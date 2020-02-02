# encoding: utf-8
# module pygame.rect
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\rect.cp37-win32.pyd
# by generator 1.147
""" Module for the rectangle object """
# no imports

# no functions
# classes

class RectType(object):
    """
    Rect(left, top, width, height) -> Rect
    Rect((left, top), (width, height)) -> Rect
    Rect(object) -> Rect
    pygame object for storing rectangular coordinates
    """
    def clamp(self, Rect): # real signature unknown; restored from __doc__
        """
        clamp(Rect) -> Rect
        moves the rectangle inside another
        """
        return Rect

    def clamp_ip(self, Rect): # real signature unknown; restored from __doc__
        """
        clamp_ip(Rect) -> None
        moves the rectangle inside another, in place
        """
        pass

    def clip(self, Rect): # real signature unknown; restored from __doc__
        """
        clip(Rect) -> Rect
        crops a rectangle inside another
        """
        return Rect

    def collidedict(self, dict): # real signature unknown; restored from __doc__
        """
        collidedict(dict) -> (key, value)
        test if one rectangle in a dictionary intersects
        """
        pass

    def collidedictall(self, dict): # real signature unknown; restored from __doc__
        """
        collidedictall(dict) -> [(key, value), ...]
        test if all rectangles in a dictionary intersect
        """
        pass

    def collidelist(self, p_list): # real signature unknown; restored from __doc__
        """
        collidelist(list) -> index
        test if one rectangle in a list intersects
        """
        pass

    def collidelistall(self, p_list): # real signature unknown; restored from __doc__
        """
        collidelistall(list) -> indices
        test if all rectangles in a list intersect
        """
        pass

    def collidepoint(self, x, y): # real signature unknown; restored from __doc__
        """
        collidepoint(x, y) -> bool
        collidepoint((x,y)) -> bool
        test if a point is inside a rectangle
        """
        return False

    def colliderect(self, Rect): # real signature unknown; restored from __doc__
        """
        colliderect(Rect) -> bool
        test if two rectangles overlap
        """
        return False

    def contains(self, Rect): # real signature unknown; restored from __doc__
        """
        contains(Rect) -> bool
        test if one rectangle is inside another
        """
        return False

    def copy(self): # real signature unknown; restored from __doc__
        """
        copy() -> Rect
        copy the rectangle
        """
        return Rect

    def fit(self, Rect): # real signature unknown; restored from __doc__
        """
        fit(Rect) -> Rect
        resize and move a rectangle with aspect ratio
        """
        return Rect

    def inflate(self, x, y): # real signature unknown; restored from __doc__
        """
        inflate(x, y) -> Rect
        grow or shrink the rectangle size
        """
        return Rect

    def inflate_ip(self, x, y): # real signature unknown; restored from __doc__
        """
        inflate_ip(x, y) -> None
        grow or shrink the rectangle size, in place
        """
        pass

    def move(self, x, y): # real signature unknown; restored from __doc__
        """
        move(x, y) -> Rect
        moves the rectangle
        """
        return Rect

    def move_ip(self, x, y): # real signature unknown; restored from __doc__
        """
        move_ip(x, y) -> None
        moves the rectangle, in place
        """
        pass

    def normalize(self): # real signature unknown; restored from __doc__
        """
        normalize() -> None
        correct negative sizes
        """
        pass

    def union(self, Rect): # real signature unknown; restored from __doc__
        """
        union(Rect) -> Rect
        joins two rectangles into one
        """
        return Rect

    def unionall(self, Rect_sequence): # real signature unknown; restored from __doc__
        """
        unionall(Rect_sequence) -> Rect
        the union of many rectangles
        """
        return Rect

    def unionall_ip(self, Rect_sequence): # real signature unknown; restored from __doc__
        """
        unionall_ip(Rect_sequence) -> None
        the union of many rectangles, in place
        """
        pass

    def union_ip(self, Rect): # real signature unknown; restored from __doc__
        """
        union_ip(Rect) -> None
        joins two rectangles into one, in place
        """
        pass

    def __bool__(self, *args, **kwargs): # real signature unknown
        """ self != 0 """
        pass

    def __copy__(self, *args, **kwargs): # real signature unknown
        pass

    def __delitem__(self, *args, **kwargs): # real signature unknown
        """ Delete self[key]. """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
        pass

    def __getitem__(self, *args, **kwargs): # real signature unknown
        """ Return self[key]. """
        pass

    def __ge__(self, *args, **kwargs): # real signature unknown
        """ Return self>=value. """
        pass

    def __gt__(self, *args, **kwargs): # real signature unknown
        """ Return self>value. """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    def __len__(self, *args, **kwargs): # real signature unknown
        """ Return len(self). """
        pass

    def __le__(self, *args, **kwargs): # real signature unknown
        """ Return self<=value. """
        pass

    def __lt__(self, *args, **kwargs): # real signature unknown
        """ Return self<value. """
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __ne__(self, *args, **kwargs): # real signature unknown
        """ Return self!=value. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setitem__(self, *args, **kwargs): # real signature unknown
        """ Set self[key] to value. """
        pass

    def __str__(self, *args, **kwargs): # real signature unknown
        """ Return str(self). """
        pass

    bottom = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    bottomleft = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    bottomright = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    centerx = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    centery = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    h = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    height = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    left = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    midbottom = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    midleft = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    midright = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    midtop = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    right = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    top = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    topleft = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    topright = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    w = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    width = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    x = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    y = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    __safe_for_unpickling__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __hash__ = None


Rect = RectType


# variables with complex values

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.rect._PYGAME_C_API" at 0x03E02C80>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x039DA3B0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.rect', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x039DA3B0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\rect.cp37-win32.pyd')"

