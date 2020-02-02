# encoding: utf-8
# module pygame.math
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\math.cp37-win32.pyd
# by generator 1.147
""" pygame module for vector classes """
# no imports

# functions

def disable_swizzling(*args, **kwargs): # real signature unknown
    """ disables swizzling. """
    pass

def enable_swizzling(*args, **kwargs): # real signature unknown
    """ enables swizzling. """
    pass

# classes

class Vector2(object):
    """
    Vector2() -> Vector2
    Vector2(int) -> Vector2
    Vector2(float) -> Vector2
    Vector2(Vector2) -> Vector2
    Vector2(x, y) -> Vector2
    Vector2((x, y)) -> Vector2
    a 2-Dimensional Vector
    """
    def angle_to(self, Vector2): # real signature unknown; restored from __doc__
        """
        angle_to(Vector2) -> float
        calculates the angle to a given vector in degrees.
        """
        return 0.0

    def as_polar(self): # real signature unknown; restored from __doc__
        """
        as_polar() -> (r, phi)
        returns a tuple with radial distance and azimuthal angle.
        """
        pass

    def cross(self, Vector2): # real signature unknown; restored from __doc__
        """
        cross(Vector2) -> Vector2
        calculates the cross- or vector-product
        """
        return Vector2

    def distance_squared_to(self, Vector2): # real signature unknown; restored from __doc__
        """
        distance_squared_to(Vector2) -> float
        calculates the squared Euclidean distance to a given vector.
        """
        return 0.0

    def distance_to(self, Vector2): # real signature unknown; restored from __doc__
        """
        distance_to(Vector2) -> float
        calculates the Euclidean distance to a given vector.
        """
        return 0.0

    def dot(self, Vector2): # real signature unknown; restored from __doc__
        """
        dot(Vector2) -> float
        calculates the dot- or scalar-product with the other vector
        """
        return 0.0

    def elementwise(self): # real signature unknown; restored from __doc__
        """
        elementwise() -> VectorElementwiseProxy
        The next operation will be performed elementwise.
        """
        return VectorElementwiseProxy

    def from_polar(self, (r, phi)): # real signature unknown; restored from __doc__
        """
        from_polar((r, phi)) -> None
        Sets x and y from a polar coordinates tuple.
        """
        pass

    def is_normalized(self): # real signature unknown; restored from __doc__
        """
        is_normalized() -> Bool
        tests if the vector is normalized i.e. has length == 1.
        """
        return False

    def length(self): # real signature unknown; restored from __doc__
        """
        length() -> float
        returns the Euclidean length of the vector.
        """
        return 0.0

    def length_squared(self): # real signature unknown; restored from __doc__
        """
        length_squared() -> float
        returns the squared Euclidean length of the vector.
        """
        return 0.0

    def lerp(self, Vector2, p_float): # real signature unknown; restored from __doc__
        """
        lerp(Vector2, float) -> Vector2
        returns a linear interpolation to the given vector.
        """
        return Vector2

    def magnitude(self): # real signature unknown; restored from __doc__
        """
        magnitude() -> float
        returns the Euclidean magnitude of the vector.
        """
        return 0.0

    def magnitude_squared(self): # real signature unknown; restored from __doc__
        """
        magnitude_squared() -> float
        returns the squared magnitude of the vector.
        """
        return 0.0

    def normalize(self): # real signature unknown; restored from __doc__
        """
        normalize() -> Vector2
        returns a vector with the same direction but length 1.
        """
        return Vector2

    def normalize_ip(self): # real signature unknown; restored from __doc__
        """
        normalize_ip() -> None
        normalizes the vector in place so that its length is 1.
        """
        pass

    def reflect(self, Vector2): # real signature unknown; restored from __doc__
        """
        reflect(Vector2) -> Vector2
        returns a vector reflected of a given normal.
        """
        return Vector2

    def reflect_ip(self, Vector2): # real signature unknown; restored from __doc__
        """
        reflect_ip(Vector2) -> None
        reflect the vector of a given normal in place.
        """
        pass

    def rotate(self, p_float): # real signature unknown; restored from __doc__
        """
        rotate(float) -> Vector2
        rotates a vector by a given angle in degrees.
        """
        return Vector2

    def rotate_ip(self, p_float): # real signature unknown; restored from __doc__
        """
        rotate_ip(float) -> None
        rotates the vector by a given angle in degrees in place.
        """
        pass

    def scale_to_length(self, p_float): # real signature unknown; restored from __doc__
        """
        scale_to_length(float) -> None
        scales the vector to a given length.
        """
        pass

    def slerp(self, Vector2, p_float): # real signature unknown; restored from __doc__
        """
        slerp(Vector2, float) -> Vector2
        returns a spherical interpolation to the given vector.
        """
        return Vector2

    def update(self): # real signature unknown; restored from __doc__
        """
        update() -> None
        update(int) -> None
        update(float) -> None
        update(Vector2) -> None
        update(x, y) -> None
        update((x, y)) -> None
        Sets the coordinates of the vector.
        """
        pass

    def __add__(self, *args, **kwargs): # real signature unknown
        """ Return self+value. """
        pass

    def __bool__(self, *args, **kwargs): # real signature unknown
        """ self != 0 """
        pass

    def __delattr__(self, *args, **kwargs): # real signature unknown
        """ Implement delattr(self, name). """
        pass

    def __delitem__(self, *args, **kwargs): # real signature unknown
        """ Delete self[key]. """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
        pass

    def __floordiv__(self, *args, **kwargs): # real signature unknown
        """ Return self//value. """
        pass

    def __getattribute__(self, *args, **kwargs): # real signature unknown
        """ Return getattr(self, name). """
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

    def __iadd__(self, *args, **kwargs): # real signature unknown
        """ Return self+=value. """
        pass

    def __ifloordiv__(self, *args, **kwargs): # real signature unknown
        """ Return self//=value. """
        pass

    def __imul__(self, *args, **kwargs): # real signature unknown
        """ Return self*=value. """
        pass

    def __init__(self): # real signature unknown; restored from __doc__
        pass

    def __isub__(self, *args, **kwargs): # real signature unknown
        """ Return self-=value. """
        pass

    def __iter__(self, *args, **kwargs): # real signature unknown
        """ Implement iter(self). """
        pass

    def __itruediv__(self, *args, **kwargs): # real signature unknown
        """ Return self/=value. """
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

    def __mul__(self, *args, **kwargs): # real signature unknown
        """ Return self*value. """
        pass

    def __neg__(self, *args, **kwargs): # real signature unknown
        """ -self """
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __ne__(self, *args, **kwargs): # real signature unknown
        """ Return self!=value. """
        pass

    def __pos__(self, *args, **kwargs): # real signature unknown
        """ +self """
        pass

    def __radd__(self, *args, **kwargs): # real signature unknown
        """ Return value+self. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __rfloordiv__(self, *args, **kwargs): # real signature unknown
        """ Return value//self. """
        pass

    def __rmul__(self, *args, **kwargs): # real signature unknown
        """ Return value*self. """
        pass

    def __rsub__(self, *args, **kwargs): # real signature unknown
        """ Return value-self. """
        pass

    def __rtruediv__(self, *args, **kwargs): # real signature unknown
        """ Return value/self. """
        pass

    def __safe_for_unpickling__(self, *args, **kwargs): # real signature unknown
        pass

    def __setattr__(self, *args, **kwargs): # real signature unknown
        """ Implement setattr(self, name, value). """
        pass

    def __setitem__(self, *args, **kwargs): # real signature unknown
        """ Set self[key] to value. """
        pass

    def __str__(self, *args, **kwargs): # real signature unknown
        """ Return str(self). """
        pass

    def __sub__(self, *args, **kwargs): # real signature unknown
        """ Return self-value. """
        pass

    def __truediv__(self, *args, **kwargs): # real signature unknown
        """ Return self/value. """
        pass

    epsilon = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """small value used in comparisons"""

    x = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    y = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __hash__ = None


class Vector3(object):
    """
    Vector3() -> Vector3
    Vector3(int) -> Vector3
    Vector3(float) -> Vector3
    Vector3(Vector3) -> Vector3
    Vector3(x, y, z) -> Vector3
    Vector3((x, y, z)) -> Vector3
    a 3-Dimensional Vector
    """
    def angle_to(self, Vector3): # real signature unknown; restored from __doc__
        """
        angle_to(Vector3) -> float
        calculates the angle to a given vector in degrees.
        """
        return 0.0

    def as_spherical(self): # real signature unknown; restored from __doc__
        """
        as_spherical() -> (r, theta, phi)
        returns a tuple with radial distance, inclination and azimuthal angle.
        """
        pass

    def cross(self, Vector3): # real signature unknown; restored from __doc__
        """
        cross(Vector3) -> Vector3
        calculates the cross- or vector-product
        """
        return Vector3

    def distance_squared_to(self, Vector3): # real signature unknown; restored from __doc__
        """
        distance_squared_to(Vector3) -> float
        calculates the squared Euclidean distance to a given vector.
        """
        return 0.0

    def distance_to(self, Vector3): # real signature unknown; restored from __doc__
        """
        distance_to(Vector3) -> float
        calculates the Euclidean distance to a given vector.
        """
        return 0.0

    def dot(self, Vector3): # real signature unknown; restored from __doc__
        """
        dot(Vector3) -> float
        calculates the dot- or scalar-product with the other vector
        """
        return 0.0

    def elementwise(self): # real signature unknown; restored from __doc__
        """
        elementwise() -> VectorElementwiseProxy
        The next operation will be performed elementwise.
        """
        return VectorElementwiseProxy

    def from_spherical(self, (r, theta, phi)): # real signature unknown; restored from __doc__
        """
        from_spherical((r, theta, phi)) -> None
        Sets x, y and z from a spherical coordinates 3-tuple.
        """
        pass

    def is_normalized(self): # real signature unknown; restored from __doc__
        """
        is_normalized() -> Bool
        tests if the vector is normalized i.e. has length == 1.
        """
        return False

    def length(self): # real signature unknown; restored from __doc__
        """
        length() -> float
        returns the Euclidean length of the vector.
        """
        return 0.0

    def length_squared(self): # real signature unknown; restored from __doc__
        """
        length_squared() -> float
        returns the squared Euclidean length of the vector.
        """
        return 0.0

    def lerp(self, Vector3, p_float): # real signature unknown; restored from __doc__
        """
        lerp(Vector3, float) -> Vector3
        returns a linear interpolation to the given vector.
        """
        return Vector3

    def magnitude(self): # real signature unknown; restored from __doc__
        """
        magnitude() -> float
        returns the Euclidean magnitude of the vector.
        """
        return 0.0

    def magnitude_squared(self): # real signature unknown; restored from __doc__
        """
        magnitude_squared() -> float
        returns the squared Euclidean magnitude of the vector.
        """
        return 0.0

    def normalize(self): # real signature unknown; restored from __doc__
        """
        normalize() -> Vector3
        returns a vector with the same direction but length 1.
        """
        return Vector3

    def normalize_ip(self): # real signature unknown; restored from __doc__
        """
        normalize_ip() -> None
        normalizes the vector in place so that its length is 1.
        """
        pass

    def reflect(self, Vector3): # real signature unknown; restored from __doc__
        """
        reflect(Vector3) -> Vector3
        returns a vector reflected of a given normal.
        """
        return Vector3

    def reflect_ip(self, Vector3): # real signature unknown; restored from __doc__
        """
        reflect_ip(Vector3) -> None
        reflect the vector of a given normal in place.
        """
        pass

    def rotate(self, Vector3, p_float): # real signature unknown; restored from __doc__
        """
        rotate(Vector3, float) -> Vector3
        rotates a vector by a given angle in degrees.
        """
        return Vector3

    def rotate_ip(self, Vector3, p_float): # real signature unknown; restored from __doc__
        """
        rotate_ip(Vector3, float) -> None
        rotates the vector by a given angle in degrees in place.
        """
        pass

    def rotate_x(self, p_float): # real signature unknown; restored from __doc__
        """
        rotate_x(float) -> Vector3
        rotates a vector around the x-axis by the angle in degrees.
        """
        return Vector3

    def rotate_x_ip(self, p_float): # real signature unknown; restored from __doc__
        """
        rotate_x_ip(float) -> None
        rotates the vector around the x-axis by the angle in degrees in place.
        """
        pass

    def rotate_y(self, p_float): # real signature unknown; restored from __doc__
        """
        rotate_y(float) -> Vector3
        rotates a vector around the y-axis by the angle in degrees.
        """
        return Vector3

    def rotate_y_ip(self, p_float): # real signature unknown; restored from __doc__
        """
        rotate_y_ip(float) -> None
        rotates the vector around the y-axis by the angle in degrees in place.
        """
        pass

    def rotate_z(self, p_float): # real signature unknown; restored from __doc__
        """
        rotate_z(float) -> Vector3
        rotates a vector around the z-axis by the angle in degrees.
        """
        return Vector3

    def rotate_z_ip(self, p_float): # real signature unknown; restored from __doc__
        """
        rotate_z_ip(float) -> None
        rotates the vector around the z-axis by the angle in degrees in place.
        """
        pass

    def scale_to_length(self, p_float): # real signature unknown; restored from __doc__
        """
        scale_to_length(float) -> None
        scales the vector to a given length.
        """
        pass

    def slerp(self, Vector3, p_float): # real signature unknown; restored from __doc__
        """
        slerp(Vector3, float) -> Vector3
        returns a spherical interpolation to the given vector.
        """
        return Vector3

    def update(self): # real signature unknown; restored from __doc__
        """
        update() -> None
        update(int) -> None
        update(float) -> None
        update(Vector3) -> None
        update(x, y, z) -> None
        update((x, y, z)) -> None
        Sets the coordinates of the vector.
        """
        pass

    def __add__(self, *args, **kwargs): # real signature unknown
        """ Return self+value. """
        pass

    def __bool__(self, *args, **kwargs): # real signature unknown
        """ self != 0 """
        pass

    def __delattr__(self, *args, **kwargs): # real signature unknown
        """ Implement delattr(self, name). """
        pass

    def __delitem__(self, *args, **kwargs): # real signature unknown
        """ Delete self[key]. """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
        pass

    def __floordiv__(self, *args, **kwargs): # real signature unknown
        """ Return self//value. """
        pass

    def __getattribute__(self, *args, **kwargs): # real signature unknown
        """ Return getattr(self, name). """
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

    def __iadd__(self, *args, **kwargs): # real signature unknown
        """ Return self+=value. """
        pass

    def __ifloordiv__(self, *args, **kwargs): # real signature unknown
        """ Return self//=value. """
        pass

    def __imul__(self, *args, **kwargs): # real signature unknown
        """ Return self*=value. """
        pass

    def __init__(self): # real signature unknown; restored from __doc__
        pass

    def __isub__(self, *args, **kwargs): # real signature unknown
        """ Return self-=value. """
        pass

    def __iter__(self, *args, **kwargs): # real signature unknown
        """ Implement iter(self). """
        pass

    def __itruediv__(self, *args, **kwargs): # real signature unknown
        """ Return self/=value. """
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

    def __mul__(self, *args, **kwargs): # real signature unknown
        """ Return self*value. """
        pass

    def __neg__(self, *args, **kwargs): # real signature unknown
        """ -self """
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __ne__(self, *args, **kwargs): # real signature unknown
        """ Return self!=value. """
        pass

    def __pos__(self, *args, **kwargs): # real signature unknown
        """ +self """
        pass

    def __radd__(self, *args, **kwargs): # real signature unknown
        """ Return value+self. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __rfloordiv__(self, *args, **kwargs): # real signature unknown
        """ Return value//self. """
        pass

    def __rmul__(self, *args, **kwargs): # real signature unknown
        """ Return value*self. """
        pass

    def __rsub__(self, *args, **kwargs): # real signature unknown
        """ Return value-self. """
        pass

    def __rtruediv__(self, *args, **kwargs): # real signature unknown
        """ Return value/self. """
        pass

    def __safe_for_unpickling__(self, *args, **kwargs): # real signature unknown
        pass

    def __setattr__(self, *args, **kwargs): # real signature unknown
        """ Implement setattr(self, name, value). """
        pass

    def __setitem__(self, *args, **kwargs): # real signature unknown
        """ Set self[key] to value. """
        pass

    def __str__(self, *args, **kwargs): # real signature unknown
        """ Return str(self). """
        pass

    def __sub__(self, *args, **kwargs): # real signature unknown
        """ Return self-value. """
        pass

    def __truediv__(self, *args, **kwargs): # real signature unknown
        """ Return self/value. """
        pass

    epsilon = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """small value used in comparisons"""

    x = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    y = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    z = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __hash__ = None


class VectorElementwiseProxy(object):
    # no doc
    def __abs__(self, *args, **kwargs): # real signature unknown
        """ abs(self) """
        pass

    def __add__(self, *args, **kwargs): # real signature unknown
        """ Return self+value. """
        pass

    def __bool__(self, *args, **kwargs): # real signature unknown
        """ self != 0 """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
        pass

    def __floordiv__(self, *args, **kwargs): # real signature unknown
        """ Return self//value. """
        pass

    def __ge__(self, *args, **kwargs): # real signature unknown
        """ Return self>=value. """
        pass

    def __gt__(self, *args, **kwargs): # real signature unknown
        """ Return self>value. """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    def __le__(self, *args, **kwargs): # real signature unknown
        """ Return self<=value. """
        pass

    def __lt__(self, *args, **kwargs): # real signature unknown
        """ Return self<value. """
        pass

    def __mod__(self, *args, **kwargs): # real signature unknown
        """ Return self%value. """
        pass

    def __mul__(self, *args, **kwargs): # real signature unknown
        """ Return self*value. """
        pass

    def __neg__(self, *args, **kwargs): # real signature unknown
        """ -self """
        pass

    def __ne__(self, *args, **kwargs): # real signature unknown
        """ Return self!=value. """
        pass

    def __pos__(self, *args, **kwargs): # real signature unknown
        """ +self """
        pass

    def __pow__(self, *args, **kwargs): # real signature unknown
        """ Return pow(self, value, mod). """
        pass

    def __radd__(self, *args, **kwargs): # real signature unknown
        """ Return value+self. """
        pass

    def __rfloordiv__(self, *args, **kwargs): # real signature unknown
        """ Return value//self. """
        pass

    def __rmod__(self, *args, **kwargs): # real signature unknown
        """ Return value%self. """
        pass

    def __rmul__(self, *args, **kwargs): # real signature unknown
        """ Return value*self. """
        pass

    def __rpow__(self, *args, **kwargs): # real signature unknown
        """ Return pow(value, self, mod). """
        pass

    def __rsub__(self, *args, **kwargs): # real signature unknown
        """ Return value-self. """
        pass

    def __rtruediv__(self, *args, **kwargs): # real signature unknown
        """ Return value/self. """
        pass

    def __sub__(self, *args, **kwargs): # real signature unknown
        """ Return self-value. """
        pass

    def __truediv__(self, *args, **kwargs): # real signature unknown
        """ Return self/value. """
        pass

    __hash__ = None


class VectorIterator(object):
    # no doc
    def __getattribute__(self, *args, **kwargs): # real signature unknown
        """ Return getattr(self, name). """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    def __iter__(self, *args, **kwargs): # real signature unknown
        """ Implement iter(self). """
        pass

    def __length_hint__(self, *args, **kwargs): # real signature unknown
        pass

    def __next__(self, *args, **kwargs): # real signature unknown
        """ Implement next(self). """
        pass


# variables with complex values

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.math._PYGAME_C_API" at 0x0342B5C0>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x033E94B0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.math', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x033E94B0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\math.cp37-win32.pyd')"

