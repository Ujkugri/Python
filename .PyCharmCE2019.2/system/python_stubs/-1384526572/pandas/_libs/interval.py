# encoding: utf-8
# module pandas._libs.interval
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pandas\_libs\interval.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numbers as numbers # C:\Program Files (x86)\Python37-32\lib\numbers.py
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
from pandas._libs.tslibs.timestamps import Timestamp

from _operator import le, lt


# functions

def intervals_to_interval_bounds(*args, **kwargs): # real signature unknown
    """
    Parameters
        ----------
        intervals : ndarray
            object array of Intervals / nulls
    
        validate_closed: boolean, default True
            boolean indicating if all intervals must be closed on the same side.
            Mismatching closed will raise if True, else return None for closed.
    
        Returns
        -------
        tuples (left: ndarray object array,
                right: ndarray object array,
                closed: str)
    """
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Float32ClosedBothIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Float32ClosedLeftIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Float32ClosedNeitherIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Float32ClosedRightIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Float64ClosedBothIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Float64ClosedLeftIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Float64ClosedNeitherIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Float64ClosedRightIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int32ClosedBothIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int32ClosedLeftIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int32ClosedNeitherIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int32ClosedRightIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int64ClosedBothIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int64ClosedLeftIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int64ClosedNeitherIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int64ClosedRightIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_IntervalMixin(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_IntervalTree(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Uint64ClosedBothIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Uint64ClosedLeftIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Uint64ClosedNeitherIntervalNode(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Uint64ClosedRightIntervalNode(*args, **kwargs): # real signature unknown
    pass

# classes

class Float32ClosedBothIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895038>'


class Float32ClosedLeftIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09837FC8>'


class Float32ClosedNeitherIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895068>'


class Float32ClosedRightIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09837FE0>'


class Float64ClosedBothIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x098950F8>'


class Float64ClosedLeftIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895098>'


class Float64ClosedNeitherIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895128>'


class Float64ClosedRightIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x098950C8>'


class Int32ClosedBothIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x098951B8>'


class Int32ClosedLeftIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895158>'


class Int32ClosedNeitherIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x098951E8>'


class Int32ClosedRightIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895188>'


class Int64ClosedBothIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895278>'


class Int64ClosedLeftIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895218>'


class Int64ClosedNeitherIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x098952A8>'


class Int64ClosedRightIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895248>'


class IntervalMixin(object):
    # no doc
    def _check_closed_matches(self, *args, **kwargs): # real signature unknown
        """
        Check if the closed attribute of `other` matches.
        
                Note that 'left' and 'right' are considered different from 'both'.
        
                Parameters
                ----------
                other : Interval, IntervalIndex, IntervalArray
                name : str
                    Name to use for 'other' in the error message.
        
                Raises
                ------
                ValueError
                    When `other` is not closed exactly the same as self.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    closed_left = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """
        Check if the interval is closed on the left side.

        For the meaning of `closed` and `open` see :class:`~pandas.Interval`.

        Returns
        -------
        bool
            ``True`` if the Interval is closed on the left-side, else
            ``False``.
        """

    closed_right = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """
        Check if the interval is closed on the right side.

        For the meaning of `closed` and `open` see :class:`~pandas.Interval`.

        Returns
        -------
        bool
            ``True`` if the Interval is closed on the left-side, else
            ``False``.
        """

    length = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """Return the length of the Interval"""

    mid = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """
        Return the midpoint of the Interval
        """

    open_left = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """
        Check if the interval is open on the left side.

        For the meaning of `closed` and `open` see :class:`~pandas.Interval`.

        Returns
        -------
        bool
            ``True`` if the Interval is closed on the left-side, else
            ``False``.
        """

    open_right = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """
        Check if the interval is open on the right side.

        For the meaning of `closed` and `open` see :class:`~pandas.Interval`.

        Returns
        -------
        bool
            ``True`` if the Interval is closed on the left-side, else
            ``False``.
        """



class Interval(IntervalMixin):
    """
    Immutable object implementing an Interval, a bounded slice-like interval.
    
        .. versionadded:: 0.20.0
    
        Parameters
        ----------
        left : orderable scalar
            Left bound for the interval.
        right : orderable scalar
            Right bound for the interval.
        closed : {'left', 'right', 'both', 'neither'}, default 'right'
            Whether the interval is closed on the left-side, right-side, both or
            neither.
        closed : {'right', 'left', 'both', 'neither'}, default 'right'
            Whether the interval is closed on the left-side, right-side, both or
            neither. See the Notes for more detailed explanation.
    
        See Also
        --------
        IntervalIndex : An Index of Interval objects that are all closed on the
            same side.
        cut : Convert continuous data into discrete bins (Categorical
            of Interval objects).
        qcut : Convert continuous data into bins (Categorical of Interval objects)
            based on quantiles.
        Period : Represents a period of time.
    
        Notes
        -----
        The parameters `left` and `right` must be from the same type, you must be
        able to compare them and they must satisfy ``left <= right``.
    
        A closed interval (in mathematics denoted by square brackets) contains
        its endpoints, i.e. the closed interval ``[0, 5]`` is characterized by the
        conditions ``0 <= x <= 5``. This is what ``closed='both'`` stands for.
        An open interval (in mathematics denoted by parentheses) does not contain
        its endpoints, i.e. the open interval ``(0, 5)`` is characterized by the
        conditions ``0 < x < 5``. This is what ``closed='neither'`` stands for.
        Intervals can also be half-open or half-closed, i.e. ``[0, 5)`` is
        described by ``0 <= x < 5`` (``closed='left'``) and ``(0, 5]`` is
        described by ``0 < x <= 5`` (``closed='right'``).
    
        Examples
        --------
        It is possible to build Intervals of different types, like numeric ones:
    
        >>> iv = pd.Interval(left=0, right=5)
        >>> iv
        Interval(0, 5, closed='right')
    
        You can check if an element belongs to it
    
        >>> 2.5 in iv
        True
    
        You can test the bounds (``closed='right'``, so ``0 < x <= 5``):
    
        >>> 0 in iv
        False
        >>> 5 in iv
        True
        >>> 0.0001 in iv
        True
    
        Calculate its length
    
        >>> iv.length
        5
    
        You can operate with `+` and `*` over an Interval and the operation
        is applied to each of its bounds, so the result depends on the type
        of the bound elements
    
        >>> shifted_iv = iv + 3
        >>> shifted_iv
        Interval(3, 8, closed='right')
        >>> extended_iv = iv * 10.0
        >>> extended_iv
        Interval(0.0, 50.0, closed='right')
    
        To create a time interval you can use Timestamps as the bounds
    
        >>> year_2017 = pd.Interval(pd.Timestamp('2017-01-01 00:00:00'),
        ...                         pd.Timestamp('2018-01-01 00:00:00'),
        ...                         closed='left')
        >>> pd.Timestamp('2017-01-01 00:00') in year_2017
        True
        >>> year_2017.length
        Timedelta('365 days 00:00:00')
    
        And also you can create string intervals
    
        >>> volume_1 = pd.Interval('Ant', 'Dog', closed='both')
        >>> 'Bee' in volume_1
        True
    """
    def overlaps(self, i2): # real signature unknown; restored from __doc__
        """
        Check whether two Interval objects overlap.
        
                Two intervals overlap if they share a common point, including closed
                endpoints. Intervals that only have an open endpoint in common do not
                overlap.
        
                .. versionadded:: 0.24.0
        
                Parameters
                ----------
                other : Interval
                    The interval to check against for an overlap.
        
                Returns
                -------
                bool
                    ``True`` if the two intervals overlap, else ``False``.
        
                See Also
                --------
                IntervalArray.overlaps : The corresponding method for IntervalArray.
                IntervalIndex.overlaps : The corresponding method for IntervalIndex.
        
                Examples
                --------
                >>> i1 = pd.Interval(0, 2)
                >>> i2 = pd.Interval(1, 3)
                >>> i1.overlaps(i2)
                True
                >>> i3 = pd.Interval(4, 5)
                >>> i1.overlaps(i3)
                False
        
                Intervals that share closed endpoints overlap:
        
                >>> i4 = pd.Interval(0, 1, closed='both')
                >>> i5 = pd.Interval(1, 2, closed='both')
                >>> i4.overlaps(i5)
                True
        
                Intervals that only have an open endpoint in common do not overlap:
        
                >>> i6 = pd.Interval(1, 2, closed='neither')
                >>> i4.overlaps(i6)
                False
        """
        pass

    def _repr_base(self, *args, **kwargs): # real signature unknown
        pass

    def __add__(self, *args, **kwargs): # real signature unknown
        """ Return self+value. """
        pass

    def __contains__(self, *args, **kwargs): # real signature unknown
        """ Return key in self. """
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

    def __hash__(self, *args, **kwargs): # real signature unknown
        """ Return hash(self). """
        pass

    def __init__(self, left=0, right=5): # real signature unknown; restored from __doc__
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

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __ne__(self, *args, **kwargs): # real signature unknown
        """ Return self!=value. """
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

    def __str__(self, *args, **kwargs): # real signature unknown
        """ Return str(self). """
        pass

    def __sub__(self, *args, **kwargs): # real signature unknown
        """ Return self-value. """
        pass

    def __truediv__(self, *args, **kwargs): # real signature unknown
        """ Return self/value. """
        pass

    closed = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """
    Whether the interval is closed on the left-side, right-side, both or
    neither
    """

    left = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """Left bound for the interval"""

    right = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """Right bound for the interval"""


    _typ = 'interval'


class IntervalTree(IntervalMixin):
    """
    A centered interval tree
    
        Based off the algorithm described on Wikipedia:
        http://en.wikipedia.org/wiki/Interval_tree
    
        we are emulating the IndexEngine interface
    """
    def clear_mapping(self, *args, **kwargs): # real signature unknown
        pass

    def get_indexer(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to unique intervals that overlap
                with the given array of scalar targets.
        """
        pass

    def get_indexer_non_unique(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to intervals that overlap with
                the given array of scalar targets. Non-unique positions are repeated.
        """
        pass

    def get_loc(self, *args, **kwargs): # real signature unknown
        """
        Return all positions corresponding to intervals that overlap with
                the given scalar key
        """
        pass

    def get_loc_interval(self, *args, **kwargs): # real signature unknown
        """
        Lookup the intervals enclosed in the given interval bounds
        
                The given interval is presumed to have closed bounds.
        """
        pass

    def _get_partial_overlap(self, *args, **kwargs): # real signature unknown
        """
        Return all positions corresponding to intervals with the given side
                falling between the left and right bounds of an interval query
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        """
        Parameters
                ----------
                left, right : np.ndarray[ndim=1]
                    Left and right bounds for each interval. Assumed to contain no
                    NaNs.
                closed : {'left', 'right', 'both', 'neither'}, optional
                    Whether the intervals are closed on the left-side, right-side, both
                    or neither. Defaults to 'right'.
                leaf_size : int, optional
                    Parameter that controls when the tree switches from creating nodes
                    to brute-force search. Tune this parameter to optimize query
                    performance.
        """
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __pyx_fuse_0get_indexer(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to unique intervals that overlap
                with the given array of scalar targets.
        """
        pass

    def __pyx_fuse_0get_indexer_non_unique(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to intervals that overlap with
                the given array of scalar targets. Non-unique positions are repeated.
        """
        pass

    def __pyx_fuse_0get_loc(self, *args, **kwargs): # real signature unknown
        """
        Return all positions corresponding to intervals that overlap with
                the given scalar key
        """
        pass

    def __pyx_fuse_1get_indexer(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to unique intervals that overlap
                with the given array of scalar targets.
        """
        pass

    def __pyx_fuse_1get_indexer_non_unique(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to intervals that overlap with
                the given array of scalar targets. Non-unique positions are repeated.
        """
        pass

    def __pyx_fuse_1get_loc(self, *args, **kwargs): # real signature unknown
        """
        Return all positions corresponding to intervals that overlap with
                the given scalar key
        """
        pass

    def __pyx_fuse_2get_indexer(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to unique intervals that overlap
                with the given array of scalar targets.
        """
        pass

    def __pyx_fuse_2get_indexer_non_unique(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to intervals that overlap with
                the given array of scalar targets. Non-unique positions are repeated.
        """
        pass

    def __pyx_fuse_2get_loc(self, *args, **kwargs): # real signature unknown
        """
        Return all positions corresponding to intervals that overlap with
                the given scalar key
        """
        pass

    def __pyx_fuse_3get_indexer(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to unique intervals that overlap
                with the given array of scalar targets.
        """
        pass

    def __pyx_fuse_3get_indexer_non_unique(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to intervals that overlap with
                the given array of scalar targets. Non-unique positions are repeated.
        """
        pass

    def __pyx_fuse_3get_loc(self, *args, **kwargs): # real signature unknown
        """
        Return all positions corresponding to intervals that overlap with
                the given scalar key
        """
        pass

    def __pyx_fuse_4get_indexer(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to unique intervals that overlap
                with the given array of scalar targets.
        """
        pass

    def __pyx_fuse_4get_indexer_non_unique(self, *args, **kwargs): # real signature unknown
        """
        Return the positions corresponding to intervals that overlap with
                the given array of scalar targets. Non-unique positions are repeated.
        """
        pass

    def __pyx_fuse_4get_loc(self, *args, **kwargs): # real signature unknown
        """
        Return all positions corresponding to intervals that overlap with
                the given scalar key
        """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    closed = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    dtype = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    is_overlapping = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """
        Determine if the IntervalTree contains overlapping intervals.
        Cached as self._is_overlapping.
        """

    left = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    left_sorter = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """How to sort the left labels; this is used for binary search
        """

    right = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    right_sorter = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """How to sort the right labels
        """

    root = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default



class Uint64ClosedBothIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895338>'


class Uint64ClosedLeftIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x098952D8>'


class Uint64ClosedNeitherIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895368>'


class Uint64ClosedRightIntervalNode(object):
    """
    Non-terminal node for an IntervalTree
    
        Categorizes intervals by those that fall to the left, those that fall to
        the right, and those that overlap with the pivot.
    """
    def counts(self, *args, **kwargs): # real signature unknown
        """
        Inspect counts on this node
                useful for debugging purposes
        """
        pass

    def query(self, *args, **kwargs): # real signature unknown
        """
        Recursively query this node and its sub-nodes for intervals that
                overlap with the query point.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    is_leaf_node = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leaf_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_center = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_elements = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    pivot = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09895308>'


# variables with complex values

NODE_CLASSES = {
    (
        'float32',
        'both',
    ): 
        Float32ClosedBothIntervalNode
    ,
    (
        'float32',
        'left',
    ): 
        Float32ClosedLeftIntervalNode
    ,
    (
        'float32',
        'neither',
    ): 
        Float32ClosedNeitherIntervalNode
    ,
    (
        'float32',
        'right',
    ): 
        Float32ClosedRightIntervalNode
    ,
    (
        'float64',
        'both',
    ): 
        Float64ClosedBothIntervalNode
    ,
    (
        'float64',
        'left',
    ): 
        Float64ClosedLeftIntervalNode
    ,
    (
        'float64',
        'neither',
    ): 
        Float64ClosedNeitherIntervalNode
    ,
    (
        'float64',
        'right',
    ): 
        Float64ClosedRightIntervalNode
    ,
    (
        'int32',
        'both',
    ): 
        Int32ClosedBothIntervalNode
    ,
    (
        'int32',
        'left',
    ): 
        Int32ClosedLeftIntervalNode
    ,
    (
        'int32',
        'neither',
    ): 
        Int32ClosedNeitherIntervalNode
    ,
    (
        'int32',
        'right',
    ): 
        Int32ClosedRightIntervalNode
    ,
    (
        'int64',
        'both',
    ): 
        Int64ClosedBothIntervalNode
    ,
    (
        'int64',
        'left',
    ): 
        Int64ClosedLeftIntervalNode
    ,
    (
        'int64',
        'neither',
    ): 
        Int64ClosedNeitherIntervalNode
    ,
    (
        'int64',
        'right',
    ): 
        Int64ClosedRightIntervalNode
    ,
    (
        'uint64',
        'both',
    ): 
        Uint64ClosedBothIntervalNode
    ,
    (
        'uint64',
        'left',
    ): 
        Uint64ClosedLeftIntervalNode
    ,
    (
        'uint64',
        'neither',
    ): 
        Uint64ClosedNeitherIntervalNode
    ,
    (
        'uint64',
        'right',
    ): 
        Uint64ClosedRightIntervalNode
    ,
}

_VALID_CLOSED = None # (!) real value is "frozenset({'both', 'right', 'neither', 'left'})"

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0988C150>'

__spec__ = None # (!) real value is "ModuleSpec(name='pandas._libs.interval', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0988C150>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pandas\\\\_libs\\\\interval.cp37-win32.pyd')"

__test__ = {
    'Interval.overlaps (line 370)': "\n        Check whether two Interval objects overlap.\n\n        Two intervals overlap if they share a common point, including closed\n        endpoints. Intervals that only have an open endpoint in common do not\n        overlap.\n\n        .. versionadded:: 0.24.0\n\n        Parameters\n        ----------\n        other : Interval\n            The interval to check against for an overlap.\n\n        Returns\n        -------\n        bool\n            ``True`` if the two intervals overlap, else ``False``.\n\n        See Also\n        --------\n        IntervalArray.overlaps : The corresponding method for IntervalArray.\n        IntervalIndex.overlaps : The corresponding method for IntervalIndex.\n\n        Examples\n        --------\n        >>> i1 = pd.Interval(0, 2)\n        >>> i2 = pd.Interval(1, 3)\n        >>> i1.overlaps(i2)\n        True\n        >>> i3 = pd.Interval(4, 5)\n        >>> i1.overlaps(i3)\n        False\n\n        Intervals that share closed endpoints overlap:\n\n        >>> i4 = pd.Interval(0, 1, closed='both')\n        >>> i5 = pd.Interval(1, 2, closed='both')\n        >>> i4.overlaps(i5)\n        True\n\n        Intervals that only have an open endpoint in common do not overlap:\n\n        >>> i6 = pd.Interval(1, 2, closed='neither')\n        >>> i4.overlaps(i6)\n        False\n        ",
}

