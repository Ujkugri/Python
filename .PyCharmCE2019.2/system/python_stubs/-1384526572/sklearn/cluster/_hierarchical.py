# encoding: utf-8
# module sklearn.cluster._hierarchical
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\cluster\_hierarchical.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import numpy as __numpy


# functions

def average_merge(*args, **kwargs): # real signature unknown
    """
    Merge two IntFloatDicts with the average strategy: when the 
        same key is present in the two dicts, the weighted average of the two 
        values is used.
    
        Parameters
        ==========
        a, b : IntFloatDict object
            The IntFloatDicts to merge
        mask : ndarray array of dtype integer and of dimension 1
            a mask for keys to ignore: if not mask[key] the corresponding key
            is skipped in the output dictionary
        n_a, n_b : float
            n_a and n_b are weights for a and b for the merge strategy.
            They are used for a weighted mean.
    
        Returns
        =======
        out : IntFloatDict object
            The IntFloatDict resulting from the merge
    """
    pass

def compute_ward_dist(*args, **kwargs): # real signature unknown
    pass

def hc_get_heads(*args, **kwargs): # real signature unknown
    """
    Returns the heads of the forest, as defined by parents.
    
        Parameters
        ----------
        parents : array of integers
            The parent structure defining the forest (ensemble of trees)
        copy : boolean
            If copy is False, the input 'parents' array is modified inplace
    
        Returns
        -------
        heads : array of integers of same shape as parents
            The indices in the 'parents' of the tree heads
    """
    pass

def max_merge(*args, **kwargs): # real signature unknown
    """
    Merge two IntFloatDicts with the max strategy: when the same key is
        present in the two dicts, the max of the two values is used.
    
        Parameters
        ==========
        a, b : IntFloatDict object
            The IntFloatDicts to merge
        mask : ndarray array of dtype integer and of dimension 1
            a mask for keys to ignore: if not mask[key] the corresponding key
            is skipped in the output dictionary
        n_a, n_b : float
            n_a and n_b are weights for a and b for the merge strategy.
            They are not used in the case of a max merge.
    
        Returns
        =======
        out : IntFloatDict object
            The IntFloatDict resulting from the merge
    """
    pass

def single_linkage_label(*args, **kwargs): # real signature unknown
    """
    Convert an linkage array or MST to a tree by labelling clusters at merges.
        This is done by using a Union find structure to keep track of merges
        efficiently.
    
        Parameters
        ----------
        L: array of shape (n_samples - 1, 3)
            The linkage array or MST where each row specifies two samples
            to be merged and a distance or weight at which the merge occurs. This
             array is assumed to be sorted by the distance/weight.
    
        Returns
        -------
        A tree in the format used by scipy.cluster.hierarchy.
    """
    pass

def _get_parents(*args, **kwargs): # real signature unknown
    """
    Returns the heads of the given nodes, as defined by parents.
    
        Modifies 'heads' and 'not_visited' in-place.
    
        Parameters
        ----------
        nodes : list of integers
            The nodes to start from
        heads : list of integers
            A list to hold the results (modified inplace)
        parents : array of integers
            The parent structure defining the tree
        not_visited
            The tree nodes to consider (modified inplace)
    """
    pass

def _hc_get_descendent(*args, **kwargs): # real signature unknown
    """
    Function returning all the descendent leaves of a set of nodes in the tree.
    
        Parameters
        ----------
        node : integer
            The node for which we want the descendents.
    
        children : list of pairs, length n_nodes
            The children of each non-leaf node. Values less than `n_samples` refer
            to leaves of the tree. A greater value `i` indicates a node with
            children `children[i - n_samples]`.
    
        n_leaves : integer
            Number of leaves.
    
        Returns
        -------
        descendent : list of int
    """
    pass

def _single_linkage_label(*args, **kwargs): # real signature unknown
    """
    Convert an linkage array or MST to a tree by labelling clusters at merges.
        This is done by using a Union find structure to keep track of merges
        efficiently. This is the private version of the function that assumes that
        ``L`` has been properly validated. See ``single_linkage_label`` for the
        user facing version of this function.
    
        Parameters
        ----------
        L: array of shape (n_samples - 1, 3)
            The linkage array or MST where each row specifies two samples
            to be merged and a distance or weight at which the merge occurs. This
             array is assumed to be sorted by the distance/weight.
    
        Returns
        -------
        A tree in the format used by scipy.cluster.hierarchy.
    """
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_UnionFind(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_WeightedEdge(*args, **kwargs): # real signature unknown
    pass

# classes

class DTYPE(__numpy.floating, float):
    """
    Double-precision floating-point number type, compatible with Python `float`
        and C ``double``.
        Character code: ``'d'``.
        Canonical name: ``np.double``.
        Alias: ``np.float_``.
        Alias *on this platform*: ``np.float64``: 64-bit precision floating-point number type: sign bit, 11 bits exponent, 52 bits mantissa.
    """
    def __abs__(self, *args, **kwargs): # real signature unknown
        """ abs(self) """
        pass

    def __add__(self, *args, **kwargs): # real signature unknown
        """ Return self+value. """
        pass

    def __bool__(self, *args, **kwargs): # real signature unknown
        """ self != 0 """
        pass

    def __divmod__(self, *args, **kwargs): # real signature unknown
        """ Return divmod(self, value). """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
        pass

    def __float__(self, *args, **kwargs): # real signature unknown
        """ float(self) """
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

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    def __int__(self, *args, **kwargs): # real signature unknown
        """ int(self) """
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

    def __pow__(self, *args, **kwargs): # real signature unknown
        """ Return pow(self, value, mod). """
        pass

    def __radd__(self, *args, **kwargs): # real signature unknown
        """ Return value+self. """
        pass

    def __rdivmod__(self, *args, **kwargs): # real signature unknown
        """ Return divmod(value, self). """
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
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

    def __str__(self, *args, **kwargs): # real signature unknown
        """ Return str(self). """
        pass

    def __sub__(self, *args, **kwargs): # real signature unknown
        """ Return self-value. """
        pass

    def __truediv__(self, *args, **kwargs): # real signature unknown
        """ Return self/value. """
        pass


class ITYPE(__numpy.signedinteger):
    """
    Signed integer type, compatible with C ``int``.
        Character code: ``'i'``.
        Canonical name: ``np.intc``.
        Alias *on this platform*: ``np.intp``: Signed integer large enough to fit pointer, compatible with C ``intptr_t``.
    """
    def __abs__(self, *args, **kwargs): # real signature unknown
        """ abs(self) """
        pass

    def __add__(self, *args, **kwargs): # real signature unknown
        """ Return self+value. """
        pass

    def __and__(self, *args, **kwargs): # real signature unknown
        """ Return self&value. """
        pass

    def __bool__(self, *args, **kwargs): # real signature unknown
        """ self != 0 """
        pass

    def __divmod__(self, *args, **kwargs): # real signature unknown
        """ Return divmod(self, value). """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
        pass

    def __float__(self, *args, **kwargs): # real signature unknown
        """ float(self) """
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

    def __index__(self, *args, **kwargs): # real signature unknown
        """ Return self converted to an integer, if self is suitable for use as an index into a list. """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    def __int__(self, *args, **kwargs): # real signature unknown
        """ int(self) """
        pass

    def __invert__(self, *args, **kwargs): # real signature unknown
        """ ~self """
        pass

    def __le__(self, *args, **kwargs): # real signature unknown
        """ Return self<=value. """
        pass

    def __lshift__(self, *args, **kwargs): # real signature unknown
        """ Return self<<value. """
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

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __ne__(self, *args, **kwargs): # real signature unknown
        """ Return self!=value. """
        pass

    def __or__(self, *args, **kwargs): # real signature unknown
        """ Return self|value. """
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

    def __rand__(self, *args, **kwargs): # real signature unknown
        """ Return value&self. """
        pass

    def __rdivmod__(self, *args, **kwargs): # real signature unknown
        """ Return divmod(value, self). """
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __rfloordiv__(self, *args, **kwargs): # real signature unknown
        """ Return value//self. """
        pass

    def __rlshift__(self, *args, **kwargs): # real signature unknown
        """ Return value<<self. """
        pass

    def __rmod__(self, *args, **kwargs): # real signature unknown
        """ Return value%self. """
        pass

    def __rmul__(self, *args, **kwargs): # real signature unknown
        """ Return value*self. """
        pass

    def __ror__(self, *args, **kwargs): # real signature unknown
        """ Return value|self. """
        pass

    def __rpow__(self, *args, **kwargs): # real signature unknown
        """ Return pow(value, self, mod). """
        pass

    def __rrshift__(self, *args, **kwargs): # real signature unknown
        """ Return value>>self. """
        pass

    def __rshift__(self, *args, **kwargs): # real signature unknown
        """ Return self>>value. """
        pass

    def __rsub__(self, *args, **kwargs): # real signature unknown
        """ Return value-self. """
        pass

    def __rtruediv__(self, *args, **kwargs): # real signature unknown
        """ Return value/self. """
        pass

    def __rxor__(self, *args, **kwargs): # real signature unknown
        """ Return value^self. """
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

    def __xor__(self, *args, **kwargs): # real signature unknown
        """ Return self^value. """
        pass


class UnionFind(object):
    # no doc
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x10CE1398>'


class WeightedEdge(object):
    # no doc
    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
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

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    a = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    b = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    weight = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __hash__ = None


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x10CF38F0>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.cluster._hierarchical', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x10CF38F0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\cluster\\\\_hierarchical.cp37-win32.pyd')"

__test__ = {}

