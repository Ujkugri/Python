# encoding: utf-8
# module sklearn.neighbors.dist_metrics
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\neighbors\dist_metrics.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import numpy as __numpy


# functions

def get_valid_metric_ids(EuclideanDistance=None, ManhattanDistance=None): # real signature unknown; restored from __doc__
    """
    Given an iterable of metric class names or class identifiers,
        return a list of metric IDs which map to those classes.
    
        Example:
        >>> L = get_valid_metric_ids([EuclideanDistance, 'ManhattanDistance'])
        >>> sorted(L)
        ['cityblock', 'euclidean', 'l1', 'l2', 'manhattan']
    """
    pass

def newObj(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# classes

class DistanceMetric(object):
    """
    DistanceMetric class
    
        This class provides a uniform interface to fast distance metric
        functions.  The various metrics can be accessed via the `get_metric`
        class method and the metric string identifier (see below).
        For example, to use the Euclidean distance:
    
        >>> dist = DistanceMetric.get_metric('euclidean')
        >>> X = [[0, 1, 2],
                 [3, 4, 5]]
        >>> dist.pairwise(X)
        array([[ 0.        ,  5.19615242],
               [ 5.19615242,  0.        ]])
    
        Available Metrics
        The following lists the string metric identifiers and the associated
        distance metric classes:
    
        **Metrics intended for real-valued vector spaces:**
    
        ==============  ====================  ========  ===============================
        identifier      class name            args      distance function
        --------------  --------------------  --------  -------------------------------
        "euclidean"     EuclideanDistance     -         ``sqrt(sum((x - y)^2))``
        "manhattan"     ManhattanDistance     -         ``sum(|x - y|)``
        "chebyshev"     ChebyshevDistance     -         ``max(|x - y|)``
        "minkowski"     MinkowskiDistance     p         ``sum(|x - y|^p)^(1/p)``
        "wminkowski"    WMinkowskiDistance    p, w      ``sum(|w * (x - y)|^p)^(1/p)``
        "seuclidean"    SEuclideanDistance    V         ``sqrt(sum((x - y)^2 / V))``
        "mahalanobis"   MahalanobisDistance   V or VI   ``sqrt((x - y)' V^-1 (x - y))``
        ==============  ====================  ========  ===============================
    
        **Metrics intended for two-dimensional vector spaces:**  Note that the haversine
        distance metric requires data in the form of [latitude, longitude] and both
        inputs and outputs are in units of radians.
    
        ============  ==================  ===============================================================
        identifier    class name          distance function
        ------------  ------------------  ---------------------------------------------------------------
        "haversine"   HaversineDistance   ``2 arcsin(sqrt(sin^2(0.5*dx) + cos(x1)cos(x2)sin^2(0.5*dy)))``
        ============  ==================  ===============================================================
    
    
        **Metrics intended for integer-valued vector spaces:**  Though intended
        for integer-valued vectors, these are also valid metrics in the case of
        real-valued vectors.
    
        =============  ====================  ========================================
        identifier     class name            distance function
        -------------  --------------------  ----------------------------------------
        "hamming"      HammingDistance       ``N_unequal(x, y) / N_tot``
        "canberra"     CanberraDistance      ``sum(|x - y| / (|x| + |y|))``
        "braycurtis"   BrayCurtisDistance    ``sum(|x - y|) / (sum(|x|) + sum(|y|))``
        =============  ====================  ========================================
    
        **Metrics intended for boolean-valued vector spaces:**  Any nonzero entry
        is evaluated to "True".  In the listings below, the following
        abbreviations are used:
    
         - N  : number of dimensions
         - NTT : number of dims in which both values are True
         - NTF : number of dims in which the first value is True, second is False
         - NFT : number of dims in which the first value is False, second is True
         - NFF : number of dims in which both values are False
         - NNEQ : number of non-equal dimensions, NNEQ = NTF + NFT
         - NNZ : number of nonzero dimensions, NNZ = NTF + NFT + NTT
    
        =================  =======================  ===============================
        identifier         class name               distance function
        -----------------  -----------------------  -------------------------------
        "jaccard"          JaccardDistance          NNEQ / NNZ
        "matching"         MatchingDistance         NNEQ / N
        "dice"             DiceDistance             NNEQ / (NTT + NNZ)
        "kulsinski"        KulsinskiDistance        (NNEQ + N - NTT) / (NNEQ + N)
        "rogerstanimoto"   RogersTanimotoDistance   2 * NNEQ / (N + NNEQ)
        "russellrao"       RussellRaoDistance       NNZ / N
        "sokalmichener"    SokalMichenerDistance    2 * NNEQ / (N + NNEQ)
        "sokalsneath"      SokalSneathDistance      NNEQ / (NNEQ + 0.5 * NTT)
        =================  =======================  ===============================
    
        **User-defined distance:**
    
        ===========    ===============    =======
        identifier     class name         args
        -----------    ---------------    -------
        "pyfunc"       PyFuncDistance     func
        ===========    ===============    =======
    
        Here ``func`` is a function which takes two one-dimensional numpy
        arrays, and returns a distance.  Note that in order to be used within
        the BallTree, the distance must be a true metric:
        i.e. it must satisfy the following properties
    
        1) Non-negativity: d(x, y) >= 0
        2) Identity: d(x, y) = 0 if and only if x == y
        3) Symmetry: d(x, y) = d(y, x)
        4) Triangle Inequality: d(x, y) + d(y, z) >= d(x, z)
    
        Because of the Python object overhead involved in calling the python
        function, this will be fairly slow, but it will have the same
        scaling as other distances.
    """
    def dist_to_rdist(self, *args, **kwargs): # real signature unknown
        """
        Convert the true distance to the reduced distance.
        
                The reduced distance, defined for some metrics, is a computationally
                more efficient measure which preserves the rank of the true distance.
                For example, in the Euclidean distance metric, the reduced distance
                is the squared-euclidean distance.
        """
        pass

    @classmethod
    def get_metric(cls, *args, **kwargs): # real signature unknown
        """
        Get the given distance metric from the string identifier.
        
                See the docstring of DistanceMetric for a list of available metrics.
        
                Parameters
                ----------
                metric : string or class name
                    The distance metric to use
                **kwargs
                    additional arguments will be passed to the requested metric
        """
        pass

    def pairwise(self, *args, **kwargs): # real signature unknown
        """
        Compute the pairwise distances between X and Y
        
                This is a convenience routine for the sake of testing.  For many
                metrics, the utilities in scipy.spatial.distance.cdist and
                scipy.spatial.distance.pdist will be faster.
        
                Parameters
                ----------
                X : array_like
                    Array of shape (Nx, D), representing Nx points in D dimensions.
                Y : array_like (optional)
                    Array of shape (Ny, D), representing Ny points in D dimensions.
                    If not specified, then Y=X.
                Returns
                -------
                dist : ndarray
                    The shape (Nx, Ny) array of pairwise distances between points in
                    X and Y.
        """
        pass

    def rdist_to_dist(self, *args, **kwargs): # real signature unknown
        """
        Convert the Reduced distance to the true distance.
        
                The reduced distance, defined for some metrics, is a computationally
                more efficient measure which preserves the rank of the true distance.
                For example, in the Euclidean distance metric, the reduced distance
                is the squared-euclidean distance.
        """
        pass

    def __getstate__(self, *args, **kwargs): # real signature unknown
        """ get state for pickling """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        """ reduce method used for pickling """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        """ set state for pickling """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0A002A88>'


class BrayCurtisDistance(DistanceMetric):
    """
    Bray-Curtis Distance
    
        Bray-Curtis distance is meant for discrete-valued vectors, though it is
        a valid metric for real-valued vectors.
    
        .. math::
           D(x, y) = \frac{\sum_i |x_i - y_i|}{\sum_i(|x_i| + |y_i|)}
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF3308>'


class CanberraDistance(DistanceMetric):
    """
    Canberra Distance
    
        Canberra distance is meant for discrete-valued vectors, though it is
        a valid metric for real-valued vectors.
    
        .. math::
           D(x, y) = \sum_i \frac{|x_i - y_i|}{|x_i| + |y_i|}
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF32D8>'


class ChebyshevDistance(DistanceMetric):
    """
    Chebyshev/Infinity Distance
    
        .. math::
           D(x, y) = max_i (|x_i - y_i|)
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF31E8>'


class DiceDistance(DistanceMetric):
    """
    Dice Distance
    
        Dice Distance is a dissimilarity measure for boolean-valued
        vectors. All nonzero entries will be treated as True, zero entries will
        be treated as False.
    
        .. math::
           D(x, y) = \frac{N_{TF} + N_{FT}}{2 * N_{TT} + N_{TF} + N_{FT}}
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF3398>'


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


class EuclideanDistance(DistanceMetric):
    """
    Euclidean Distance metric
    
        .. math::
           D(x, y) = \sqrt{ \sum_i (x_i - y_i) ^ 2 }
    """
    def dist_to_rdist(self, *args, **kwargs): # real signature unknown
        pass

    def rdist_to_dist(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0A034FE0>'


class HammingDistance(DistanceMetric):
    """
    Hamming Distance
    
        Hamming distance is meant for discrete-valued vectors, though it is
        a valid metric for real-valued vectors.
    
        .. math::
           D(x, y) = \frac{1}{N} \sum_i \delta_{x_i, y_i}
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF32A8>'


class HaversineDistance(DistanceMetric):
    """
    Haversine (Spherical) Distance
    
        The Haversine distance is the angular distance between two points on
        the surface of a sphere.  The first distance of each point is assumed
        to be the latitude, the second is the longitude, given in radians.
        The dimension of the points must be 2:
    
        .. math::
           D(x, y) = 2\arcsin[\sqrt{\sin^2((x1 - y1) / 2)
                                    + \cos(x1)\cos(y1)\sin^2((x2 - y2) / 2)}]
    """
    def dist_to_rdist(self, *args, **kwargs): # real signature unknown
        pass

    def rdist_to_dist(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF34B8>'


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


class JaccardDistance(DistanceMetric):
    """
    Jaccard Distance
    
        Jaccard Distance is a dissimilarity measure for boolean-valued
        vectors. All nonzero entries will be treated as True, zero entries will
        be treated as False.
    
        .. math::
           D(x, y) = \frac{N_{TF} + N_{FT}}{N_{TT} + N_{TF} + N_{FT}}
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF3338>'


class KulsinskiDistance(DistanceMetric):
    """
    Kulsinski Distance
    
        Kulsinski Distance is a dissimilarity measure for boolean-valued
        vectors. All nonzero entries will be treated as True, zero entries will
        be treated as False.
    
        .. math::
           D(x, y) = 1 - \frac{N_{TT}}{N + N_{TF} + N_{FT}}
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF33C8>'


class MahalanobisDistance(DistanceMetric):
    """
    Mahalanobis Distance
    
        .. math::
           D(x, y) = \sqrt{ (x - y)^T V^{-1} (x - y) }
    
        Parameters
        ----------
        V : array_like
            Symmetric positive-definite covariance matrix.
            The inverse of this matrix will be explicitly computed.
        VI : array_like
            optionally specify the inverse directly.  If VI is passed,
            then V is not referenced.
    """
    def dist_to_rdist(self, *args, **kwargs): # real signature unknown
        pass

    def rdist_to_dist(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF3278>'


class ManhattanDistance(DistanceMetric):
    """
    Manhattan/City-block Distance metric
    
        .. math::
           D(x, y) = \sum_i |x_i - y_i|
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FCCB980>'


class MatchingDistance(DistanceMetric):
    """
    Matching Distance
    
        Matching Distance is a dissimilarity measure for boolean-valued
        vectors. All nonzero entries will be treated as True, zero entries will
        be treated as False.
    
        .. math::
           D(x, y) = \frac{N_{TF} + N_{FT}}{N}
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF3368>'


class MinkowskiDistance(DistanceMetric):
    """
    Minkowski Distance
    
        .. math::
           D(x, y) = [\sum_i (x_i - y_i)^p] ^ (1/p)
    
        Minkowski Distance requires p >= 1 and finite. For p = infinity,
        use ChebyshevDistance.
        Note that for p=1, ManhattanDistance is more efficient, and for
        p=2, EuclideanDistance is more efficient.
    """
    def dist_to_rdist(self, *args, **kwargs): # real signature unknown
        pass

    def rdist_to_dist(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF3218>'


class PyFuncDistance(DistanceMetric):
    """
    PyFunc Distance
    
        A user-defined distance
    
        Parameters
        ----------
        func : function
            func should take two numpy arrays as input, and return a distance.
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF34E8>'


class RogersTanimotoDistance(DistanceMetric):
    """
    Rogers-Tanimoto Distance
    
        Rogers-Tanimoto Distance is a dissimilarity measure for boolean-valued
        vectors. All nonzero entries will be treated as True, zero entries will
        be treated as False.
    
        .. math::
           D(x, y) = \frac{2 (N_{TF} + N_{FT})}{N + N_{TF} + N_{FT}}
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF33F8>'


class RussellRaoDistance(DistanceMetric):
    """
    Russell-Rao Distance
    
        Russell-Rao Distance is a dissimilarity measure for boolean-valued
        vectors. All nonzero entries will be treated as True, zero entries will
        be treated as False.
    
        .. math::
           D(x, y) = \frac{N - N_{TT}}{N}
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF3428>'


class SEuclideanDistance(DistanceMetric):
    """
    Standardized Euclidean Distance metric
    
        .. math::
           D(x, y) = \sqrt{ \sum_i \frac{ (x_i - y_i) ^ 2}{V_i} }
    """
    def dist_to_rdist(self, *args, **kwargs): # real signature unknown
        pass

    def rdist_to_dist(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0A034F38>'


class SokalMichenerDistance(DistanceMetric):
    """
    Sokal-Michener Distance
    
        Sokal-Michener Distance is a dissimilarity measure for boolean-valued
        vectors. All nonzero entries will be treated as True, zero entries will
        be treated as False.
    
        .. math::
           D(x, y) = \frac{2 (N_{TF} + N_{FT})}{N + N_{TF} + N_{FT}}
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF3458>'


class SokalSneathDistance(DistanceMetric):
    """
    Sokal-Sneath Distance
    
        Sokal-Sneath Distance is a dissimilarity measure for boolean-valued
        vectors. All nonzero entries will be treated as True, zero entries will
        be treated as False.
    
        .. math::
           D(x, y) = \frac{N_{TF} + N_{FT}}{N_{TT} / 2 + N_{TF} + N_{FT}}
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF3488>'


class WMinkowskiDistance(DistanceMetric):
    """
    Weighted Minkowski Distance
    
        .. math::
           D(x, y) = [\sum_i |w_i * (x_i - y_i)|^p] ^ (1/p)
    
        Weighted Minkowski Distance requires p >= 1 and finite.
    
        Parameters
        ----------
        p : int
            The order of the norm of the difference :math:`{||u-v||}_p`.
        w : (N,) array_like
            The weight vector.
    """
    def dist_to_rdist(self, *args, **kwargs): # real signature unknown
        pass

    def rdist_to_dist(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0FEF3248>'


# variables with complex values

METRIC_MAPPING = {
    'braycurtis': BrayCurtisDistance,
    'canberra': CanberraDistance,
    'chebyshev': ChebyshevDistance,
    'cityblock': ManhattanDistance,
    'dice': DiceDistance,
    'euclidean': EuclideanDistance,
    'hamming': HammingDistance,
    'haversine': HaversineDistance,
    'infinity': '<value is a self-reference, replaced by this string>',
    'jaccard': JaccardDistance,
    'kulsinski': KulsinskiDistance,
    'l1': '<value is a self-reference, replaced by this string>',
    'l2': '<value is a self-reference, replaced by this string>',
    'mahalanobis': MahalanobisDistance,
    'manhattan': '<value is a self-reference, replaced by this string>',
    'matching': MatchingDistance,
    'minkowski': MinkowskiDistance,
    'p': '<value is a self-reference, replaced by this string>',
    'pyfunc': PyFuncDistance,
    'rogerstanimoto': RogersTanimotoDistance,
    'russellrao': RussellRaoDistance,
    'seuclidean': SEuclideanDistance,
    'sokalmichener': SokalMichenerDistance,
    'sokalsneath': SokalSneathDistance,
    'wminkowski': WMinkowskiDistance,
}

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0FEFF950>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.neighbors.dist_metrics', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0FEFF950>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\neighbors\\\\dist_metrics.cp37-win32.pyd')"

__test__ = {
    'get_valid_metric_ids (line 92)': "Given an iterable of metric class names or class identifiers,\n    return a list of metric IDs which map to those classes.\n\n    Example:\n    >>> L = get_valid_metric_ids([EuclideanDistance, 'ManhattanDistance'])\n    >>> sorted(L)\n    ['cityblock', 'euclidean', 'l1', 'l2', 'manhattan']\n    ",
}

