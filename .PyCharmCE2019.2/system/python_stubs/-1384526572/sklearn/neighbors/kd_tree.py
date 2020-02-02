# encoding: utf-8
# module sklearn.neighbors.kd_tree
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\neighbors\kd_tree.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import warnings as warnings # C:\Program Files (x86)\Python37-32\lib\warnings.py
from sklearn.neighbors.dist_metrics import get_valid_metric_ids

import numpy as __numpy


# Variables with simple values

CLASS_DOC = "{BinaryTree} for fast generalized N-point problems\n\n{BinaryTree}(X, leaf_size=40, metric='minkowski', \\**kwargs)\n\nParameters\n----------\nX : array-like, shape = [n_samples, n_features]\n    n_samples is the number of points in the data set, and\n    n_features is the dimension of the parameter space.\n    Note: if X is a C-contiguous array of doubles then data will\n    not be copied. Otherwise, an internal copy will be made.\n\nleaf_size : positive integer (default = 40)\n    Number of points at which to switch to brute-force. Changing\n    leaf_size will not affect the results of a query, but can\n    significantly impact the speed of a query and the memory required\n    to store the constructed tree.  The amount of memory needed to\n    store the tree scales as approximately n_samples / leaf_size.\n    For a specified ``leaf_size``, a leaf node is guaranteed to\n    satisfy ``leaf_size <= n_points <= 2 * leaf_size``, except in\n    the case that ``n_samples < leaf_size``.\n\nmetric : string or DistanceMetric object\n    the distance metric to use for the tree.  Default='minkowski'\n    with p=2 (that is, a euclidean metric). See the documentation\n    of the DistanceMetric class for a list of available metrics.\n    {binary_tree}.valid_metrics gives a list of the metrics which\n    are valid for {BinaryTree}.\n\nAdditional keywords are passed to the distance metric class.\n\nAttributes\n----------\ndata : memory view\n    The training data\n\nExamples\n--------\nQuery for k-nearest neighbors\n\n    >>> import numpy as np\n    >>> rng = np.random.RandomState(0)\n    >>> X = rng.random_sample((10, 3))  # 10 points in 3 dimensions\n    >>> tree = {BinaryTree}(X, leaf_size=2)              # doctest: +SKIP\n    >>> dist, ind = tree.query(X[:1], k=3)                # doctest: +SKIP\n    >>> print(ind)  # indices of 3 closest neighbors\n    [0 3 1]\n    >>> print(dist)  # distances to 3 closest neighbors\n    [ 0.          0.19662693  0.29473397]\n\nPickle and Unpickle a tree.  Note that the state of the tree is saved in the\npickle operation: the tree needs not be rebuilt upon unpickling.\n\n    >>> import numpy as np\n    >>> import pickle\n    >>> rng = np.random.RandomState(0)\n    >>> X = rng.random_sample((10, 3))  # 10 points in 3 dimensions\n    >>> tree = {BinaryTree}(X, leaf_size=2)        # doctest: +SKIP\n    >>> s = pickle.dumps(tree)                     # doctest: +SKIP\n    >>> tree_copy = pickle.loads(s)                # doctest: +SKIP\n    >>> dist, ind = tree_copy.query(X[:1], k=3)     # doctest: +SKIP\n    >>> print(ind)  # indices of 3 closest neighbors\n    [0 3 1]\n    >>> print(dist)  # distances to 3 closest neighbors\n    [ 0.          0.19662693  0.29473397]\n\nQuery for neighbors within a given radius\n\n    >>> import numpy as np\n    >>> rng = np.random.RandomState(0)\n    >>> X = rng.random_sample((10, 3))  # 10 points in 3 dimensions\n    >>> tree = {BinaryTree}(X, leaf_size=2)     # doctest: +SKIP\n    >>> print(tree.query_radius(X[:1], r=0.3, count_only=True))\n    3\n    >>> ind = tree.query_radius(X[:1], r=0.3)  # doctest: +SKIP\n    >>> print(ind)  # indices of neighbors within distance 0.3\n    [3 0 1]\n\n\nCompute a gaussian kernel density estimate:\n\n    >>> import numpy as np\n    >>> rng = np.random.RandomState(42)\n    >>> X = rng.random_sample((100, 3))\n    >>> tree = {BinaryTree}(X)                # doctest: +SKIP\n    >>> tree.kernel_density(X[:3], h=0.1, kernel='gaussian')\n    array([ 6.94114649,  7.83281226,  7.2071716 ])\n\nCompute a two-point auto-correlation function\n\n    >>> import numpy as np\n    >>> rng = np.random.RandomState(0)\n    >>> X = rng.random_sample((30, 3))\n    >>> r = np.linspace(0, 1, 5)\n    >>> tree = {BinaryTree}(X)                # doctest: +SKIP\n    >>> tree.two_point_correlation(X, r)\n    array([ 30,  62, 278, 580, 820])\n\n"

# functions

def check_array(array, accept_sparse=False, accept_large_sparse=True, dtype=None, order=None, copy=False, force_all_finite=True, ensure_2d=True, allow_nd=False, ensure_min_samples=1, ensure_min_features=1, warn_on_dtype=None, estimator=None): # reliably restored by inspect
    """
    Input validation on an array, list, sparse matrix or similar.
    
        By default, the input is checked to be a non-empty 2D array containing
        only finite values. If the dtype of the array is object, attempt
        converting to float, raising on failure.
    
        Parameters
        ----------
        array : object
            Input object to check / convert.
    
        accept_sparse : string, boolean or list/tuple of strings (default=False)
            String[s] representing allowed sparse matrix formats, such as 'csc',
            'csr', etc. If the input is sparse but not in the allowed format,
            it will be converted to the first listed format. True allows the input
            to be any format. False means that a sparse matrix input will
            raise an error.
    
        accept_large_sparse : bool (default=True)
            If a CSR, CSC, COO or BSR sparse matrix is supplied and accepted by
            accept_sparse, accept_large_sparse=False will cause it to be accepted
            only if its indices are stored with a 32-bit dtype.
    
            .. versionadded:: 0.20
    
        dtype : string, type, list of types or None (default="numeric")
            Data type of result. If None, the dtype of the input is preserved.
            If "numeric", dtype is preserved unless array.dtype is object.
            If dtype is a list of types, conversion on the first type is only
            performed if the dtype of the input is not in the list.
    
        order : 'F', 'C' or None (default=None)
            Whether an array will be forced to be fortran or c-style.
            When order is None (default), then if copy=False, nothing is ensured
            about the memory layout of the output array; otherwise (copy=True)
            the memory layout of the returned array is kept as close as possible
            to the original array.
    
        copy : boolean (default=False)
            Whether a forced copy will be triggered. If copy=False, a copy might
            be triggered by a conversion.
    
        force_all_finite : boolean or 'allow-nan', (default=True)
            Whether to raise an error on np.inf and np.nan in array. The
            possibilities are:
    
            - True: Force all values of array to be finite.
            - False: accept both np.inf and np.nan in array.
            - 'allow-nan': accept only np.nan values in array. Values cannot
              be infinite.
    
            For object dtyped data, only np.nan is checked and not np.inf.
    
            .. versionadded:: 0.20
               ``force_all_finite`` accepts the string ``'allow-nan'``.
    
        ensure_2d : boolean (default=True)
            Whether to raise a value error if array is not 2D.
    
        allow_nd : boolean (default=False)
            Whether to allow array.ndim > 2.
    
        ensure_min_samples : int (default=1)
            Make sure that the array has a minimum number of samples in its first
            axis (rows for a 2D array). Setting to 0 disables this check.
    
        ensure_min_features : int (default=1)
            Make sure that the 2D array has some minimum number of features
            (columns). The default value of 1 rejects empty datasets.
            This check is only enforced when the input data has effectively 2
            dimensions or is originally 1D and ``ensure_2d`` is True. Setting to 0
            disables this check.
    
        warn_on_dtype : boolean or None, optional (default=None)
            Raise DataConversionWarning if the dtype of the input data structure
            does not match the requested dtype, causing a memory copy.
    
            .. deprecated:: 0.21
                ``warn_on_dtype`` is deprecated in version 0.21 and will be
                removed in 0.23.
    
        estimator : str or estimator instance (default=None)
            If passed, include the name of the estimator in warning messages.
    
        Returns
        -------
        array_converted : object
            The converted and validated array.
    """
    pass

def kernel_norm(*args, **kwargs): # real signature unknown
    """
    Given a string specification of a kernel, compute the normalization.
    
        Parameters
        ----------
        h : float
            the bandwidth of the kernel
        d : int
            the dimension of the space in which the kernel norm is computed
        kernel : string
            The kernel identifier.  Must be one of
            ['gaussian'|'tophat'|'epanechnikov'|
             'exponential'|'linear'|'cosine']
        return_log : boolean
            if True, return the log of the kernel norm.  Otherwise, return the
            kernel norm.
        Returns
        -------
        knorm or log_knorm : float
            the kernel norm or logarithm of the kernel norm.
    """
    pass

def load_heap(*args, **kwargs): # real signature unknown
    """ test fully loading the heap """
    pass

def newObj(*args, **kwargs): # real signature unknown
    pass

def nodeheap_sort(*args, **kwargs): # real signature unknown
    """ In-place reverse sort of vals using NodeHeap """
    pass

def simultaneous_sort(*args, **kwargs): # real signature unknown
    """
    In-place simultaneous sort the given row of the arrays
    
        This python wrapper exists primarily to enable unit testing
        of the _simultaneous_sort C routine.
    """
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# classes

class BinaryTree(object):
    # no doc
    def get_arrays(self, *args, **kwargs): # real signature unknown
        pass

    def get_n_calls(self, *args, **kwargs): # real signature unknown
        pass

    def get_tree_stats(self, *args, **kwargs): # real signature unknown
        pass

    def kernel_density(self, X, h, kernel='gaussian', atol=0, rtol=1, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        kernel_density(self, X, h, kernel='gaussian', atol=0, rtol=1E-8,
                               breadth_first=True, return_log=False)
        
                Compute the kernel density estimate at points X with the given kernel,
                using the distance metric specified at tree creation.
        
                Parameters
                ----------
                X : array-like, shape = [n_samples, n_features]
                    An array of points to query.  Last dimension should match dimension
                    of training data.
                h : float
                    the bandwidth of the kernel
                kernel : string
                    specify the kernel to use.  Options are
                    - 'gaussian'
                    - 'tophat'
                    - 'epanechnikov'
                    - 'exponential'
                    - 'linear'
                    - 'cosine'
                    Default is kernel = 'gaussian'
                atol, rtol : float (default = 0)
                    Specify the desired relative and absolute tolerance of the result.
                    If the true result is K_true, then the returned result K_ret
                    satisfies ``abs(K_true - K_ret) < atol + rtol * K_ret``
                    The default is zero (i.e. machine precision) for both.
                breadth_first : boolean (default = False)
                    if True, use a breadth-first search.  If False (default) use a
                    depth-first search.  Breadth-first is generally faster for
                    compact kernels and/or high tolerances.
                return_log : boolean (default = False)
                    return the logarithm of the result.  This can be more accurate
                    than returning the result itself for narrow kernels.
        
                Returns
                -------
                density : ndarray
                    The array of (log)-density evaluations, shape = X.shape[:-1]
        """
        pass

    def query(self, X, k=1, return_distance=True, dualtree=False, breadth_first=False): # real signature unknown; restored from __doc__
        """
        query(X, k=1, return_distance=True,
                      dualtree=False, breadth_first=False)
        
                query the tree for the k nearest neighbors
        
                Parameters
                ----------
                X : array-like, shape = [n_samples, n_features]
                    An array of points to query
                k : integer  (default = 1)
                    The number of nearest neighbors to return
                return_distance : boolean (default = True)
                    if True, return a tuple (d, i) of distances and indices
                    if False, return array i
                dualtree : boolean (default = False)
                    if True, use the dual tree formalism for the query: a tree is
                    built for the query points, and the pair of trees is used to
                    efficiently search this space.  This can lead to better
                    performance as the number of points grows large.
                breadth_first : boolean (default = False)
                    if True, then query the nodes in a breadth-first manner.
                    Otherwise, query the nodes in a depth-first manner.
                sort_results : boolean (default = True)
                    if True, then distances and indices of each point are sorted
                    on return, so that the first column contains the closest points.
                    Otherwise, neighbors are returned in an arbitrary order.
        
                Returns
                -------
                i    : if return_distance == False
                (d,i) : if return_distance == True
        
                d : array of doubles - shape: x.shape[:-1] + (k,)
                    each entry gives the list of distances to the
                    neighbors of the corresponding point
        
                i : array of integers - shape: x.shape[:-1] + (k,)
                    each entry gives the list of indices of
                    neighbors of the corresponding point
        """
        pass

    def query_radius(self, X, r, count_only=False): # real signature unknown; restored from __doc__
        """
        query_radius(self, X, r, count_only = False):
        
                query the tree for neighbors within a radius r
        
                Parameters
                ----------
                X : array-like, shape = [n_samples, n_features]
                    An array of points to query
                r : distance within which neighbors are returned
                    r can be a single value, or an array of values of shape
                    x.shape[:-1] if different radii are desired for each point.
                return_distance : boolean (default = False)
                    if True,  return distances to neighbors of each point
                    if False, return only neighbors
                    Note that unlike the query() method, setting return_distance=True
                    here adds to the computation time.  Not all distances need to be
                    calculated explicitly for return_distance=False.  Results are
                    not sorted by default: see ``sort_results`` keyword.
                count_only : boolean (default = False)
                    if True,  return only the count of points within distance r
                    if False, return the indices of all points within distance r
                    If return_distance==True, setting count_only=True will
                    result in an error.
                sort_results : boolean (default = False)
                    if True, the distances and indices will be sorted before being
                    returned.  If False, the results will not be sorted.  If
                    return_distance == False, setting sort_results = True will
                    result in an error.
        
                Returns
                -------
                count       : if count_only == True
                ind         : if count_only == False and return_distance == False
                (ind, dist) : if count_only == False and return_distance == True
        
                count : array of integers, shape = X.shape[:-1]
                    each entry gives the number of neighbors within
                    a distance r of the corresponding point.
        
                ind : array of objects, shape = X.shape[:-1]
                    each element is a numpy integer array listing the indices of
                    neighbors of the corresponding point.  Note that unlike
                    the results of a k-neighbors query, the returned neighbors
                    are not sorted by distance by default.
        
                dist : array of objects, shape = X.shape[:-1]
                    each element is a numpy double array
                    listing the distances corresponding to indices in i.
        """
        pass

    def reset_n_calls(self, *args, **kwargs): # real signature unknown
        pass

    def two_point_correlation(self, *args, **kwargs): # real signature unknown
        """
        Compute the two-point correlation function
        
                Parameters
                ----------
                X : array-like, shape = [n_samples, n_features]
                    An array of points to query.  Last dimension should match dimension
                    of training data.
                r : array_like
                    A one-dimensional array of distances
                dualtree : boolean (default = False)
                    If true, use a dualtree algorithm.  Otherwise, use a single-tree
                    algorithm.  Dual tree algorithms can have better scaling for
                    large N.
        
                Returns
                -------
                counts : ndarray
                    counts[i] contains the number of pairs of points with distance
                    less than or equal to r[i]
        """
        pass

    def _update_memviews(self, *args, **kwargs): # real signature unknown
        pass

    def _update_sample_weight(self, *args, **kwargs): # real signature unknown
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

    data = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    idx_array = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    node_bounds = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    node_data = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    sample_weight = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    sum_weight = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    valid_metrics = [
        'euclidean',
        'l2',
        'minkowski',
        'p',
        'manhattan',
        'cityblock',
        'l1',
        'chebyshev',
        'infinity',
    ]
    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0F5D3638>'


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


class KDTree(BinaryTree):
    """
    KDTree for fast generalized N-point problems
    
    KDTree(X, leaf_size=40, metric='minkowski', \**kwargs)
    
    Parameters
    ----------
    X : array-like, shape = [n_samples, n_features]
        n_samples is the number of points in the data set, and
        n_features is the dimension of the parameter space.
        Note: if X is a C-contiguous array of doubles then data will
        not be copied. Otherwise, an internal copy will be made.
    
    leaf_size : positive integer (default = 40)
        Number of points at which to switch to brute-force. Changing
        leaf_size will not affect the results of a query, but can
        significantly impact the speed of a query and the memory required
        to store the constructed tree.  The amount of memory needed to
        store the tree scales as approximately n_samples / leaf_size.
        For a specified ``leaf_size``, a leaf node is guaranteed to
        satisfy ``leaf_size <= n_points <= 2 * leaf_size``, except in
        the case that ``n_samples < leaf_size``.
    
    metric : string or DistanceMetric object
        the distance metric to use for the tree.  Default='minkowski'
        with p=2 (that is, a euclidean metric). See the documentation
        of the DistanceMetric class for a list of available metrics.
        kd_tree.valid_metrics gives a list of the metrics which
        are valid for KDTree.
    
    Additional keywords are passed to the distance metric class.
    
    Attributes
    ----------
    data : memory view
        The training data
    
    Examples
    --------
    Query for k-nearest neighbors
    
        >>> import numpy as np
        >>> rng = np.random.RandomState(0)
        >>> X = rng.random_sample((10, 3))  # 10 points in 3 dimensions
        >>> tree = KDTree(X, leaf_size=2)              # doctest: +SKIP
        >>> dist, ind = tree.query(X[:1], k=3)                # doctest: +SKIP
        >>> print(ind)  # indices of 3 closest neighbors
        [0 3 1]
        >>> print(dist)  # distances to 3 closest neighbors
        [ 0.          0.19662693  0.29473397]
    
    Pickle and Unpickle a tree.  Note that the state of the tree is saved in the
    pickle operation: the tree needs not be rebuilt upon unpickling.
    
        >>> import numpy as np
        >>> import pickle
        >>> rng = np.random.RandomState(0)
        >>> X = rng.random_sample((10, 3))  # 10 points in 3 dimensions
        >>> tree = KDTree(X, leaf_size=2)        # doctest: +SKIP
        >>> s = pickle.dumps(tree)                     # doctest: +SKIP
        >>> tree_copy = pickle.loads(s)                # doctest: +SKIP
        >>> dist, ind = tree_copy.query(X[:1], k=3)     # doctest: +SKIP
        >>> print(ind)  # indices of 3 closest neighbors
        [0 3 1]
        >>> print(dist)  # distances to 3 closest neighbors
        [ 0.          0.19662693  0.29473397]
    
    Query for neighbors within a given radius
    
        >>> import numpy as np
        >>> rng = np.random.RandomState(0)
        >>> X = rng.random_sample((10, 3))  # 10 points in 3 dimensions
        >>> tree = KDTree(X, leaf_size=2)     # doctest: +SKIP
        >>> print(tree.query_radius(X[:1], r=0.3, count_only=True))
        3
        >>> ind = tree.query_radius(X[:1], r=0.3)  # doctest: +SKIP
        >>> print(ind)  # indices of neighbors within distance 0.3
        [3 0 1]
    
    
    Compute a gaussian kernel density estimate:
    
        >>> import numpy as np
        >>> rng = np.random.RandomState(42)
        >>> X = rng.random_sample((100, 3))
        >>> tree = KDTree(X)                # doctest: +SKIP
        >>> tree.kernel_density(X[:3], h=0.1, kernel='gaussian')
        array([ 6.94114649,  7.83281226,  7.2071716 ])
    
    Compute a two-point auto-correlation function
    
        >>> import numpy as np
        >>> rng = np.random.RandomState(0)
        >>> X = rng.random_sample((30, 3))
        >>> r = np.linspace(0, 1, 5)
        >>> tree = KDTree(X)                # doctest: +SKIP
        >>> tree.two_point_correlation(X, r)
        array([ 30,  62, 278, 580, 820])
    """
    def __init__(self, X, leaf_size=40, metric='minkowski', *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0F5D36F8>'


class NeighborsHeap(object):
    """
    A max-heap structure to keep track of distances/indices of neighbors
    
        This implements an efficient pre-allocated set of fixed-size heaps
        for chasing neighbors, holding both an index and a distance.
        When any row of the heap is full, adding an additional point will push
        the furthest point off the heap.
    
        Parameters
        ----------
        n_pts : int
            the number of heaps to use
        n_nbrs : int
            the size of each heap.
    """
    def get_arrays(self, *args, **kwargs): # real signature unknown
        """
        Get the arrays of distances and indices within the heap.
        
                If sort=True, then simultaneously sort the indices and distances,
                so the closer points are listed first.
        """
        pass

    def push(self, *args, **kwargs): # real signature unknown
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0F5D36C8>'


class NodeHeap(object):
    """
    NodeHeap
    
        This is a min-heap implementation for keeping track of nodes
        during a breadth-first search.  Unlike the NeighborsHeap above,
        the NodeHeap does not have a fixed size and must be able to grow
        as elements are added.
    
        Internally, the data is stored in a simple binary heap which meets
        the min heap condition:
    
            heap[i].val < min(heap[2 * i + 1].val, heap[2 * i + 2].val)
    """
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0F5D3668>'


# variables with complex values

DOC_DICT = {
    'BinaryTree': 'KDTree',
    'binary_tree': 'kd_tree',
}

NodeData = None # (!) real value is "dtype({'names':['idx_start','idx_end','is_leaf','radius'], 'formats':['<i4','<i4','<i4','<f8'], 'offsets':[0,4,8,16], 'itemsize':24})"

NodeHeapData = None # (!) real value is "dtype([('val', '<f8'), ('i1', '<i4'), ('i2', '<i4')])"

offsets = [
    0,
    4,
    8,
    16,
]

VALID_METRICS = [
    'EuclideanDistance',
    'ManhattanDistance',
    'ChebyshevDistance',
    'MinkowskiDistance',
]

VALID_METRIC_IDS = [
    'euclidean',
    'l2',
    'minkowski',
    'p',
    'manhattan',
    'cityblock',
    'l1',
    'chebyshev',
    'infinity',
]

__all__ = [
    'KDTree',
]

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0F5E74F0>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.neighbors.kd_tree', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0F5E74F0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\neighbors\\\\kd_tree.cp37-win32.pyd')"

__test__ = {}

