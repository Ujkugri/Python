# encoding: utf-8
# module sklearn.utils.graph_shortest_path
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\utils\graph_shortest_path.cp37-win32.pyd
# by generator 1.147
"""
Routines for performing shortest-path graph searches

The main interface is in the function `graph_shortest_path`.  This
calls cython routines that compute the shortest path using either
the Floyd-Warshall algorithm, or Dykstra's algorithm with Fibonacci Heaps.
"""

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import numpy as __numpy
import scipy.sparse.compressed as __scipy_sparse_compressed


# functions

def graph_shortest_path(*args, **kwargs): # real signature unknown
    """
    Perform a shortest-path graph search on a positive directed or
        undirected graph.
    
        Parameters
        ----------
        dist_matrix : arraylike or sparse matrix, shape = (N,N)
            Array of positive distances.
            If vertex i is connected to vertex j, then dist_matrix[i,j] gives
            the distance between the vertices.
            If vertex i is not connected to vertex j, then dist_matrix[i,j] = 0
        directed : boolean
            if True, then find the shortest path on a directed graph: only
            progress from a point to its neighbors, not the other way around.
            if False, then find the shortest path on an undirected graph: the
            algorithm can progress from a point to its neighbors and vice versa.
        method : string ['auto'|'FW'|'D']
            method to use.  Options are
            'auto' : attempt to choose the best method for the current problem
            'FW' : Floyd-Warshall algorithm.  O[N^3]
            'D' : Dijkstra's algorithm with Fibonacci stacks.  O[(k+log(N))N^2]
    
        Returns
        -------
        G : np.ndarray, float, shape = [N,N]
            G[i,j] gives the shortest distance from point i to point j
            along the graph.
    
        Notes
        -----
        As currently implemented, Dijkstra's algorithm does not work for
        graphs with direction-dependent distances when directed == False.
        i.e., if dist_matrix[i,j] and dist_matrix[j,i] are not equal and
        both are nonzero, method='D' will not necessarily yield the correct
        result.
    
        Also, these routines have not been tested for graphs with negative
        distances.  Negative distances can lead to infinite cycles that must
        be handled by specialized algorithms.
    """
    pass

def isspmatrix(x): # reliably restored by inspect
    """
    Is x of a sparse matrix type?
    
        Parameters
        ----------
        x
            object to check for being a sparse matrix
    
        Returns
        -------
        bool
            True if x is a sparse matrix, False otherwise
    
        Notes
        -----
        issparse and isspmatrix are aliases for the same function.
    
        Examples
        --------
        >>> from scipy.sparse import csr_matrix, isspmatrix
        >>> isspmatrix(csr_matrix([[5]]))
        True
    
        >>> from scipy.sparse import isspmatrix
        >>> isspmatrix(5)
        False
    """
    pass

def isspmatrix_csr(x): # reliably restored by inspect
    """
    Is x of csr_matrix type?
    
        Parameters
        ----------
        x
            object to check for being a csr matrix
    
        Returns
        -------
        bool
            True if x is a csr matrix, False otherwise
    
        Examples
        --------
        >>> from scipy.sparse import csr_matrix, isspmatrix_csr
        >>> isspmatrix_csr(csr_matrix([[5]]))
        True
    
        >>> from scipy.sparse import csc_matrix, csr_matrix, isspmatrix_csc
        >>> isspmatrix_csr(csc_matrix([[5]]))
        False
    """
    pass

# classes

class csr_matrix(__scipy_sparse_compressed._cs_matrix):
    """
    Compressed Sparse Row matrix
    
        This can be instantiated in several ways:
            csr_matrix(D)
                with a dense matrix or rank-2 ndarray D
    
            csr_matrix(S)
                with another sparse matrix S (equivalent to S.tocsr())
    
            csr_matrix((M, N), [dtype])
                to construct an empty matrix with shape (M, N)
                dtype is optional, defaulting to dtype='d'.
    
            csr_matrix((data, (row_ind, col_ind)), [shape=(M, N)])
                where ``data``, ``row_ind`` and ``col_ind`` satisfy the
                relationship ``a[row_ind[k], col_ind[k]] = data[k]``.
    
            csr_matrix((data, indices, indptr), [shape=(M, N)])
                is the standard CSR representation where the column indices for
                row i are stored in ``indices[indptr[i]:indptr[i+1]]`` and their
                corresponding values are stored in ``data[indptr[i]:indptr[i+1]]``.
                If the shape parameter is not supplied, the matrix dimensions
                are inferred from the index arrays.
    
        Attributes
        ----------
        dtype : dtype
            Data type of the matrix
        shape : 2-tuple
            Shape of the matrix
        ndim : int
            Number of dimensions (this is always 2)
        nnz
            Number of nonzero elements
        data
            CSR format data array of the matrix
        indices
            CSR format index array of the matrix
        indptr
            CSR format index pointer array of the matrix
        has_sorted_indices
            Whether indices are sorted
    
        Notes
        -----
    
        Sparse matrices can be used in arithmetic operations: they support
        addition, subtraction, multiplication, division, and matrix power.
    
        Advantages of the CSR format
          - efficient arithmetic operations CSR + CSR, CSR * CSR, etc.
          - efficient row slicing
          - fast matrix vector products
    
        Disadvantages of the CSR format
          - slow column slicing operations (consider CSC)
          - changes to the sparsity structure are expensive (consider LIL or DOK)
    
        Examples
        --------
    
        >>> import numpy as np
        >>> from scipy.sparse import csr_matrix
        >>> csr_matrix((3, 4), dtype=np.int8).toarray()
        array([[0, 0, 0, 0],
               [0, 0, 0, 0],
               [0, 0, 0, 0]], dtype=int8)
    
        >>> row = np.array([0, 0, 1, 2, 2, 2])
        >>> col = np.array([0, 2, 2, 0, 1, 2])
        >>> data = np.array([1, 2, 3, 4, 5, 6])
        >>> csr_matrix((data, (row, col)), shape=(3, 3)).toarray()
        array([[1, 0, 2],
               [0, 0, 3],
               [4, 5, 6]])
    
        >>> indptr = np.array([0, 2, 3, 6])
        >>> indices = np.array([0, 2, 2, 0, 1, 2])
        >>> data = np.array([1, 2, 3, 4, 5, 6])
        >>> csr_matrix((data, indices, indptr), shape=(3, 3)).toarray()
        array([[1, 0, 2],
               [0, 0, 3],
               [4, 5, 6]])
    
        As an example of how to construct a CSR matrix incrementally,
        the following snippet builds a term-document matrix from texts:
    
        >>> docs = [["hello", "world", "hello"], ["goodbye", "cruel", "world"]]
        >>> indptr = [0]
        >>> indices = []
        >>> data = []
        >>> vocabulary = {}
        >>> for d in docs:
        ...     for term in d:
        ...         index = vocabulary.setdefault(term, len(vocabulary))
        ...         indices.append(index)
        ...         data.append(1)
        ...     indptr.append(len(indices))
        ...
        >>> csr_matrix((data, indices, indptr), dtype=int).toarray()
        array([[2, 1, 0, 0],
               [0, 1, 1, 1]])
    """
    def getcol(self, i): # reliably restored by inspect
        """
        Returns a copy of column i of the matrix, as a (m x 1)
                CSR matrix (column vector).
        """
        pass

    def getrow(self, i): # reliably restored by inspect
        """
        Returns a copy of row i of the matrix, as a (1 x n)
                CSR matrix (row vector).
        """
        pass

    def tobsr(self, blocksize=None, copy=True): # reliably restored by inspect
        """
        Convert this matrix to Block Sparse Row format.
        
                With copy=False, the data/indices may be shared between this matrix and
                the resultant bsr_matrix.
        
                When blocksize=(R, C) is provided, it will be used for construction of
                the bsr_matrix.
        """
        pass

    def tocsc(self, copy=False): # reliably restored by inspect
        """
        Convert this matrix to Compressed Sparse Column format.
        
                With copy=False, the data/indices may be shared between this matrix and
                the resultant csc_matrix.
        """
        pass

    def tocsr(self, copy=False): # reliably restored by inspect
        """
        Convert this matrix to Compressed Sparse Row format.
        
                With copy=False, the data/indices may be shared between this matrix and
                the resultant csr_matrix.
        """
        pass

    def tolil(self, copy=False): # reliably restored by inspect
        """
        Convert this matrix to LInked List format.
        
                With copy=False, the data/indices may be shared between this matrix and
                the resultant lil_matrix.
        """
        pass

    def transpose(self, axes=None, copy=False): # reliably restored by inspect
        """
        Reverses the dimensions of the sparse matrix.
        
                Parameters
                ----------
                axes : None, optional
                    This argument is in the signature *solely* for NumPy
                    compatibility reasons. Do not pass in anything except
                    for the default value.
                copy : bool, optional
                    Indicates whether or not attributes of `self` should be
                    copied whenever possible. The degree to which attributes
                    are copied varies depending on the type of sparse matrix
                    being used.
        
                Returns
                -------
                p : `self` with the dimensions reversed.
        
                See Also
                --------
                numpy.matrix.transpose : NumPy's implementation of 'transpose'
                                         for matrices
        """
        pass

    def _get_arrayXint(self, row, col): # reliably restored by inspect
        # no doc
        pass

    def _get_arrayXslice(self, row, col): # reliably restored by inspect
        # no doc
        pass

    def _get_intXarray(self, row, col): # reliably restored by inspect
        # no doc
        pass

    def _get_intXslice(self, row, col): # reliably restored by inspect
        # no doc
        pass

    def _get_sliceXarray(self, row, col): # reliably restored by inspect
        # no doc
        pass

    def _get_sliceXint(self, row, col): # reliably restored by inspect
        # no doc
        pass

    def _swap(self, x): # reliably restored by inspect
        """ swap the members of x if this is a column-oriented matrix """
        pass

    def __init__(self, arg1, shape=None, dtype=None, copy=False): # reliably restored by inspect
        # no doc
        pass

    def __iter__(self): # reliably restored by inspect
        # no doc
        pass

    format = 'csr'


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
    Signed integer type, compatible with Python `int` anc C ``long``.
        Character code: ``'l'``.
        Canonical name: ``np.int_``.
        Alias *on this platform*: ``np.int32``: 32-bit signed integer (-2147483648 to 2147483647).
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


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x03D4E0D0>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.utils.graph_shortest_path', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x03D4E0D0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\utils\\\\graph_shortest_path.cp37-win32.pyd')"

__test__ = {}

