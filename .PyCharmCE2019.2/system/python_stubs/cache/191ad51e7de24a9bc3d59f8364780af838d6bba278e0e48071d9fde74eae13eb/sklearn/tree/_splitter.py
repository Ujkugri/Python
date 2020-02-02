# encoding: utf-8
# module sklearn.tree._splitter
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\tree\_splitter.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import scipy.sparse.compressed as __scipy_sparse_compressed


# functions

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# classes

class Splitter(object):
    """
    Abstract splitter class.
    
        Splitters are called by tree builders to find the best splits on both
        sparse and dense data, one split at a time.
    """
    def __getstate__(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce_cython__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate_cython__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    criterion = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    max_features = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    min_samples_leaf = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    min_weight_leaf = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x101E3338>'


class BaseDenseSplitter(Splitter):
    # no doc
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce_cython__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate_cython__(self, *args, **kwargs): # real signature unknown
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x101E3380>'


class BaseSparseSplitter(Splitter):
    # no doc
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce_cython__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate_cython__(self, *args, **kwargs): # real signature unknown
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x101E3410>'


class BestSparseSplitter(BaseSparseSplitter):
    """ Splitter for finding the best split, using the sparse data. """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x101E3440>'


class BestSplitter(BaseDenseSplitter):
    """ Splitter for finding the best split. """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x101E3308>'


class csc_matrix(__scipy_sparse_compressed._cs_matrix):
    """
    Compressed Sparse Column matrix
    
        This can be instantiated in several ways:
    
            csc_matrix(D)
                with a dense matrix or rank-2 ndarray D
    
            csc_matrix(S)
                with another sparse matrix S (equivalent to S.tocsc())
    
            csc_matrix((M, N), [dtype])
                to construct an empty matrix with shape (M, N)
                dtype is optional, defaulting to dtype='d'.
    
            csc_matrix((data, (row_ind, col_ind)), [shape=(M, N)])
                where ``data``, ``row_ind`` and ``col_ind`` satisfy the
                relationship ``a[row_ind[k], col_ind[k]] = data[k]``.
    
            csc_matrix((data, indices, indptr), [shape=(M, N)])
                is the standard CSC representation where the row indices for
                column i are stored in ``indices[indptr[i]:indptr[i+1]]``
                and their corresponding values are stored in
                ``data[indptr[i]:indptr[i+1]]``.  If the shape parameter is
                not supplied, the matrix dimensions are inferred from
                the index arrays.
    
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
            Data array of the matrix
        indices
            CSC format index array
        indptr
            CSC format index pointer array
        has_sorted_indices
            Whether indices are sorted
    
        Notes
        -----
    
        Sparse matrices can be used in arithmetic operations: they support
        addition, subtraction, multiplication, division, and matrix power.
    
        Advantages of the CSC format
            - efficient arithmetic operations CSC + CSC, CSC * CSC, etc.
            - efficient column slicing
            - fast matrix vector products (CSR, BSR may be faster)
    
        Disadvantages of the CSC format
          - slow row slicing operations (consider CSR)
          - changes to the sparsity structure are expensive (consider LIL or DOK)
    
    
        Examples
        --------
    
        >>> import numpy as np
        >>> from scipy.sparse import csc_matrix
        >>> csc_matrix((3, 4), dtype=np.int8).toarray()
        array([[0, 0, 0, 0],
               [0, 0, 0, 0],
               [0, 0, 0, 0]], dtype=int8)
    
        >>> row = np.array([0, 2, 2, 0, 1, 2])
        >>> col = np.array([0, 0, 1, 2, 2, 2])
        >>> data = np.array([1, 2, 3, 4, 5, 6])
        >>> csc_matrix((data, (row, col)), shape=(3, 3)).toarray()
        array([[1, 0, 4],
               [0, 0, 5],
               [2, 3, 6]])
    
        >>> indptr = np.array([0, 2, 3, 6])
        >>> indices = np.array([0, 2, 2, 0, 1, 2])
        >>> data = np.array([1, 2, 3, 4, 5, 6])
        >>> csc_matrix((data, indices, indptr), shape=(3, 3)).toarray()
        array([[1, 0, 4],
               [0, 0, 5],
               [2, 3, 6]])
    """
    def getcol(self, i): # reliably restored by inspect
        """
        Returns a copy of column i of the matrix, as a (m x 1)
                CSC matrix (column vector).
        """
        pass

    def getrow(self, i): # reliably restored by inspect
        """
        Returns a copy of row i of the matrix, as a (1 x n)
                CSR matrix (row vector).
        """
        pass

    def nonzero(self): # reliably restored by inspect
        """
        nonzero indices
        
                Returns a tuple of arrays (row,col) containing the indices
                of the non-zero elements of the matrix.
        
                Examples
                --------
                >>> from scipy.sparse import csr_matrix
                >>> A = csr_matrix([[1,2,0],[0,0,3],[4,0,5]])
                >>> A.nonzero()
                (array([0, 0, 1, 2, 2]), array([0, 1, 2, 0, 2]))
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

    format = 'csc'


class RandomSparseSplitter(BaseSparseSplitter):
    """ Splitter for finding a random split, using the sparse data. """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x101E3470>'


class RandomSplitter(BaseDenseSplitter):
    """ Splitter for finding the best random split. """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x101E33E0>'


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x101F1E10>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.tree._splitter', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x101F1E10>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\tree\\\\_splitter.cp37-win32.pyd')"

__test__ = {}

