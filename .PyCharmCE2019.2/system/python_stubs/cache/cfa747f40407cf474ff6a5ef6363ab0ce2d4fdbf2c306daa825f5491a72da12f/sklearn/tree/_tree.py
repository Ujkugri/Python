# encoding: utf-8
# module sklearn.tree._tree
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\tree\_tree.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import numpy as __numpy
import scipy.sparse.compressed as __scipy_sparse_compressed


# Variables with simple values

TREE_LEAF = -1
TREE_UNDEFINED = -2

# functions

def issparse(x): # reliably restored by inspect
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

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_TreeBuilder(*args, **kwargs): # real signature unknown
    pass

# classes

class TreeBuilder(object):
    """ Interface for different tree building strategies. """
    def build(self, *args, **kwargs): # real signature unknown
        """ Build a decision tree from the training set (X, y). """
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0F5235F0>'


class BestFirstTreeBuilder(TreeBuilder):
    """
    Build a decision tree in best-first fashion.
    
        The best node to expand is given by the node at the frontier that has the
        highest impurity improvement.
    """
    def build(self, *args, **kwargs): # real signature unknown
        """ Build a decision tree from the training set (X, y). """
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0F523650>'


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


class DepthFirstTreeBuilder(TreeBuilder):
    """ Build a decision tree in depth-first fashion. """
    def build(self, *args, **kwargs): # real signature unknown
        """ Build a decision tree from the training set (X, y). """
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0F523578>'


class DOUBLE(__numpy.floating, float):
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


class DTYPE(__numpy.floating):
    """
    Single-precision floating-point number type, compatible with C ``float``.
        Character code: ``'f'``.
        Canonical name: ``np.single``.
        Alias *on this platform*: ``np.float32``: 32-bit-precision floating-point number type: sign bit, 8 bits exponent, 23 bits mantissa.
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


class Tree(object):
    """
    Array-based representation of a binary decision tree.
    
        The binary tree is represented as a number of parallel arrays. The i-th
        element of each array holds information about the node `i`. Node 0 is the
        tree's root. You can find a detailed description of all arrays in
        `_tree.pxd`. NOTE: Some of the arrays only apply to either leaves or split
        nodes, resp. In this case the values of nodes of the other type are
        arbitrary!
    
        Attributes
        ----------
        node_count : int
            The number of nodes (internal nodes + leaves) in the tree.
    
        capacity : int
            The current capacity (i.e., size) of the arrays, which is at least as
            great as `node_count`.
    
        max_depth : int
            The depth of the tree, i.e. the maximum depth of its leaves.
    
        children_left : array of int, shape [node_count]
            children_left[i] holds the node id of the left child of node i.
            For leaves, children_left[i] == TREE_LEAF. Otherwise,
            children_left[i] > i. This child handles the case where
            X[:, feature[i]] <= threshold[i].
    
        children_right : array of int, shape [node_count]
            children_right[i] holds the node id of the right child of node i.
            For leaves, children_right[i] == TREE_LEAF. Otherwise,
            children_right[i] > i. This child handles the case where
            X[:, feature[i]] > threshold[i].
    
        feature : array of int, shape [node_count]
            feature[i] holds the feature to split on, for the internal node i.
    
        threshold : array of double, shape [node_count]
            threshold[i] holds the threshold for the internal node i.
    
        value : array of double, shape [node_count, n_outputs, max_n_classes]
            Contains the constant prediction value of each node.
    
        impurity : array of double, shape [node_count]
            impurity[i] holds the impurity (i.e., the value of the splitting
            criterion) at node i.
    
        n_node_samples : array of int, shape [node_count]
            n_node_samples[i] holds the number of training samples reaching node i.
    
        weighted_n_node_samples : array of int, shape [node_count]
            weighted_n_node_samples[i] holds the weighted number of training samples
            reaching node i.
    """
    def apply(self, *args, **kwargs): # real signature unknown
        """ Finds the terminal region (=leaf node) for each sample in X. """
        pass

    def compute_feature_importances(self, *args, **kwargs): # real signature unknown
        """ Computes the importance of each feature (aka variable). """
        pass

    def compute_partial_dependence(self, *args, **kwargs): # real signature unknown
        """
        Partial dependence of the response on the ``target_feature`` set.
        
                For each sample in ``X`` a tree traversal is performed.
                Each traversal starts from the root with weight 1.0.
        
                At each non-leaf node that splits on a target feature, either
                the left child or the right child is visited based on the feature
                value of the current sample, and the weight is not modified.
                At each non-leaf node that splits on a complementary feature,
                both children are visited and the weight is multiplied by the fraction
                of training samples which went to each child.
        
                At each leaf, the value of the node is multiplied by the current
                weight (weights sum to 1 for all visited terminal nodes).
        
                Parameters
                ----------
                X : view on 2d ndarray, shape (n_samples, n_target_features)
                    The grid points on which the partial dependence should be
                    evaluated.
                target_feature : view on 1d ndarray, shape (n_target_features)
                    The set of target features for which the partial dependence
                    should be evaluated.
                out : view on 1d ndarray, shape (n_samples)
                    The value of the partial dependence function on each grid
                    point.
        """
        pass

    def decision_path(self, *args, **kwargs): # real signature unknown
        """ Finds the decision path (=node) for each sample in X. """
        pass

    def predict(self, *args, **kwargs): # real signature unknown
        """ Predict target for X. """
        pass

    def __getstate__(self, *args, **kwargs): # real signature unknown
        """ Getstate re-implementation, for pickling. """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        """ Reduce re-implementation, for pickling. """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        """ Setstate re-implementation, for unpickling. """
        pass

    capacity = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    children_left = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    children_right = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    feature = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    impurity = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    max_depth = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    max_n_classes = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    node_count = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_classes = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_features = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_leaves = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_node_samples = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_outputs = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    threshold = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    value = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    weighted_n_node_samples = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x0F5235A8>'


# variables with complex values

NODE_DTYPE = None # (!) real value is "dtype({'names':['left_child','right_child','feature','threshold','impurity','n_node_samples','weighted_n_node_samples'], 'formats':['<i4','<i4','<i4','<f8','<f8','<i4','<f8'], 'offsets':[0,4,8,16,24,32,40], 'itemsize':48})"

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0F53C570>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.tree._tree', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0F53C570>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\tree\\\\_tree.cp37-win32.pyd')"

__test__ = {}

