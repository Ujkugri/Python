# encoding: utf-8
# module sklearn.neighbors.quad_tree
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\neighbors\quad_tree.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

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

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# classes

class _QuadTree(object):
    """
    Array-based representation of a QuadTree.
    
        This class is currently working for indexing 2D data (regular QuadTree) and
        for indexing 3D data (OcTree). It is planned to split the 2 implementations
        using `Cython.Tempita` to save some memory for QuadTree.
    
        Note that this code is currently internally used only by the Barnes-Hut
        method in `sklearn.manifold.TSNE`. It is planned to be refactored and
        generalized in the future to be compatible with nearest neighbors API of
        `sklearn.neighbors` with 2D and 3D data.
    """
    def build_tree(self, *args, **kwargs): # real signature unknown
        """ Build a tree from an arary of points X. """
        pass

    def get_cell(self, *args, **kwargs): # real signature unknown
        """
        return the id of the cell containing the query point or raise 
                ValueError if the point is not in the tree
        """
        pass

    def test_summarize(self, *args, **kwargs): # real signature unknown
        pass

    def _check_coherence(self, *args, **kwargs): # real signature unknown
        """
        Check the coherence of the cells of the tree.
        
                Check that the info stored in each cell is compatible with the info
                stored in descendent and sibling cells. Raise a ValueError if this
                fails.
        """
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

    cell_count = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    cumulative_size = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    leafs = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    max_depth = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_dimensions = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_points = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    verbose = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x02F6B710>'


# variables with complex values

CELL_DTYPE = None # (!) real value is "dtype([('parent', '<i4'), ('children', '<i4', (8,)), ('cell_id', '<i4'), ('point_index', '<i4'), ('is_leaf', '<i4'), ('max_width', '<f4'), ('depth', '<i4'), ('cumulative_size', '<i4'), ('center', '<f4', (3,)), ('barycenter', '<f4', (3,)), ('min_bounds', '<f4', (3,)), ('max_bounds', '<f4', (3,))])"

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0F522F50>'

__pyx_capi__ = {
    'EPSILON': None, # (!) real value is '<capsule object "float" at 0x00B76C50>'
}

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.neighbors.quad_tree', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0F522F50>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\neighbors\\\\quad_tree.cp37-win32.pyd')"

__test__ = {}

