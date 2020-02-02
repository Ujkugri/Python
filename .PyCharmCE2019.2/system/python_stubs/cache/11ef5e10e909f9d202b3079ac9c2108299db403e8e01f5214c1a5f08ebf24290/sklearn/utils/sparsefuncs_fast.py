# encoding: utf-8
# module sklearn.utils.sparsefuncs_fast
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\utils\sparsefuncs_fast.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import scipy.sparse as sp # C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\sparse\__init__.py

# functions

def assign_rows_csr(*args, **kwargs): # real signature unknown
    """
    Densify selected rows of a CSR matrix into a preallocated array.
    
        Like out[out_rows] = X[X_rows].toarray() but without copying.
        No-copy supported for both dtype=np.float32 and dtype=np.float64.
    
        Parameters
        ----------
        X : scipy.sparse.csr_matrix, shape=(n_samples, n_features)
        X_rows : array, dtype=np.intp, shape=n_rows
        out_rows : array, dtype=np.intp, shape=n_rows
        out : array, shape=(arbitrary, n_features)
    """
    pass

def csc_mean_variance_axis0(*args, **kwargs): # real signature unknown
    """
    Compute mean and variance along axis 0 on a CSC matrix
    
        Parameters
        ----------
        X : CSC sparse matrix, shape (n_samples, n_features)
            Input data.
    
        Returns
        -------
        means : float array with shape (n_features,)
            Feature-wise means
    
        variances : float array with shape (n_features,)
            Feature-wise variances
    """
    pass

def csr_mean_variance_axis0(*args, **kwargs): # real signature unknown
    """
    Compute mean and variance along axis 0 on a CSR matrix
    
        Parameters
        ----------
        X : CSR sparse matrix, shape (n_samples, n_features)
            Input data.
    
        Returns
        -------
        means : float array with shape (n_features,)
            Feature-wise means
    
        variances : float array with shape (n_features,)
            Feature-wise variances
    """
    pass

def csr_row_norms(*args, **kwargs): # real signature unknown
    """ L2 norm of each row in CSR matrix X. """
    pass

def incr_mean_variance_axis0(*args, **kwargs): # real signature unknown
    """
    Compute mean and variance along axis 0 on a CSR or CSC matrix.
    
        last_mean, last_var are the statistics computed at the last step by this
        function. Both must be initialized to 0.0. last_n is the
        number of samples encountered until now and is initialized at 0.
    
        Parameters
        ----------
        X : CSR or CSC sparse matrix, shape (n_samples, n_features)
          Input data.
    
        last_mean : float array with shape (n_features,)
          Array of feature-wise means to update with the new data X.
    
        last_var : float array with shape (n_features,)
          Array of feature-wise var to update with the new data X.
    
        last_n : int array with shape (n_features,)
          Number of samples seen so far, before X.
    
        Returns
        -------
        updated_mean : float array with shape (n_features,)
          Feature-wise means
    
        updated_variance : float array with shape (n_features,)
          Feature-wise variances
    
        updated_n : int array with shape (n_features,)
          Updated number of samples seen
    
        Notes
        -----
        NaNs are ignored during the computation.
    
        References
        ----------
        T. Chan, G. Golub, R. LeVeque. Algorithms for computing the sample
          variance: recommendations, The American Statistician, Vol. 37, No. 3,
          pp. 242-247
    
        Also, see the non-sparse implementation of this in
        `utils.extmath._batch_mean_variance_update`.
    """
    pass

def inplace_csr_row_normalize_l1(*args, **kwargs): # real signature unknown
    """ Inplace row normalize using the l1 norm """
    pass

def inplace_csr_row_normalize_l2(*args, **kwargs): # real signature unknown
    """ Inplace row normalize using the l2 norm """
    pass

def _csc_mean_variance_axis0(*args, **kwargs): # real signature unknown
    pass

def _csr_mean_variance_axis0(*args, **kwargs): # real signature unknown
    pass

def _csr_row_norms(*args, **kwargs): # real signature unknown
    pass

def _incr_mean_variance_axis0(*args, **kwargs): # real signature unknown
    pass

def _inplace_csr_row_normalize_l1(*args, **kwargs): # real signature unknown
    pass

def _inplace_csr_row_normalize_l2(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x00DCE0B0>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.utils.sparsefuncs_fast', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x00DCE0B0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\utils\\\\sparsefuncs_fast.cp37-win32.pyd')"

__test__ = {}

