# encoding: utf-8
# module sklearn.cluster._k_means
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\cluster\_k_means.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import scipy.sparse as sp # C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\sparse\__init__.py
from sklearn.utils.sparsefuncs_fast import assign_rows_csr


# functions

def _assign_labels_array(*args, **kwargs): # real signature unknown
    """
    Compute label assignment and inertia for a dense array
    
        Return the inertia (sum of squared distances to the centers).
    """
    pass

def _assign_labels_csr(*args, **kwargs): # real signature unknown
    """
    Compute label assignment and inertia for a CSR input
    
        Return the inertia (sum of squared distances to the centers).
    """
    pass

def _centers_dense(*args, **kwargs): # real signature unknown
    """
    M step of the K-means EM algorithm
    
        Computation of cluster centers / means.
    
        Parameters
        ----------
        X : array-like, shape (n_samples, n_features)
    
        sample_weight : array-like, shape (n_samples,)
            The weights for each observation in X.
    
        labels : array of integers, shape (n_samples)
            Current label assignment
    
        n_clusters : int
            Number of desired clusters
    
        distances : array-like, shape (n_samples)
            Distance to closest cluster for each sample.
    
        Returns
        -------
        centers : array, shape (n_clusters, n_features)
            The resulting centers
    """
    pass

def _centers_sparse(*args, **kwargs): # real signature unknown
    """
    M step of the K-means EM algorithm
    
        Computation of cluster centers / means.
    
        Parameters
        ----------
        X : scipy.sparse.csr_matrix, shape (n_samples, n_features)
    
        sample_weight : array-like, shape (n_samples,)
            The weights for each observation in X.
    
        labels : array of integers, shape (n_samples)
            Current label assignment
    
        n_clusters : int
            Number of desired clusters
    
        distances : array-like, shape (n_samples)
            Distance to closest cluster for each sample.
    
        Returns
        -------
        centers : array, shape (n_clusters, n_features)
            The resulting centers
    """
    pass

def _mini_batch_update_csr(*args, **kwargs): # real signature unknown
    """
    Incremental update of the centers for sparse MiniBatchKMeans.
    
        Parameters
        ----------
    
        X : CSR matrix, dtype float
            The complete (pre allocated) training set as a CSR matrix.
    
        centers : array, shape (n_clusters, n_features)
            The cluster centers
    
        counts : array, shape (n_clusters,)
             The vector in which we keep track of the numbers of elements in a
             cluster
    
        Returns
        -------
        inertia : float
            The inertia of the batch prior to centers update, i.e. the sum
            of squared distances to the closest center for each sample. This
            is the objective function being minimized by the k-means algorithm.
    
        squared_diff : float
            The sum of squared update (squared norm of the centers position
            change). If compute_squared_diff is 0, this computation is skipped and
            0.0 is returned instead.
    
        Both squared diff and inertia are commonly used to monitor the convergence
        of the algorithm.
    """
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x11C8B250>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.cluster._k_means', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x11C8B250>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\cluster\\\\_k_means.cp37-win32.pyd')"

__test__ = {}

