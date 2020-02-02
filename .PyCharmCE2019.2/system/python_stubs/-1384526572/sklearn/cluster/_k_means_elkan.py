# encoding: utf-8
# module sklearn.cluster._k_means_elkan
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\cluster\_k_means_elkan.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def euclidean_distances(X, Y=None, Y_norm_squared=None, squared=False, X_norm_squared=None): # reliably restored by inspect
    """
    Considering the rows of X (and Y=X) as vectors, compute the
        distance matrix between each pair of vectors.
    
        For efficiency reasons, the euclidean distance between a pair of row
        vector x and y is computed as::
    
            dist(x, y) = sqrt(dot(x, x) - 2 * dot(x, y) + dot(y, y))
    
        This formulation has two advantages over other ways of computing distances.
        First, it is computationally efficient when dealing with sparse data.
        Second, if one argument varies but the other remains unchanged, then
        `dot(x, x)` and/or `dot(y, y)` can be pre-computed.
    
        However, this is not the most precise way of doing this computation, and
        the distance matrix returned by this function may not be exactly
        symmetric as required by, e.g., ``scipy.spatial.distance`` functions.
    
        Read more in the :ref:`User Guide <metrics>`.
    
        Parameters
        ----------
        X : {array-like, sparse matrix}, shape (n_samples_1, n_features)
    
        Y : {array-like, sparse matrix}, shape (n_samples_2, n_features)
    
        Y_norm_squared : array-like, shape (n_samples_2, ), optional
            Pre-computed dot-products of vectors in Y (e.g.,
            ``(Y**2).sum(axis=1)``)
            May be ignored in some cases, see the note below.
    
        squared : boolean, optional
            Return squared Euclidean distances.
    
        X_norm_squared : array-like, shape = [n_samples_1], optional
            Pre-computed dot-products of vectors in X (e.g.,
            ``(X**2).sum(axis=1)``)
            May be ignored in some cases, see the note below.
    
        Notes
        -----
        To achieve better accuracy, `X_norm_squared` and `Y_norm_squared` may be
        unused if they are passed as ``float32``.
    
        Returns
        -------
        distances : array, shape (n_samples_1, n_samples_2)
    
        Examples
        --------
        >>> from sklearn.metrics.pairwise import euclidean_distances
        >>> X = [[0, 1], [1, 1]]
        >>> # distance between rows of X
        >>> euclidean_distances(X, X)
        array([[0., 1.],
               [1., 0.]])
        >>> # get distance to origin
        >>> euclidean_distances(X, [[0, 0]])
        array([[1.        ],
               [1.41421356]])
    
        See also
        --------
        paired_distances : distances betweens pairs of elements of X and Y.
    """
    pass

def k_means_elkan(*args, **kwargs): # real signature unknown
    """
    Run Elkan's k-means.
    
        Parameters
        ----------
        X_ : nd-array, shape (n_samples, n_features)
    
        sample_weight : nd-array, shape (n_samples,)
            The weights for each observation in X.
    
        n_clusters : int
            Number of clusters to find.
    
        init : nd-array, shape (n_clusters, n_features)
            Initial position of centers.
    
        tol : float, default=1e-4
            The relative increment in cluster means before declaring convergence.
    
        max_iter : int, default=30
        Maximum number of iterations of the k-means algorithm.
    
        verbose : bool, default=False
            Whether to be verbose.
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

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x1177B7B0>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.cluster._k_means_elkan', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x1177B7B0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\cluster\\\\_k_means_elkan.cp37-win32.pyd')"

__test__ = {}

