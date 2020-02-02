# encoding: utf-8
# module sklearn.ensemble._hist_gradient_boosting.splitting
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\ensemble\_hist_gradient_boosting\splitting.cp37-win32.pyd
# by generator 1.147
"""
This module contains routines and data structures to:

- Find the best possible split of a node. For a given node, a split is
  characterized by a feature and a bin.
- Apply a split to a node, i.e. split the indices of the samples at the node
  into the newly created left and right childs.
"""

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Splitter(*args, **kwargs): # real signature unknown
    pass

# classes

class SplitInfo(object):
    """
    Pure data class to store information about a potential split.
    
        Parameters
        ----------
        gain : float
            The gain of the split.
        feature_idx : int
            The index of the feature to be split.
        bin_idx : int
            The index of the bin on which the split is made.
        sum_gradient_left : float
            The sum of the gradients of all the samples in the left child.
        sum_hessian_left : float
            The sum of the hessians of all the samples in the left child.
        sum_gradient_right : float
            The sum of the gradients of all the samples in the right child.
        sum_hessian_right : float
            The sum of the hessians of all the samples in the right child.
        n_samples_left : int, default=0
            The number of samples in the left child.
        n_samples_right : int
            The number of samples in the right child.
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    __weakref__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """list of weak references to the object (if defined)"""


    __dict__ = None # (!) real value is "mappingproxy({'__module__': 'sklearn.ensemble._hist_gradient_boosting.splitting', '__doc__': 'Pure data class to store information about a potential split.\\n\\n    Parameters\\n    ----------\\n    gain : float\\n        The gain of the split.\\n    feature_idx : int\\n        The index of the feature to be split.\\n    bin_idx : int\\n        The index of the bin on which the split is made.\\n    sum_gradient_left : float\\n        The sum of the gradients of all the samples in the left child.\\n    sum_hessian_left : float\\n        The sum of the hessians of all the samples in the left child.\\n    sum_gradient_right : float\\n        The sum of the gradients of all the samples in the right child.\\n    sum_hessian_right : float\\n        The sum of the hessians of all the samples in the right child.\\n    n_samples_left : int, default=0\\n        The number of samples in the left child.\\n    n_samples_right : int\\n        The number of samples in the right child.\\n    ', '__init__': <cyfunction SplitInfo.__init__ at 0x11BA26B0>, '__dict__': <attribute '__dict__' of 'SplitInfo' objects>, '__weakref__': <attribute '__weakref__' of 'SplitInfo' objects>})"


class Splitter(object):
    """
    Splitter used to find the best possible split at each node.
    
        A split (see SplitInfo) is characterized by a feature and a bin.
    
        The Splitter is also responsible for partitioning the samples among the
        leaves of the tree (see split_indices() and the partition attribute).
    
        Parameters
        ----------
        X_binned : ndarray of int, shape (n_samples, n_features)
            The binned input samples. Must be Fortran-aligned.
        max_bins : int
            The maximum number of bins. Used to define the shape of the
            histograms.
        actual_n_bins : ndarray, shape (n_features,)
            The actual number of bins needed for each feature, which is lower or
            equal to max_bins.
        l2_regularization : float
            The L2 regularization parameter.
        min_hessian_to_split : float, default=1e-3
            The minimum sum of hessians needed in each node. Splits that result in
            at least one child having a sum of hessians less than
            min_hessian_to_split are discarded.
        min_samples_leaf : int, default=20
            The minimum number of samples per leaf.
        min_gain_to_split : float, default=0.0
            The minimum gain needed to split a node. Splits with lower gain will
            be ignored.
        hessians_are_constant: bool, default is False
            Whether hessians are constant.
    """
    def find_node_split(self, *args, **kwargs): # real signature unknown
        """
        For each feature, find the best bin to split on at a given node.
        
                Return the best split info among all features.
        
                Parameters
                ----------
                sample_indices : ndarray of unsigned int, shape (n_samples_at_node,)
                    The indices of the samples at the node to split.
                histograms : ndarray of HISTOGRAM_DTYPE of                 shape (n_features, max_bins)
                    The histograms of the current node.
                sum_gradients : float
                    The sum of the gradients for each sample at the node.
                sum_hessians : float
                    The sum of the hessians for each sample at the node.
        
                Returns
                -------
                best_split_info : SplitInfo
                    The info about the best possible split among all features.
        """
        pass

    def split_indices(self, *args, **kwargs): # real signature unknown
        """
        Split samples into left and right arrays.
        
                The split is performed according to the best possible split
                (split_info).
        
                Ultimately, this is nothing but a partition of the sample_indices
                array with a given pivot, exactly like a quicksort subroutine.
        
                Parameters
                ----------
                split_info : SplitInfo
                    The SplitInfo of the node to split.
                sample_indices : ndarray of unsigned int, shape (n_samples_at_node,)
                    The indices of the samples at the node to split. This is a view
                    on self.partition, and it is modified inplace by placing the
                    indices of the left child at the beginning, and the indices of
                    the right child at the end.
        
                Returns
                -------
                left_indices : ndarray of int, shape (n_left_samples,)
                    The indices of the samples in the left child. This is a view on
                    self.partition.
                right_indices : ndarray of int, shape (n_right_samples,)
                    The indices of the samples in the right child. This is a view on
                    self.partition.
                right_child_position : int
                    The position of the right child in ``sample_indices``.
        """
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

    actual_n_bins = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    hessians_are_constant = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    l2_regularization = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    left_indices_buffer = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    max_bins = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    min_gain_to_split = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    min_hessian_to_split = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    min_samples_leaf = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_features = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    partition = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    right_indices_buffer = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    X_binned = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x03AA6C50>'


# variables with complex values

HISTOGRAM_DTYPE = None # (!) real value is "dtype([('sum_gradients', '<f8'), ('sum_hessians', '<f8'), ('count', '<u4')])"

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x103D2ED0>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.ensemble._hist_gradient_boosting.splitting', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x103D2ED0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\ensemble\\\\_hist_gradient_boosting\\\\splitting.cp37-win32.pyd')"

__test__ = {}

