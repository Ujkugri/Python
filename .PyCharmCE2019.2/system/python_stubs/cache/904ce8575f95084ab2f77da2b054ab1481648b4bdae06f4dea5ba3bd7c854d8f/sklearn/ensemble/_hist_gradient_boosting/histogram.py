# encoding: utf-8
# module sklearn.ensemble._hist_gradient_boosting.histogram
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\ensemble\_hist_gradient_boosting\histogram.cp37-win32.pyd
# by generator 1.147
""" This module contains routines for building histograms. """

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def _build_histogram(*args, **kwargs): # real signature unknown
    """ Return histogram for a given feature. """
    pass

def _build_histogram_naive(*args, **kwargs): # real signature unknown
    """
    Build histogram in a naive way, without optimizing for cache hit.
    
        Used in tests to compare with the optimized version.
    """
    pass

def _build_histogram_no_hessian(*args, **kwargs): # real signature unknown
    """
    Return histogram for a given feature, not updating hessians.
    
        Used when the hessians of the loss are constant (typically LS loss).
    """
    pass

def _build_histogram_root(*args, **kwargs): # real signature unknown
    """
    Compute histogram of the root node.
    
        Unlike other nodes, the root node has to find the split among *all* the
        samples from the training set. binned_feature and all_gradients /
        all_hessians already have a consistent ordering.
    """
    pass

def _build_histogram_root_no_hessian(*args, **kwargs): # real signature unknown
    """
    Compute histogram of the root node, not updating hessians.
    
        Used when the hessians of the loss are constant (typically LS loss).
    """
    pass

def _subtract_histograms(*args, **kwargs): # real signature unknown
    """ compute (hist_a - hist_b) in out """
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_HistogramBuilder(*args, **kwargs): # real signature unknown
    pass

# classes

class HistogramBuilder(object):
    """
    A Histogram builder... used to build histograms.
    
        A histogram is an array with n_bins entries of type HISTOGRAM_DTYPE. Each
        feature has its own histogram. A histogram contains the sum of gradients
        and hessians of all the samples belonging to each bin.
    
        There are different ways to build a histogram:
        - by subtraction: hist(child) = hist(parent) - hist(sibling)
        - from scratch. In this case we have rountines that update the hessians
          or not (not useful when hessians are constant for some losses e.g.
          least squares). Also, there's a special case for the root which
          contains all the samples, leading to some possible optimizations.
          Overall all the implementations look the same, and are optimized for
          cache hit.
    
        Parameters
        ----------
        X_binned : ndarray of int, shape (n_samples, n_features)
            The binned input samples. Must be Fortran-aligned.
        max_bins : int
            The maximum number of bins. Used to define the shape of the
            histograms.
        gradients : ndarray, shape (n_samples,)
            The gradients of each training sample. Those are the gradients of the
            loss w.r.t the predictions, evaluated at iteration i - 1.
        hessians : ndarray, shape (n_samples,)
            The hessians of each training sample. Those are the hessians of the
            loss w.r.t the predictions, evaluated at iteration i - 1.
        hessians_are_constant : bool
            Whether hessians are constant.
    """
    def compute_histograms_brute(self, *args, **kwargs): # real signature unknown
        """
        Compute the histograms of the node by scanning through all the data.
        
                For a given feature, the complexity is O(n_samples)
        
                Parameters
                ----------
                sample_indices : array of int, shape (n_samples_at_node,)
                    The indices of the samples at the node to split.
        
                Returns
                -------
                histograms : ndarray of HISTOGRAM_DTYPE, shape (n_features, max_bins)
                    The computed histograms of the current node.
        """
        pass

    def compute_histograms_subtraction(self, *args, **kwargs): # real signature unknown
        """
        Compute the histograms of the node using the subtraction trick.
        
                hist(parent) = hist(left_child) + hist(right_child)
        
                For a given feature, the complexity is O(n_bins). This is much more
                efficient than compute_histograms_brute, but it's only possible for one
                of the siblings.
        
                Parameters
                ----------
                parent_histograms : ndarray of HISTOGRAM_DTYPE,                 shape (n_features, max_bins)
                    The histograms of the parent.
                sibling_histograms : ndarray of HISTOGRAM_DTYPE,                 shape (n_features, max_bins)
                    The histograms of the sibling.
        
                Returns
                -------
                histograms : ndarray of HISTOGRAM_DTYPE, shape(n_features, max_bins)
                    The computed histograms of the current node.
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

    gradients = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    hessians = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    hessians_are_constant = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    max_bins = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    n_features = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    ordered_gradients = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    ordered_hessians = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    X_binned = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x01416C50>'


# variables with complex values

HISTOGRAM_DTYPE = None # (!) real value is "dtype([('sum_gradients', '<f8'), ('sum_hessians', '<f8'), ('count', '<u4')])"

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x11581DD0>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.ensemble._hist_gradient_boosting.histogram', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x11581DD0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\ensemble\\\\_hist_gradient_boosting\\\\histogram.cp37-win32.pyd')"

__test__ = {}

