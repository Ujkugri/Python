# encoding: utf-8
# module scipy.cluster._vq
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\cluster\_vq.cp37-win32.pyd
# by generator 1.147
"""
Cython rewrite of the vector quantization module, originally written
in C at src/vq.c and the wrapper at src/vq_module.c. This should be
easier to maintain than old SWIG output.

Original C version by Damian Eads.
Translated to Cython by David Warde-Farley, October 2009.
"""

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def update_cluster_means(*args, **kwargs): # real signature unknown
    """
    The update-step of K-means. Calculate the mean of observations in each
        cluster.
    
        Parameters
        ----------
        obs : ndarray
            The observation matrix. Each row is an observation. Its dtype must be
            float32 or float64.
        labels : ndarray
            The label of each observation. Must be an 1d array.
        nc : int
            The number of centroids.
    
        Returns
        -------
        cb : ndarray
            The new code book.
        has_members : ndarray
            A boolean array indicating which clusters have members.
    
        Notes
        -----
        The empty clusters will be set to all zeros and the curresponding elements
        in `has_members` will be `False`. The upper level function should decide
        how to deal with them.
    """
    pass

def vq(*args, **kwargs): # real signature unknown
    """
    Vector quantization ndarray wrapper. Only support float32 and float64.
    
        Parameters
        ----------
        obs : ndarray
            The observation matrix. Each row is an observation.
        codes : ndarray
            The code book matrix.
    
        Notes
        -----
        The observation matrix and code book matrix should have same ndim and
        same number of columns (features). Only 1-dimensional and 2-dimensional
        arrays are supported.
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x093AAF30>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.cluster._vq', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x093AAF30>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\cluster\\\\_vq.cp37-win32.pyd')"

__test__ = {}

