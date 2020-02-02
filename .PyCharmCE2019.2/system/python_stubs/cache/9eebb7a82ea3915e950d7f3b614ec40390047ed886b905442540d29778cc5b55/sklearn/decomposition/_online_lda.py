# encoding: utf-8
# module sklearn.decomposition._online_lda
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\decomposition\_online_lda.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def mean_change(*args, **kwargs): # real signature unknown
    """
    Calculate the mean difference between two arrays.
    
        Equivalent to np.abs(arr_1 - arr2).mean().
    """
    pass

def _dirichlet_expectation_1d(*args, **kwargs): # real signature unknown
    """
    Dirichlet expectation for a single sample:
            exp(E[log(theta)]) for theta ~ Dir(doc_topic)
        after adding doc_topic_prior to doc_topic, in-place.
    
        Equivalent to
            doc_topic += doc_topic_prior
            out[:] = np.exp(psi(doc_topic) - psi(np.sum(doc_topic)))
    """
    pass

def _dirichlet_expectation_2d(*args, **kwargs): # real signature unknown
    """
    Dirichlet expectation for multiple samples:
        E[log(theta)] for theta ~ Dir(arr).
    
        Equivalent to psi(arr) - psi(np.sum(arr, axis=1))[:, np.newaxis].
    
        Note that unlike _dirichlet_expectation_1d, this function doesn't compute
        the exp and doesn't add in the prior.
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x10B33CB0>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.decomposition._online_lda', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x10B33CB0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\decomposition\\\\_online_lda.cp37-win32.pyd')"

__test__ = {}

