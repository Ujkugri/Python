# encoding: utf-8
# module sklearn.metrics.pairwise_fast
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\metrics\pairwise_fast.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def _chi2_kernel_fast(*args, **kwargs): # real signature unknown
    pass

def _sparse_manhattan(*args, **kwargs): # real signature unknown
    """
    Pairwise L1 distances for CSR matrices.
    
        Usage:
    
        >>> D = np.zeros(X.shape[0], Y.shape[0])
        >>> sparse_manhattan(X.data, X.indices, X.indptr,
        ...                  Y.data, Y.indices, Y.indptr,
        ...                  X.shape[1], D)
    """
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x10F66DF0>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.metrics.pairwise_fast', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x10F66DF0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\metrics\\\\pairwise_fast.cp37-win32.pyd')"

__test__ = {
    '_sparse_manhattan (line 42)': 'Pairwise L1 distances for CSR matrices.\n\n    Usage:\n\n    >>> D = np.zeros(X.shape[0], Y.shape[0])\n    >>> sparse_manhattan(X.data, X.indices, X.indptr,\n    ...                  Y.data, Y.indices, Y.indptr,\n    ...                  X.shape[1], D)\n    ',
}

