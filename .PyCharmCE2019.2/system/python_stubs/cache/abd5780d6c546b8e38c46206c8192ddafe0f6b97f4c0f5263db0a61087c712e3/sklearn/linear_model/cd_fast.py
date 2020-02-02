# encoding: utf-8
# module sklearn.linear_model.cd_fast
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\linear_model\cd_fast.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import numpy.linalg as linalg # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\linalg\__init__.py
import warnings as warnings # C:\Program Files (x86)\Python37-32\lib\warnings.py

# functions

def enet_coordinate_descent(*args, **kwargs): # real signature unknown
    """
    Cython version of the coordinate descent algorithm
            for Elastic-Net regression
    
            We minimize
    
            (1/2) * norm(y - X w, 2)^2 + alpha norm(w, 1) + (beta/2) norm(w, 2)^2
    """
    pass

def enet_coordinate_descent_gram(*args, **kwargs): # real signature unknown
    """
    Cython version of the coordinate descent algorithm
            for Elastic-Net regression
    
            We minimize
    
            (1/2) * w^T Q w - q^T w + alpha norm(w, 1) + (beta/2) * norm(w, 2)^2
    
            which amount to the Elastic-Net problem when:
            Q = X^T X (Gram matrix)
            q = X^T y
    """
    pass

def enet_coordinate_descent_multi_task(*args, **kwargs): # real signature unknown
    """
    Cython version of the coordinate descent algorithm
            for Elastic-Net mult-task regression
    
            We minimize
    
            (1/2) * norm(y - X w, 2)^2 + l1_reg ||w||_21 + (1/2) * l2_reg norm(w, 2)^2
    """
    pass

def sparse_enet_coordinate_descent(*args, **kwargs): # real signature unknown
    """
    Cython version of the coordinate descent algorithm for Elastic-Net
    
        We minimize:
    
            (1/2) * norm(y - X w, 2)^2 + alpha norm(w, 1) + (beta/2) * norm(w, 2)^2
    """
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# classes

class ConvergenceWarning(UserWarning):
    """
    Custom warning to capture convergence problems
    
        .. versionchanged:: 0.18
           Moved from sklearn.utils.
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    __weakref__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """list of weak references to the object (if defined)"""



# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x114AB710>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.linear_model.cd_fast', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x114AB710>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\linear_model\\\\cd_fast.cp37-win32.pyd')"

__test__ = {}

