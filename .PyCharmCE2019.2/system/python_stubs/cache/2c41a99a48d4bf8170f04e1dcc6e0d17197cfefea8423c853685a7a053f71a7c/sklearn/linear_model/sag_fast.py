# encoding: utf-8
# module sklearn.linear_model.sag_fast
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\linear_model\sag_fast.cp37-win32.pyd
# by generator 1.147
"""
SAG and SAGA implementation
WARNING: Do not edit .pyx file directly, it is generated from .pyx.tp
"""

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def sag32(*args, **kwargs): # real signature unknown
    """
    Stochastic Average Gradient (SAG) and SAGA solvers.
    
        Used in Ridge and LogisticRegression.
    
        Reference
        ---------
        Schmidt, M., Roux, N. L., & Bach, F. (2013).
        Minimizing finite sums with the stochastic average gradient
        https://hal.inria.fr/hal-00860051/document
        (section 4.3)
    
        Defazio, A., Bach, F., Lacoste-Julien, S. (2014),
        SAGA: A Fast Incremental Gradient Method With Support
        for Non-Strongly Convex Composite Objectives
        https://arxiv.org/abs/1407.0202
    """
    pass

def sag64(*args, **kwargs): # real signature unknown
    """
    Stochastic Average Gradient (SAG) and SAGA solvers.
    
        Used in Ridge and LogisticRegression.
    
        Reference
        ---------
        Schmidt, M., Roux, N. L., & Bach, F. (2013).
        Minimizing finite sums with the stochastic average gradient
        https://hal.inria.fr/hal-00860051/document
        (section 4.3)
    
        Defazio, A., Bach, F., Lacoste-Julien, S. (2014),
        SAGA: A Fast Incremental Gradient Method With Support
        for Non-Strongly Convex Composite Objectives
        https://arxiv.org/abs/1407.0202
    """
    pass

def _multinomial_grad_loss_all_samples(*args, **kwargs): # real signature unknown
    """
    Compute multinomial gradient and loss across all samples.
    
        Used for testing purpose only.
    """
    pass

# classes

class MultinomialLogLoss32(object):
    # no doc
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x111E4518>'


class MultinomialLogLoss64(object):
    # no doc
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x111E4560>'


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x111F8910>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.linear_model.sag_fast', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x111F8910>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\linear_model\\\\sag_fast.cp37-win32.pyd')"

__test__ = {}

