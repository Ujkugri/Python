# encoding: utf-8
# module sklearn.utils.weight_vector
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\utils\weight_vector.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# no functions
# classes

class WeightVector(object):
    """
    Dense vector represented by a scalar and a numpy array.
    
        The class provides methods to ``add`` a sparse vector
        and scale the vector.
        Representing a vector explicitly as a scalar times a
        vector allows for efficient scaling operations.
    
        Attributes
        ----------
        w : ndarray, dtype=double, order='C'
            The numpy array which backs the weight vector.
        aw : ndarray, dtype=double, order='C'
            The numpy array which backs the average_weight vector.
        w_data_ptr : double*
            A pointer to the data of the numpy array.
        wscale : double
            The scale of the vector.
        n_features : int
            The number of features (= dimensionality of ``w``).
        sq_norm : double
            The squared norm of ``w``.
    """
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x00E64D88>'


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0361E0D0>'

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.utils.weight_vector', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0361E0D0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\utils\\\\weight_vector.cp37-win32.pyd')"

__test__ = {}

