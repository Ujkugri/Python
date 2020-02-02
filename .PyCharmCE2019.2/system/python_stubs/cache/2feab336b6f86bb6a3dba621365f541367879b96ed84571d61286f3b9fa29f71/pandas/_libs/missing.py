# encoding: utf-8
# module pandas._libs.missing
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pandas\_libs\missing.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def checknull(*args, **kwargs): # real signature unknown
    """
    Return boolean describing of the input is NA-like, defined here as any
        of:
         - None
         - nan
         - NaT
         - np.datetime64 representation of NaT
         - np.timedelta64 representation of NaT
    
        Parameters
        ----------
        val : object
    
        Returns
        -------
        result : bool
    
        Notes
        -----
        The difference between `checknull` and `checknull_old` is that `checknull`
        does *not* consider INF or NEGINF to be NA.
    """
    pass

def checknull_old(*args, **kwargs): # real signature unknown
    """
    Return boolean describing of the input is NA-like, defined here as any
        of:
         - None
         - nan
         - INF
         - NEGINF
         - NaT
         - np.datetime64 representation of NaT
         - np.timedelta64 representation of NaT
    
        Parameters
        ----------
        val : object
    
        Returns
        -------
        result : bool
    
        Notes
        -----
        The difference between `checknull` and `checknull_old` is that `checknull`
        does *not* consider INF or NEGINF to be NA.
    """
    pass

def isnaobj(*args, **kwargs): # real signature unknown
    """
    Return boolean mask denoting which elements of a 1-D array are na-like,
        according to the criteria defined in `checknull`:
         - None
         - nan
         - NaT
         - np.datetime64 representation of NaT
         - np.timedelta64 representation of NaT
    
        Parameters
        ----------
        arr : ndarray
    
        Returns
        -------
        result : ndarray (dtype=np.bool_)
    """
    pass

def isnaobj2d(*args, **kwargs): # real signature unknown
    """
    Return boolean mask denoting which elements of a 2-D array are na-like,
        according to the criteria defined in `checknull`:
         - None
         - nan
         - NaT
         - np.datetime64 representation of NaT
         - np.timedelta64 representation of NaT
    
        Parameters
        ----------
        arr : ndarray
    
        Returns
        -------
        result : ndarray (dtype=np.bool_)
    
        Notes
        -----
        The difference between `isnaobj2d` and `isnaobj2d_old` is that `isnaobj2d`
        does *not* consider INF or NEGINF to be NA.
    """
    pass

def isnaobj2d_old(*args, **kwargs): # real signature unknown
    """
    Return boolean mask denoting which elements of a 2-D array are na-like,
        according to the criteria defined in `checknull_old`:
         - None
         - nan
         - INF
         - NEGINF
         - NaT
         - np.datetime64 representation of NaT
         - np.timedelta64 representation of NaT
    
        Parameters
        ----------
        arr : ndarray
    
        Returns
        -------
        result : ndarray (dtype=np.bool_)
    
        Notes
        -----
        The difference between `isnaobj2d` and `isnaobj2d_old` is that `isnaobj2d`
        does *not* consider INF or NEGINF to be NA.
    """
    pass

def isnaobj_old(*args, **kwargs): # real signature unknown
    """
    Return boolean mask denoting which elements of a 1-D array are na-like,
        defined as being any of:
         - None
         - nan
         - INF
         - NEGINF
         - NaT
    
        Parameters
        ----------
        arr : ndarray
    
        Returns
        -------
        result : ndarray (dtype=np.bool_)
    """
    pass

def isneginf_scalar(*args, **kwargs): # real signature unknown
    pass

def isposinf_scalar(*args, **kwargs): # real signature unknown
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0A1681B0>'

__pyx_capi__ = {
    'checknull': None, # (!) real value is '<capsule object "int (PyObject *, int __pyx_skip_dispatch)" at 0x0A1574D0>'
    'checknull_old': None, # (!) real value is '<capsule object "int (PyObject *, int __pyx_skip_dispatch)" at 0x0A1574A0>'
    'is_null_datetime64': None, # (!) real value is '<capsule object "int (PyObject *)" at 0x0A1574E8>'
    'is_null_period': None, # (!) real value is '<capsule object "int (PyObject *)" at 0x0A157470>'
    'is_null_timedelta64': None, # (!) real value is '<capsule object "int (PyObject *)" at 0x0A157518>'
    'isnaobj': None, # (!) real value is '<capsule object "PyArrayObject *(PyArrayObject *, int __pyx_skip_dispatch)" at 0x0A1574B8>'
}

__spec__ = None # (!) real value is "ModuleSpec(name='pandas._libs.missing', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0A1681B0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pandas\\\\_libs\\\\missing.cp37-win32.pyd')"

__test__ = {}

