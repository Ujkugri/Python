# encoding: utf-8
# module scipy.stats.statlib
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\stats\statlib.cp37-win32.pyd
# by generator 1.147
"""
This module 'statlib' is auto-generated with f2py (version:2).
Functions:
  a,w,pw,ifault = swilk(x,a,init=0,n1=n)
  astart,a1,ifault = gscale(test,other)
  ifault = prho(n,is)
.
"""
# no imports

# Variables with simple values

__version__ = b'$Revision: $'

# functions

def gscale(test, other): # real signature unknown; restored from __doc__
    """
    astart,a1,ifault = gscale(test,other)
    
    Wrapper for ``gscale``.
    
    Parameters
    ----------
    test : input int
    other : input int
    
    Returns
    -------
    astart : float
    a1 : rank-1 array('f') with bounds (l1)
    ifault : int
    """
    pass

def prho(n, is_): # real signature unknown; restored from __doc__
    """
    ifault = prho(n,is)
    
    Wrapper for ``prho``.
    
    Parameters
    ----------
    n : input int
    is : input int
    
    Returns
    -------
    prho : float
    ifault : int
    """
    pass

def swilk(x, a, init=None, n1=None): # real signature unknown; restored from __doc__
    """
    a,w,pw,ifault = swilk(x,a,[init,n1])
    
    Wrapper for ``swilk``.
    
    Parameters
    ----------
    x : input rank-1 array('f') with bounds (n)
    a : input rank-1 array('f') with bounds (n2)
    
    Other Parameters
    ----------------
    init : input int, optional
        Default: 0
    n1 : input int, optional
        Default: n
    
    Returns
    -------
    a : rank-1 array('f') with bounds (n2)
    w : float
    pw : float
    ifault : int
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x101AB0F0>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.stats.statlib', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x101AB0F0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\stats\\\\statlib.cp37-win32.pyd')"

