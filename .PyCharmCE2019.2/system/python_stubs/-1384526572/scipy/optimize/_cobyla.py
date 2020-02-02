# encoding: utf-8
# module scipy.optimize._cobyla
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\optimize\_cobyla.cp37-win32.pyd
# by generator 1.147
"""
This module '_cobyla' is auto-generated with f2py (version:2).
Functions:
  x,dinfo = minimize(calcfc,m,x,rhobeg,rhoend,dinfo,iprint=1,maxfun=100,calcfc_extra_args=())
.
"""
# no imports

# Variables with simple values

__version__ = b'$Revision: $'

# functions

def minimize(calcfc, m, x, rhobeg, rhoend, dinfo, iprint=None, maxfun=None, calcfc_extra_args=None): # real signature unknown; restored from __doc__
    """
    x,dinfo = minimize(calcfc,m,x,rhobeg,rhoend,dinfo,[iprint,maxfun,calcfc_extra_args])
    
    Wrapper for ``minimize``.
    
    Parameters
    ----------
    calcfc : call-back function
    m : input int
    x : input rank-1 array('d') with bounds (n)
    rhobeg : input float
    rhoend : input float
    dinfo : input rank-1 array('d') with bounds (4)
    
    Other Parameters
    ----------------
    calcfc_extra_args : input tuple, optional
        Default: ()
    iprint : input int, optional
        Default: 1
    maxfun : input int, optional
        Default: 100
    
    Returns
    -------
    x : rank-1 array('d') with bounds (n)
    dinfo : rank-1 array('d') with bounds (4)
    
    Notes
    -----
    Call-back functions::
    
      def calcfc(x,con): return f
      Required arguments:
        x : input rank-1 array('d') with bounds (n)
        con : input rank-1 array('d') with bounds (m)
      Return objects:
        f : float
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0F56C9B0>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.optimize._cobyla', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0F56C9B0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\optimize\\\\_cobyla.cp37-win32.pyd')"

