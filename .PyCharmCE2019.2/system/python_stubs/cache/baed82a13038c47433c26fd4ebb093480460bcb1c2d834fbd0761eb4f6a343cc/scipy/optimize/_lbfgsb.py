# encoding: utf-8
# module scipy.optimize._lbfgsb
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\optimize\_lbfgsb.cp37-win32.pyd
# by generator 1.147
"""
This module '_lbfgsb' is auto-generated with f2py (version:2).
Functions:
  setulb(m,x,l,u,nbd,f,g,factr,pgtol,wa,iwa,task,iprint,csave,lsave,isave,dsave,maxls,n=len(x))
.
"""
# no imports

# Variables with simple values

__version__ = b'$Revision: $'

# functions

def setulb(m, x, l, u, nbd, f, g, factr, pgtol, wa, iwa, task, iprint, csave, lsave, isave, dsave, maxls, n=None): # real signature unknown; restored from __doc__
    """
    setulb(m,x,l,u,nbd,f,g,factr,pgtol,wa,iwa,task,iprint,csave,lsave,isave,dsave,maxls,[n])
    
    Wrapper for ``setulb``.
    
    Parameters
    ----------
    m : input int
    x : in/output rank-1 array('d') with bounds (n)
    l : input rank-1 array('d') with bounds (n)
    u : input rank-1 array('d') with bounds (n)
    nbd : input rank-1 array('i') with bounds (n)
    f : in/output rank-0 array(float,'d')
    g : in/output rank-1 array('d') with bounds (n)
    factr : input float
    pgtol : input float
    wa : in/output rank-1 array('d') with bounds (2*m*n+5*n+11*m*m+8*m)
    iwa : in/output rank-1 array('i') with bounds (3 * n)
    task : in/output rank-0 array(string(len=60),'c')
    iprint : input int
    csave : in/output rank-0 array(string(len=60),'c')
    lsave : in/output rank-1 array('i') with bounds (4)
    isave : in/output rank-1 array('i') with bounds (44)
    dsave : in/output rank-1 array('d') with bounds (29)
    maxls : input int
    
    Other Parameters
    ----------------
    n : input int, optional
        Default: len(x)
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0F44B4F0>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.optimize._lbfgsb', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0F44B4F0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\optimize\\\\_lbfgsb.cp37-win32.pyd')"

