# encoding: utf-8
# module scipy.integrate._dop
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\integrate\_dop.cp37-win32.pyd
# by generator 1.147
"""
This module '_dop' is auto-generated with f2py (version:2).
Functions:
  x,y,iwork,idid = dopri5(fcn,x,y,xend,rtol,atol,solout,iout,work,iwork,fcn_extra_args=(),overwrite_y=0,solout_extra_args=())
  x,y,iwork,idid = dop853(fcn,x,y,xend,rtol,atol,solout,iout,work,iwork,fcn_extra_args=(),overwrite_y=0,solout_extra_args=())
.
"""
# no imports

# Variables with simple values

__version__ = b'$Revision: $'

# functions

def dop853(fcn, x, y, xend, rtol, atol, solout, iout, work, iwork, fcn_extra_args=None, overwrite_y=None, solout_extra_args=None): # real signature unknown; restored from __doc__
    """
    x,y,iwork,idid = dop853(fcn,x,y,xend,rtol,atol,solout,iout,work,iwork,[fcn_extra_args,overwrite_y,solout_extra_args])
    
    Wrapper for ``dop853``.
    
    Parameters
    ----------
    fcn : call-back function
    x : input float
    y : input rank-1 array('d') with bounds (n)
    xend : input float
    rtol : input rank-1 array('d') with bounds (*)
    atol : input rank-1 array('d') with bounds (*)
    solout : call-back function
    iout : input int
    work : input rank-1 array('d') with bounds (*)
    iwork : input rank-1 array('i') with bounds (*)
    
    Other Parameters
    ----------------
    fcn_extra_args : input tuple, optional
        Default: ()
    overwrite_y : input int, optional
        Default: 0
    solout_extra_args : input tuple, optional
        Default: ()
    
    Returns
    -------
    x : float
    y : rank-1 array('d') with bounds (n)
    iwork : rank-1 array('i') with bounds (*)
    idid : int
    
    Notes
    -----
    Call-back functions::
    
      def fcn(x,y): return f
      Required arguments:
        x : input float
        y : input rank-1 array('d') with bounds (n)
      Return objects:
        f : rank-1 array('d') with bounds (n)
      def solout(nr,xold,x,y,con,icomp,[nd]): return irtn
      Required arguments:
        nr : input int
        xold : input float
        x : input float
        y : input rank-1 array('d') with bounds (n)
        con : input rank-1 array('d') with bounds (5 * nd)
        icomp : input rank-1 array('i') with bounds (nd)
      Optional arguments:
        nd : input int, optional
        Default: (len(con))/(5)
      Return objects:
        irtn : int
    """
    pass

def dopri5(fcn, x, y, xend, rtol, atol, solout, iout, work, iwork, fcn_extra_args=None, overwrite_y=None, solout_extra_args=None): # real signature unknown; restored from __doc__
    """
    x,y,iwork,idid = dopri5(fcn,x,y,xend,rtol,atol,solout,iout,work,iwork,[fcn_extra_args,overwrite_y,solout_extra_args])
    
    Wrapper for ``dopri5``.
    
    Parameters
    ----------
    fcn : call-back function
    x : input float
    y : input rank-1 array('d') with bounds (n)
    xend : input float
    rtol : input rank-1 array('d') with bounds (*)
    atol : input rank-1 array('d') with bounds (*)
    solout : call-back function
    iout : input int
    work : input rank-1 array('d') with bounds (*)
    iwork : input rank-1 array('i') with bounds (*)
    
    Other Parameters
    ----------------
    fcn_extra_args : input tuple, optional
        Default: ()
    overwrite_y : input int, optional
        Default: 0
    solout_extra_args : input tuple, optional
        Default: ()
    
    Returns
    -------
    x : float
    y : rank-1 array('d') with bounds (n)
    iwork : rank-1 array('i') with bounds (*)
    idid : int
    
    Notes
    -----
    Call-back functions::
    
      def fcn(x,y): return f
      Required arguments:
        x : input float
        y : input rank-1 array('d') with bounds (n)
      Return objects:
        f : rank-1 array('d') with bounds (n)
      def solout(nr,xold,x,y,con,icomp,[nd]): return irtn
      Required arguments:
        nr : input int
        xold : input float
        x : input float
        y : input rank-1 array('d') with bounds (n)
        con : input rank-1 array('d') with bounds (5 * nd)
        icomp : input rank-1 array('i') with bounds (nd)
      Optional arguments:
        nd : input int, optional
        Default: (len(con))/(5)
      Return objects:
        irtn : int
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0FDFDDB0>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.integrate._dop', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0FDFDDB0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\integrate\\\\_dop.cp37-win32.pyd')"

