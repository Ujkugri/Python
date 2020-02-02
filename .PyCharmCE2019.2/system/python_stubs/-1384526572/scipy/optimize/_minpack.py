# encoding: utf-8
# module scipy.optimize._minpack
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\optimize\_minpack.cp37-win32.pyd
# by generator 1.147
# no doc
# no imports

# Variables with simple values

__version__ = ' 1.10 '

# functions

def _chkder(m, n, x, fvec, fjac, ldfjac, xp, fvecp, mode, err): # real signature unknown; restored from __doc__
    """ _chkder(m,n,x,fvec,fjac,ldfjac,xp,fvecp,mode,err) """
    pass

def _hybrd(fun, x0, args, full_output, xtol, maxfev, ml, mu, epsfcn, factor, diag): # real signature unknown; restored from __doc__
    """ [x,infodict,info] = _hybrd(fun, x0, args, full_output, xtol, maxfev, ml, mu, epsfcn, factor, diag) """
    pass

def _hybrj(fun, Dfun, x0, args, full_output, col_deriv, xtol, maxfev, factor, diag): # real signature unknown; restored from __doc__
    """ [x,infodict,info] = _hybrj(fun, Dfun, x0, args, full_output, col_deriv, xtol, maxfev, factor, diag) """
    pass

def _lmder(fun, Dfun, x0, args, full_output, col_deriv, ftol, xtol, gtol, maxfev, factor, diag): # real signature unknown; restored from __doc__
    """ [x,infodict,info] = _lmder(fun, Dfun, x0, args, full_output, col_deriv, ftol, xtol, gtol, maxfev, factor, diag) """
    pass

def _lmdif(fun, x0, args, full_output, ftol, xtol, gtol, maxfev, epsfcn, factor, diag): # real signature unknown; restored from __doc__
    """ [x,infodict,info] = _lmdif(fun, x0, args, full_output, ftol, xtol, gtol, maxfev, epsfcn, factor, diag) """
    pass

# classes

class error(Exception):
    # no doc
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    __weakref__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """list of weak references to the object (if defined)"""



# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x103E4DD0>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.optimize._minpack', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x103E4DD0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\optimize\\\\_minpack.cp37-win32.pyd')"

