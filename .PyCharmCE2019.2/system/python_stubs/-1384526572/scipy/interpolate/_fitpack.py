# encoding: utf-8
# module scipy.interpolate._fitpack
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\interpolate\_fitpack.cp37-win32.pyd
# by generator 1.147
# no doc
# no imports

# Variables with simple values

__version__ = ' 1.7 '

# functions

def _bispev(tx, ty, c, kx, ky, x, y, nux, nuy): # real signature unknown; restored from __doc__
    """ [z,ier] = _bispev(tx,ty,c,kx,ky,x,y,nux,nuy) """
    pass

def _bspldismat(order, xk): # real signature unknown; restored from __doc__
    """
    B = _bspldismat(order,xk)
    Construct the kth derivative discontinuity jump constraint matrix 
    for spline fitting of order k given sample positions in xk.
    
    If xk is an integer (N+1), then the result is equivalent to
    xk=arange(N+1)+x0 for any value of x0.   This produces the
    integer-spaced matrix a bit faster.  If xk is a 2-tuple (N+1,dx)
    then it produces the result as if the sample distance were dx
    """
    pass

def _bspleval(xx, xk, coef, k, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
    """
    y = _bspleval(xx,xk,coef,k,{deriv (0)})
    
    The spline is defined by the approximation interval xk[0] to xk[-1],
    the length of xk (N+1), the order of the spline, k, and 
    the number of coeficients N+k.  The coefficients range from xk_{-K}
    to xk_{N-1} inclusive and are all the coefficients needed to define
    an arbitrary spline of order k, on the given approximation interval
    
    Extra knot points are internally added using knot-point symmetry 
    around xk[0] and xk[-1]
    """
    pass

def _bsplmat(order, xk): # real signature unknown; restored from __doc__
    """
    B = _bsplmat(order,xk)
    Construct the constraint matrix for spline fitting of order k
    given sample positions in xk.
    
    If xk is an integer (N+1), then the result is equivalent to
    xk=arange(N+1)+x0 for any value of x0.   This produces the
    integer-spaced, or cardinal spline matrix a bit faster.
    """
    pass

def _curfit(x, y, w, xb, xe, k, iopt, s, t, nest, wrk, iwrk, per): # real signature unknown; restored from __doc__
    """ [t,c,o] = _curfit(x,y,w,xb,xe,k,iopt,s,t,nest,wrk,iwrk,per) """
    pass

def _insert(iopt, t, c, k, x, m): # real signature unknown; restored from __doc__
    """ [tt,cc,ier] = _insert(iopt,t,c,k,x,m) """
    pass

def _parcur(x, w, u, ub, ue, k, iopt, ipar, s, t, nest, wrk, iwrk, per): # real signature unknown; restored from __doc__
    """ [t,c,o] = _parcur(x,w,u,ub,ue,k,iopt,ipar,s,t,nest,wrk,iwrk,per) """
    pass

def _spalde(t, c, k, x): # real signature unknown; restored from __doc__
    """ [d,ier] = _spalde(t,c,k,x) """
    pass

def _splint(t, c, k, a, b): # real signature unknown; restored from __doc__
    """ [aint,wrk] = _splint(t,c,k,a,b) """
    pass

def _spl_(x, nu, t, c, k, e): # real signature unknown; restored from __doc__
    """ [y,ier] = _spl_(x,nu,t,c,k,e) """
    pass

def _sproot(t, c, k, mest): # real signature unknown; restored from __doc__
    """ [z,ier] = _sproot(t,c,k,mest) """
    pass

def _surfit(x, y, z, w, xb, xe, yb, ye, kx, ky, iopt, s, eps, tx, ty, nxest, nyest, wrk, lwrk1, lwrk2): # real signature unknown; restored from __doc__
    """ [tx,ty,c,o] = _surfit(x, y, z, w, xb, xe, yb, ye, kx,ky,iopt,s,eps,tx,ty,nxest,nyest,wrk,lwrk1,lwrk2) """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x100EDC10>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.interpolate._fitpack', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x100EDC10>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\interpolate\\\\_fitpack.cp37-win32.pyd')"

