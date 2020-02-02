# encoding: utf-8
# module scipy.signal.spline
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\signal\spline.cp37-win32.pyd
# by generator 1.147
# no doc
# no imports

# Variables with simple values

__version__ = '0.2'

# functions

def cspline2d(input, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
    """
    cspline2d(input {, lambda, precision}) -> ck
    
      Description:
    
        Return the third-order B-spline coefficients over a regularly spacedi
        input grid for the two-dimensional input image.  The lambda argument
        specifies the amount of smoothing.  The precision argument allows specifying
        the precision used when computing the infinite sum needed to apply mirror-
        symmetric boundary conditions.
    """
    pass

def qspline2d(input, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
    """
    qspline2d(input {, lambda, precision}) -> qk
    
      Description:
    
        Return the second-order B-spline coefficients over a regularly spaced
        input grid for the two-dimensional input image.  The lambda argument
        specifies the amount of smoothing.  The precision argument allows specifying
        the precision used when computing the infinite sum needed to apply mirror-
        symmetric boundary conditions.
    """
    pass

def sepfir2d(input, hrow, hcol): # real signature unknown; restored from __doc__
    """
    sepfir2d(input, hrow, hcol) -> output
    
      Description:
    
        Convolve the rank-2 input array with the separable filter defined by the
        rank-1 arrays hrow, and hcol. Mirror symmetric boundary conditions are
        assumed.  This function can be used to find an image given its B-spline
        representation.
    """
    pass

def symiirorder1(input, c0, z1, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
    """
    symiirorder1(input, c0, z1 {, precision}) -> output
    
        Implement a smoothing IIR filter with mirror-symmetric boundary conditions
        using a cascade of first-order sections.  The second section uses a
        reversed sequence.  This implements a system with the following
        transfer function and mirror-symmetric boundary conditions::
    
                               c0              
               H(z) = ---------------------    
                       (1-z1/z) (1 - z1 z)     
    
        The resulting signal will have mirror symmetric boundary conditions as well.
    
        Parameters
        ----------
        input : ndarray
            The input signal.
        c0, z1 : scalar
            Parameters in the transfer function.
        precision :
            Specifies the precision for calculating initial conditions
            of the recursive filter based on mirror-symmetric input.
    
        Returns
        -------
        output : ndarray
            The filtered signal.
    """
    pass

def symiirorder2(input, r, omega, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
    """
    symiirorder2(input, r, omega {, precision}) -> output
    
        Implement a smoothing IIR filter with mirror-symmetric boundary conditions
        using a cascade of second-order sections.  The second section uses a
        reversed sequence.  This implements the following transfer function::
    
                                      cs^2
             H(z) = ---------------------------------------
                    (1 - a2/z - a3/z^2) (1 - a2 z - a3 z^2 )
    
        where::
    
              a2 = (2 r cos omega)
              a3 = - r^2
              cs = 1 - 2 r cos omega + r^2
    
        Parameters
        ----------
        input : ndarray
            The input signal.
        r, omega : scalar
            Parameters in the transfer function.
        precision :
            Specifies the precision for calculating initial conditions
            of the recursive filter based on mirror-symmetric input.
    
        Returns
        -------
        output : ndarray
            The filtered signal.
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0FA05530>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.signal.spline', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0FA05530>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\signal\\\\spline.cp37-win32.pyd')"

