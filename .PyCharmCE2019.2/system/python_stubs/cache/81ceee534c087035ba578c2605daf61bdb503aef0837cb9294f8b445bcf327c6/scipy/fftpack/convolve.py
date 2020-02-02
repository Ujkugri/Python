# encoding: utf-8
# module scipy.fftpack.convolve
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\fftpack\convolve.cp37-win32.pyd
# by generator 1.147
"""
This module 'convolve' is auto-generated with f2py (version:2).
Functions:
  omega = init_convolution_kernel(n,kernel_func,d=0,zero_nyquist=d%2,kernel_func_extra_args=())
  destroy_convolve_cache()
  y = convolve(x,omega,swap_real_imag=0,overwrite_x=0)
  y = convolve_z(x,omega_real,omega_imag,overwrite_x=0)
.
"""
# no imports

# Variables with simple values

__version__ = b'$Revision: $'

# functions

def convolve(x, omega, swap_real_imag=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = convolve(x,omega,[swap_real_imag,overwrite_x])
    
    Wrapper for ``convolve``.
    
    Parameters
    ----------
    x : input rank-1 array('d') with bounds (n)
    omega : input rank-1 array('d') with bounds (n)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    swap_real_imag : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('d') with bounds (n) and x storage
    """
    pass

def convolve_z(x, omega_real, omega_imag, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = convolve_z(x,omega_real,omega_imag,[overwrite_x])
    
    Wrapper for ``convolve_z``.
    
    Parameters
    ----------
    x : input rank-1 array('d') with bounds (n)
    omega_real : input rank-1 array('d') with bounds (n)
    omega_imag : input rank-1 array('d') with bounds (n)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('d') with bounds (n) and x storage
    """
    pass

def destroy_convolve_cache(): # real signature unknown; restored from __doc__
    """
    destroy_convolve_cache()
    
    Wrapper for ``destroy_convolve_cache``.
    """
    pass

def init_convolution_kernel(n, kernel_func, d=None, zero_nyquist=None, kernel_func_extra_args=None): # real signature unknown; restored from __doc__
    """
    omega = init_convolution_kernel(n,kernel_func,[d,zero_nyquist,kernel_func_extra_args])
    
    Wrapper for ``init_convolution_kernel``.
    
    Parameters
    ----------
    n : input int
    kernel_func : call-back function
    
    Other Parameters
    ----------------
    d : input int, optional
        Default: 0
    kernel_func_extra_args : input tuple, optional
        Default: ()
    zero_nyquist : input int, optional
        Default: d%2
    
    Returns
    -------
    omega : rank-1 array('d') with bounds (n)
    
    Notes
    -----
    Call-back functions::
    
      def kernel_func(k): return kernel_func
      Required arguments:
        k : input int
      Return objects:
        kernel_func : float
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0A19AD10>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.fftpack.convolve', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0A19AD10>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\fftpack\\\\convolve.cp37-win32.pyd')"

