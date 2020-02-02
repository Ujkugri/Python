# encoding: utf-8
# module scipy.fftpack._fftpack
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\fftpack\_fftpack.cp37-win32.pyd
# by generator 1.147
"""
This module '_fftpack' is auto-generated with f2py (version:2).
Functions:
  y = zfft(x,n=size(x),direction=1,normalize=(direction<0),overwrite_x=0)
  y = drfft(x,n=size(x),direction=1,normalize=(direction<0),overwrite_x=0)
  y = zrfft(x,n=size(x),direction=1,normalize=(direction<0),overwrite_x=1)
  y = zfftnd(x,s=old_shape(x,j++),direction=1,normalize=(direction<0),overwrite_x=0)
  destroy_zfft_cache()
  destroy_zfftnd_cache()
  destroy_drfft_cache()
  y = cfft(x,n=size(x),direction=1,normalize=(direction<0),overwrite_x=0)
  y = rfft(x,n=size(x),direction=1,normalize=(direction<0),overwrite_x=0)
  y = crfft(x,n=size(x),direction=1,normalize=(direction<0),overwrite_x=1)
  y = cfftnd(x,s=old_shape(x,j++),direction=1,normalize=(direction<0),overwrite_x=0)
  destroy_cfft_cache()
  destroy_cfftnd_cache()
  destroy_rfft_cache()
  y = ddct1(x,n=size(x),normalize=0,overwrite_x=0)
  y = ddct2(x,n=size(x),normalize=0,overwrite_x=0)
  y = ddct3(x,n=size(x),normalize=0,overwrite_x=0)
  y = ddct4(x,n=size(x),normalize=0,overwrite_x=0)
  y = dct1(x,n=size(x),normalize=0,overwrite_x=0)
  y = dct2(x,n=size(x),normalize=0,overwrite_x=0)
  y = dct3(x,n=size(x),normalize=0,overwrite_x=0)
  y = dct4(x,n=size(x),normalize=0,overwrite_x=0)
  destroy_ddct2_cache()
  destroy_ddct1_cache()
  destroy_ddct4_cache()
  destroy_dct2_cache()
  destroy_dct1_cache()
  destroy_dct4_cache()
  y = ddst1(x,n=size(x),normalize=0,overwrite_x=0)
  y = ddst2(x,n=size(x),normalize=0,overwrite_x=0)
  y = ddst3(x,n=size(x),normalize=0,overwrite_x=0)
  y = ddst4(x,n=size(x),normalize=0,overwrite_x=0)
  y = dst1(x,n=size(x),normalize=0,overwrite_x=0)
  y = dst2(x,n=size(x),normalize=0,overwrite_x=0)
  y = dst3(x,n=size(x),normalize=0,overwrite_x=0)
  y = dst4(x,n=size(x),normalize=0,overwrite_x=0)
  destroy_ddst2_cache()
  destroy_ddst1_cache()
  destroy_dst2_cache()
  destroy_dst1_cache()
.
"""
# no imports

# Variables with simple values

__version__ = b'$Revision: $'

# functions

def cfft(x, n=None, direction=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = cfft(x,[n,direction,normalize,overwrite_x])
    
    Wrapper for ``cfft``.
    
    Parameters
    ----------
    x : input rank-1 array('F') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    direction : input int, optional
        Default: 1
    normalize : input int, optional
        Default: (direction<0)
    
    Returns
    -------
    y : rank-1 array('F') with bounds (*) and x storage
    """
    pass

def cfftnd(x, s=None, direction=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = cfftnd(x,[s,direction,normalize,overwrite_x])
    
    Wrapper for ``cfftnd``.
    
    Parameters
    ----------
    x : input rank-1 array('F') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    s : input rank-1 array('i') with bounds (r), optional
        Default: old_shape(x,j++)
    direction : input int, optional
        Default: 1
    normalize : input int, optional
        Default: (direction<0)
    
    Returns
    -------
    y : rank-1 array('F') with bounds (*) and x storage
    """
    pass

def crfft(x, n=None, direction=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = crfft(x,[n,direction,normalize,overwrite_x])
    
    Wrapper for ``crfft``.
    
    Parameters
    ----------
    x : input rank-1 array('F') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 1
    n : input int, optional
        Default: size(x)
    direction : input int, optional
        Default: 1
    normalize : input int, optional
        Default: (direction<0)
    
    Returns
    -------
    y : rank-1 array('F') with bounds (*) and x storage
    """
    pass

def dct1(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = dct1(x,[n,normalize,overwrite_x])
    
    Wrapper for ``dct1``.
    
    Parameters
    ----------
    x : input rank-1 array('f') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('f') with bounds (*) and x storage
    """
    pass

def dct2(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = dct2(x,[n,normalize,overwrite_x])
    
    Wrapper for ``dct2``.
    
    Parameters
    ----------
    x : input rank-1 array('f') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('f') with bounds (*) and x storage
    """
    pass

def dct3(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = dct3(x,[n,normalize,overwrite_x])
    
    Wrapper for ``dct3``.
    
    Parameters
    ----------
    x : input rank-1 array('f') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('f') with bounds (*) and x storage
    """
    pass

def dct4(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = dct4(x,[n,normalize,overwrite_x])
    
    Wrapper for ``dct4``.
    
    Parameters
    ----------
    x : input rank-1 array('f') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('f') with bounds (*) and x storage
    """
    pass

def ddct1(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = ddct1(x,[n,normalize,overwrite_x])
    
    Wrapper for ``ddct1``.
    
    Parameters
    ----------
    x : input rank-1 array('d') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('d') with bounds (*) and x storage
    """
    pass

def ddct2(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = ddct2(x,[n,normalize,overwrite_x])
    
    Wrapper for ``ddct2``.
    
    Parameters
    ----------
    x : input rank-1 array('d') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('d') with bounds (*) and x storage
    """
    pass

def ddct3(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = ddct3(x,[n,normalize,overwrite_x])
    
    Wrapper for ``ddct3``.
    
    Parameters
    ----------
    x : input rank-1 array('d') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('d') with bounds (*) and x storage
    """
    pass

def ddct4(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = ddct4(x,[n,normalize,overwrite_x])
    
    Wrapper for ``ddct4``.
    
    Parameters
    ----------
    x : input rank-1 array('d') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('d') with bounds (*) and x storage
    """
    pass

def ddst1(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = ddst1(x,[n,normalize,overwrite_x])
    
    Wrapper for ``ddst1``.
    
    Parameters
    ----------
    x : input rank-1 array('d') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('d') with bounds (*) and x storage
    """
    pass

def ddst2(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = ddst2(x,[n,normalize,overwrite_x])
    
    Wrapper for ``ddst2``.
    
    Parameters
    ----------
    x : input rank-1 array('d') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('d') with bounds (*) and x storage
    """
    pass

def ddst3(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = ddst3(x,[n,normalize,overwrite_x])
    
    Wrapper for ``ddst3``.
    
    Parameters
    ----------
    x : input rank-1 array('d') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('d') with bounds (*) and x storage
    """
    pass

def ddst4(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = ddst4(x,[n,normalize,overwrite_x])
    
    Wrapper for ``ddst4``.
    
    Parameters
    ----------
    x : input rank-1 array('d') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('d') with bounds (*) and x storage
    """
    pass

def destroy_cfftnd_cache(): # real signature unknown; restored from __doc__
    """
    destroy_cfftnd_cache()
    
    Wrapper for ``destroy_cfftnd_cache``.
    """
    pass

def destroy_cfft_cache(): # real signature unknown; restored from __doc__
    """
    destroy_cfft_cache()
    
    Wrapper for ``destroy_cfft_cache``.
    """
    pass

def destroy_dct1_cache(): # real signature unknown; restored from __doc__
    """
    destroy_dct1_cache()
    
    Wrapper for ``destroy_dct1_cache``.
    """
    pass

def destroy_dct2_cache(): # real signature unknown; restored from __doc__
    """
    destroy_dct2_cache()
    
    Wrapper for ``destroy_dct2_cache``.
    """
    pass

def destroy_dct4_cache(): # real signature unknown; restored from __doc__
    """
    destroy_dct4_cache()
    
    Wrapper for ``destroy_dct4_cache``.
    """
    pass

def destroy_ddct1_cache(): # real signature unknown; restored from __doc__
    """
    destroy_ddct1_cache()
    
    Wrapper for ``destroy_ddct1_cache``.
    """
    pass

def destroy_ddct2_cache(): # real signature unknown; restored from __doc__
    """
    destroy_ddct2_cache()
    
    Wrapper for ``destroy_ddct2_cache``.
    """
    pass

def destroy_ddct4_cache(): # real signature unknown; restored from __doc__
    """
    destroy_ddct4_cache()
    
    Wrapper for ``destroy_ddct4_cache``.
    """
    pass

def destroy_ddst1_cache(): # real signature unknown; restored from __doc__
    """
    destroy_ddst1_cache()
    
    Wrapper for ``destroy_ddst1_cache``.
    """
    pass

def destroy_ddst2_cache(): # real signature unknown; restored from __doc__
    """
    destroy_ddst2_cache()
    
    Wrapper for ``destroy_ddst2_cache``.
    """
    pass

def destroy_drfft_cache(): # real signature unknown; restored from __doc__
    """
    destroy_drfft_cache()
    
    Wrapper for ``destroy_drfft_cache``.
    """
    pass

def destroy_dst1_cache(): # real signature unknown; restored from __doc__
    """
    destroy_dst1_cache()
    
    Wrapper for ``destroy_dst1_cache``.
    """
    pass

def destroy_dst2_cache(): # real signature unknown; restored from __doc__
    """
    destroy_dst2_cache()
    
    Wrapper for ``destroy_dst2_cache``.
    """
    pass

def destroy_rfft_cache(): # real signature unknown; restored from __doc__
    """
    destroy_rfft_cache()
    
    Wrapper for ``destroy_rfft_cache``.
    """
    pass

def destroy_zfftnd_cache(): # real signature unknown; restored from __doc__
    """
    destroy_zfftnd_cache()
    
    Wrapper for ``destroy_zfftnd_cache``.
    """
    pass

def destroy_zfft_cache(): # real signature unknown; restored from __doc__
    """
    destroy_zfft_cache()
    
    Wrapper for ``destroy_zfft_cache``.
    """
    pass

def drfft(x, n=None, direction=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = drfft(x,[n,direction,normalize,overwrite_x])
    
    Wrapper for ``drfft``.
    
    Parameters
    ----------
    x : input rank-1 array('d') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    direction : input int, optional
        Default: 1
    normalize : input int, optional
        Default: (direction<0)
    
    Returns
    -------
    y : rank-1 array('d') with bounds (*) and x storage
    """
    pass

def dst1(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = dst1(x,[n,normalize,overwrite_x])
    
    Wrapper for ``dst1``.
    
    Parameters
    ----------
    x : input rank-1 array('f') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('f') with bounds (*) and x storage
    """
    pass

def dst2(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = dst2(x,[n,normalize,overwrite_x])
    
    Wrapper for ``dst2``.
    
    Parameters
    ----------
    x : input rank-1 array('f') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('f') with bounds (*) and x storage
    """
    pass

def dst3(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = dst3(x,[n,normalize,overwrite_x])
    
    Wrapper for ``dst3``.
    
    Parameters
    ----------
    x : input rank-1 array('f') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('f') with bounds (*) and x storage
    """
    pass

def dst4(x, n=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = dst4(x,[n,normalize,overwrite_x])
    
    Wrapper for ``dst4``.
    
    Parameters
    ----------
    x : input rank-1 array('f') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    normalize : input int, optional
        Default: 0
    
    Returns
    -------
    y : rank-1 array('f') with bounds (*) and x storage
    """
    pass

def rfft(x, n=None, direction=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = rfft(x,[n,direction,normalize,overwrite_x])
    
    Wrapper for ``rfft``.
    
    Parameters
    ----------
    x : input rank-1 array('f') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    direction : input int, optional
        Default: 1
    normalize : input int, optional
        Default: (direction<0)
    
    Returns
    -------
    y : rank-1 array('f') with bounds (*) and x storage
    """
    pass

def zfft(x, n=None, direction=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = zfft(x,[n,direction,normalize,overwrite_x])
    
    Wrapper for ``zfft``.
    
    Parameters
    ----------
    x : input rank-1 array('D') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    n : input int, optional
        Default: size(x)
    direction : input int, optional
        Default: 1
    normalize : input int, optional
        Default: (direction<0)
    
    Returns
    -------
    y : rank-1 array('D') with bounds (*) and x storage
    """
    pass

def zfftnd(x, s=None, direction=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = zfftnd(x,[s,direction,normalize,overwrite_x])
    
    Wrapper for ``zfftnd``.
    
    Parameters
    ----------
    x : input rank-1 array('D') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 0
    s : input rank-1 array('i') with bounds (r), optional
        Default: old_shape(x,j++)
    direction : input int, optional
        Default: 1
    normalize : input int, optional
        Default: (direction<0)
    
    Returns
    -------
    y : rank-1 array('D') with bounds (*) and x storage
    """
    pass

def zrfft(x, n=None, direction=None, normalize=None, overwrite_x=None): # real signature unknown; restored from __doc__
    """
    y = zrfft(x,[n,direction,normalize,overwrite_x])
    
    Wrapper for ``zrfft``.
    
    Parameters
    ----------
    x : input rank-1 array('D') with bounds (*)
    
    Other Parameters
    ----------------
    overwrite_x : input int, optional
        Default: 1
    n : input int, optional
        Default: size(x)
    direction : input int, optional
        Default: 1
    normalize : input int, optional
        Default: (direction<0)
    
    Returns
    -------
    y : rank-1 array('D') with bounds (*) and x storage
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0983A590>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.fftpack._fftpack', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0983A590>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\fftpack\\\\_fftpack.cp37-win32.pyd')"

