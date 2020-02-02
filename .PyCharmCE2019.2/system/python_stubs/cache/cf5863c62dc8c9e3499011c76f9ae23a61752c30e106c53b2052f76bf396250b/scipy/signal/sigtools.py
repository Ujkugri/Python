# encoding: utf-8
# module scipy.signal.sigtools
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\signal\sigtools.cp37-win32.pyd
# by generator 1.147
# no doc
# no imports

# functions

def _convolve2d(in1, in2, flip, mode, boundary, fillvalue): # real signature unknown; restored from __doc__
    """ out = _convolve2d(in1, in2, flip, mode, boundary, fillvalue) """
    pass

def _correlateND(a, kernel, mode): # real signature unknown; restored from __doc__
    """
    out = _correlateND(a,kernel,mode) 
    
       mode = 0 - 'valid', 1 - 'same', 
      2 - 'full' (default)
    """
    pass

def _linear_filter(b, a, X, Dim=-1, Vi=None): # real signature unknown; restored from __doc__
    """ (y,Vf) = _linear_filter(b,a,X,Dim=-1,Vi=None)  implemented using Direct Form II transposed flow diagram. If Vi is not given, Vf is not returned. """
    pass

def _medfilt2d(*args, **kwargs): # real signature unknown
    """ filt = _median2d(data, size) """
    pass

def _order_filterND(a, domain, order): # real signature unknown; restored from __doc__
    """ out = _order_filterND(a,domain,order) """
    pass

def _remez(numtaps, bands, des, weight, type, fs, maxiter, grid_density): # real signature unknown; restored from __doc__
    """
    h = _remez(numtaps, bands, des, weight, type, fs, maxiter, grid_density)
      returns the optimal (in the Chebyshev/minimax sense) FIR filter impulse
      response given a set of band edges, the desired response on those bands,
      and the weight given to the error in those bands.  Bands is a monotonic
      vector with band edges given in frequency domain where fs is the sampling
      frequency.
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x09C8A190>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.signal.sigtools', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x09C8A190>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\signal\\\\sigtools.cp37-win32.pyd')"

