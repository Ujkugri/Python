# encoding: utf-8
# module matplotlib._image
# from C:\Program Files (x86)\Python37-32\lib\site-packages\matplotlib\_image.cp37-win32.pyd
# by generator 1.147
# no doc
# no imports

# Variables with simple values

BESSEL = 12

BICUBIC = 2
BILINEAR = 1

BLACKMAN = 16

CATROM = 10

GAUSSIAN = 11

HAMMING = 6
HANNING = 5

HERMITE = 7

KAISER = 8

LANCZOS = 15

MITCHELL = 13

NEAREST = 0

QUADRIC = 9

SINC = 14

SPLINE16 = 3
SPLINE36 = 4

_n_interpolation = 17

# functions

def pcolor(x, y, data, rows, cols, bounds): # real signature unknown; restored from __doc__
    """
    pcolor(x, y, data, rows, cols, bounds)
    
    Generate a pseudo-color image from data on a non-uniform grid using
    nearest neighbour or linear interpolation.
    bounds = (x_min, x_max, y_min, y_max)
    interpolation = NEAREST or BILINEAR
    """
    pass

def pcolor2(x, y, data, rows, cols, bounds, bg): # real signature unknown; restored from __doc__
    """
    pcolor2(x, y, data, rows, cols, bounds, bg)
    
    Generate a pseudo-color image from data on a non-uniform grid
    specified by its cell boundaries.
    bounds = (x_left, x_right, y_bot, y_top)
    bg = ndarray of 4 uint8 representing background rgba
    """
    pass

def resample(input_array, output_array, matrix, interpolation=None, alpha=1.0, norm=False, radius=1): # real signature unknown; restored from __doc__
    """
    resample(input_array, output_array, matrix, interpolation=NEAREST, alpha=1.0, norm=False, radius=1)
    
    Resample input_array, blending it in-place into output_array, using an
    affine transformation.
    
    Parameters
    ----------
    input_array : 2-d or 3-d Numpy array of float, double or uint8
        If 2-d, the image is grayscale.  If 3-d, the image must be of size
        4 in the last dimension and represents RGBA data.
    
    output_array : 2-d or 3-d Numpy array of float, double or uint8
        The dtype and number of dimensions must match `input_array`.
    
    transform : matplotlib.transforms.Transform instance
        The transformation from the input array to the output
        array.
    
    interpolation : int, optional
        The interpolation method.  Must be one of the following constants
        defined in this module:
    
          NEAREST (default), BILINEAR, BICUBIC, SPLINE16, SPLINE36,
          HANNING, HAMMING, HERMITE, KAISER, QUADRIC, CATROM, GAUSSIAN,
          BESSEL, MITCHELL, SINC, LANCZOS, BLACKMAN
    
    resample : bool, optional
        When `True`, use a full resampling method.  When `False`, only
        resample when the output image is larger than the input image.
    
    alpha : float, optional
        The level of transparency to apply.  1.0 is completely opaque.
        0.0 is completely transparent.
    
    norm : bool, optional
        Whether to norm the interpolation function.  Default is `False`.
    
    radius: float, optional
        The radius of the kernel, if method is SINC, LANCZOS or BLACKMAN.
        Default is 1.
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x02F301D0>'

__spec__ = None # (!) real value is "ModuleSpec(name='matplotlib._image', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x02F301D0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\matplotlib\\\\_image.cp37-win32.pyd')"

