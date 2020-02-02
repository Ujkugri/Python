# encoding: utf-8
# module matplotlib._png
# from C:\Program Files (x86)\Python37-32\lib\site-packages\matplotlib\_png.cp37-win32.pyd
# by generator 1.147
# no doc
# no imports

# Variables with simple values

PNG_FILTER_AVG = 64
PNG_FILTER_NONE = 8
PNG_FILTER_PAETH = 128
PNG_FILTER_SUB = 16
PNG_FILTER_UP = 32

# functions

def read_png(file): # real signature unknown; restored from __doc__
    """
    read_png(file)
    
    Read in a PNG file, converting values to floating-point doubles
    in the range (0, 1)
    
    Alias for read_png_float()
    
    Parameters
    ----------
    file : str path or file-like object
    """
    pass

def read_png_float(file): # real signature unknown; restored from __doc__
    """
    read_png_float(file)
    
    Read in a PNG file, converting values to floating-point doubles
    in the range (0, 1)
    
    Parameters
    ----------
    file : str path or file-like object
    """
    pass

def read_png_int(file): # real signature unknown; restored from __doc__
    """
    read_png_int(file)
    
    Read in a PNG file with original integer values.
    
    Parameters
    ----------
    file : str path or file-like object
    """
    pass

def write_png(buffer, file, dpi=0, compression=6, filter=None, metadata=None): # real signature unknown; restored from __doc__
    """
    write_png(buffer, file, dpi=0, compression=6, filter=auto, metadata=None)
    
    Parameters
    ----------
    buffer : numpy array of image data
        Must be an MxNxD array of dtype uint8.
        - if D is 1, the image is greyscale
        - if D is 3, the image is RGB
        - if D is 4, the image is RGBA
    
    file : str path, file-like object or None
        - If a str, must be a file path
        - If a file-like object, must write bytes
        - If None, a byte string containing the PNG data will be returned
    
    dpi : float
        The dpi to store in the file metadata.
    
    compression : int
        The level of lossless zlib compression to apply.  0 indicates no
        compression.  Values 1-9 indicate low/fast through high/slow
        compression.  Default is 6.
    
    filter : int
        Filter to apply.  Must be one of the constants: PNG_FILTER_NONE,
        PNG_FILTER_SUB, PNG_FILTER_UP, PNG_FILTER_AVG, PNG_FILTER_PAETH.
        See the PNG standard for more information.
        If not provided, libpng will try to automatically determine the
        best filter on a line-by-line basis.
    
    metadata : dictionary
        The keyword-text pairs that are stored as comments in the image.
        Keys must be shorter than 79 chars. The only supported encoding
        for both keywords and values is Latin-1 (ISO 8859-1).
        Examples given in the PNG Specification are:
        - Title: Short (one line) title or caption for image
        - Author: Name of image's creator
        - Description: Description of image (possibly long)
        - Copyright: Copyright notice
        - Creation Time: Time of original image creation
                         (usually RFC 1123 format, see below)
        - Software: Software used to create the image
        - Disclaimer: Legal disclaimer
        - Warning: Warning of nature of content
        - Source: Device used to create the image
        - Comment: Miscellaneous comment; conversion
                   from other image format
    
        For more details see the PNG specification:
        https://www.w3.org/TR/2003/REC-PNG-20031110/#11keywords
    
    Returns
    -------
    buffer : bytes or None
        Byte string containing the PNG content if None was passed in for
        file, otherwise None is returned.
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x03A901D0>'

__spec__ = None # (!) real value is "ModuleSpec(name='matplotlib._png', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x03A901D0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\matplotlib\\\\_png.cp37-win32.pyd')"

