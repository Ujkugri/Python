# encoding: utf-8
# module pandas._libs.tslibs.resolution
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pandas\_libs\tslibs\resolution.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def get_freq_group(W_MON): # real signature unknown; restored from __doc__
    """
    Return frequency code group of given frequency str or offset.
    
        Example
        -------
        >>> get_freq_group('W-MON')
        4000
    
        >>> get_freq_group('W-FRI')
        4000
    """
    pass

def month_position_check(*args, **kwargs): # real signature unknown
    pass

def resolution(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# classes

class Resolution(object):
    # no doc
    @classmethod
    def get_freq(cls, day): # real signature unknown; restored from __doc__
        """
        Return frequency str against resolution str.
        
                Example
                -------
                >>> f.Resolution.get_freq('day')
                'D'
        """
        pass

    @classmethod
    def get_freq_group(cls, day): # real signature unknown; restored from __doc__
        """
        Return frequency str against resolution str.
        
                Example
                -------
                >>> f.Resolution.get_freq_group('day')
                4000
        """
        pass

    @classmethod
    def get_reso(cls, second): # real signature unknown; restored from __doc__
        """
        Return resolution str against resolution code.
        
                Example
                -------
                >>> Resolution.get_reso('second')
                2
        
                >>> Resolution.get_reso('second') == Resolution.RESO_SEC
                True
        """
        pass

    @classmethod
    def get_reso_from_freq(cls, H): # real signature unknown; restored from __doc__
        """
        Return resolution code against frequency str.
        
                Example
                -------
                >>> Resolution.get_reso_from_freq('H')
                4
        
                >>> Resolution.get_reso_from_freq('H') == Resolution.RESO_HR
                True
        """
        pass

    @classmethod
    def get_str(cls, Resolution_RESO_SEC): # real signature unknown; restored from __doc__
        """
        Return resolution str against resolution code.
        
                Example
                -------
                >>> Resolution.get_str(Resolution.RESO_SEC)
                'second'
        """
        pass

    @classmethod
    def get_stride_from_decimal(cls, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Convert freq with decimal stride into a higher freq with integer stride
        
                Parameters
                ----------
                value : integer or float
                freq : string
                    Frequency string
        
                Raises
                ------
                ValueError
                    If the float cannot be converted to an integer at any resolution.
        
                Example
                -------
                >>> Resolution.get_stride_from_decimal(1.5, 'T')
                (90, 'S')
        
                >>> Resolution.get_stride_from_decimal(1.04, 'H')
                (3744, 'S')
        
                >>> Resolution.get_stride_from_decimal(1, 'D')
                (1, 'D')
        """
        pass

    @classmethod
    def get_str_from_freq(cls, H): # real signature unknown; restored from __doc__
        """
        Return resolution str against frequency str.
        
                Example
                -------
                >>> Resolution.get_str_from_freq('H')
                'hour'
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    __weakref__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """list of weak references to the object (if defined)"""


    RESO_DAY = 6
    RESO_HR = 5
    RESO_MIN = 4
    RESO_MS = 2
    RESO_NS = 0
    RESO_SEC = 3
    RESO_US = 1
    _freq_reso_map = {
        'A': 'year',
        'D': 'day',
        'H': 'hour',
        'L': 'millisecond',
        'M': 'month',
        'N': 'nanosecond',
        'Q': 'quarter',
        'S': 'second',
        'T': 'minute',
        'U': 'microsecond',
    }
    _reso_freq_map = {
        'day': 'D',
        'hour': 'H',
        'microsecond': 'U',
        'millisecond': 'L',
        'minute': 'T',
        'month': 'M',
        'nanosecond': 'N',
        'quarter': 'Q',
        'second': 'S',
        'year': 'A',
    }
    _reso_mult_map = {
        0: None,
        1: 1000,
        2: 1000,
        3: 1000,
        4: 60,
        5: 60,
        6: 24,
    }
    _reso_str_bump_map = {
        'D': 'H',
        'H': 'T',
        'L': 'U',
        'N': None,
        'S': 'L',
        'T': 'S',
        'U': 'N',
    }
    _reso_str_map = {
        0: 'nanosecond',
        1: 'microsecond',
        2: 'millisecond',
        3: 'second',
        4: 'minute',
        5: 'hour',
        6: 'day',
    }
    _str_reso_map = {
        'day': 6,
        'hour': 5,
        'microsecond': 1,
        'millisecond': 2,
        'minute': 4,
        'nanosecond': 0,
        'second': 3,
    }
    __dict__ = None # (!) real value is "mappingproxy({'__module__': 'pandas._libs.tslibs.resolution', 'RESO_NS': 0, 'RESO_US': 1, 'RESO_MS': 2, 'RESO_SEC': 3, 'RESO_MIN': 4, 'RESO_HR': 5, 'RESO_DAY': 6, '_reso_str_map': {0: 'nanosecond', 1: 'microsecond', 2: 'millisecond', 3: 'second', 4: 'minute', 5: 'hour', 6: 'day'}, '_reso_mult_map': {0: None, 1: 1000, 2: 1000, 3: 1000, 4: 60, 5: 60, 6: 24}, '_reso_str_bump_map': {'D': 'H', 'H': 'T', 'T': 'S', 'S': 'L', 'L': 'U', 'U': 'N', 'N': None}, '_str_reso_map': {'nanosecond': 0, 'microsecond': 1, 'millisecond': 2, 'second': 3, 'minute': 4, 'hour': 5, 'day': 6}, '_reso_freq_map': {'year': 'A', 'quarter': 'Q', 'month': 'M', 'day': 'D', 'hour': 'H', 'minute': 'T', 'second': 'S', 'millisecond': 'L', 'microsecond': 'U', 'nanosecond': 'N'}, '_freq_reso_map': {'A': 'year', 'Q': 'quarter', 'M': 'month', 'D': 'day', 'H': 'hour', 'T': 'minute', 'S': 'second', 'L': 'millisecond', 'U': 'microsecond', 'N': 'nanosecond'}, 'get_str': <classmethod object at 0x097BB3B0>, 'get_reso': <classmethod object at 0x097C2ED0>, 'get_freq_group': <classmethod object at 0x097CB1D0>, 'get_freq': <classmethod object at 0x097CB650>, 'get_str_from_freq': <classmethod object at 0x097CB670>, 'get_reso_from_freq': <classmethod object at 0x097CB6B0>, 'get_stride_from_decimal': <classmethod object at 0x097CB6D0>, '__dict__': <attribute '__dict__' of 'Resolution' objects>, '__weakref__': <attribute '__weakref__' of 'Resolution' objects>, '__doc__': None})"


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x097CB1F0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pandas._libs.tslibs.resolution', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x097CB1F0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pandas\\\\_libs\\\\tslibs\\\\resolution.cp37-win32.pyd')"

__test__ = {
    'Resolution.get_freq (line 232)': "\n        Return frequency str against resolution str.\n\n        Example\n        -------\n        >>> f.Resolution.get_freq('day')\n        'D'\n        ",
    'Resolution.get_freq_group (line 220)': "\n        Return frequency str against resolution str.\n\n        Example\n        -------\n        >>> f.Resolution.get_freq_group('day')\n        4000\n        ",
    'Resolution.get_reso (line 205)': "\n        Return resolution str against resolution code.\n\n        Example\n        -------\n        >>> Resolution.get_reso('second')\n        2\n\n        >>> Resolution.get_reso('second') == Resolution.RESO_SEC\n        True\n        ",
    'Resolution.get_reso_from_freq (line 256)': "\n        Return resolution code against frequency str.\n\n        Example\n        -------\n        >>> Resolution.get_reso_from_freq('H')\n        4\n\n        >>> Resolution.get_reso_from_freq('H') == Resolution.RESO_HR\n        True\n        ",
    'Resolution.get_str (line 193)': "\n        Return resolution str against resolution code.\n\n        Example\n        -------\n        >>> Resolution.get_str(Resolution.RESO_SEC)\n        'second'\n        ",
    'Resolution.get_str_from_freq (line 244)': "\n        Return resolution str against frequency str.\n\n        Example\n        -------\n        >>> Resolution.get_str_from_freq('H')\n        'hour'\n        ",
    'Resolution.get_stride_from_decimal (line 271)': "\n        Convert freq with decimal stride into a higher freq with integer stride\n\n        Parameters\n        ----------\n        value : integer or float\n        freq : string\n            Frequency string\n\n        Raises\n        ------\n        ValueError\n            If the float cannot be converted to an integer at any resolution.\n\n        Example\n        -------\n        >>> Resolution.get_stride_from_decimal(1.5, 'T')\n        (90, 'S')\n\n        >>> Resolution.get_stride_from_decimal(1.04, 'H')\n        (3744, 'S')\n\n        >>> Resolution.get_stride_from_decimal(1, 'D')\n        (1, 'D')\n        ",
    'get_freq_group (line 110)': "\n    Return frequency code group of given frequency str or offset.\n\n    Example\n    -------\n    >>> get_freq_group('W-MON')\n    4000\n\n    >>> get_freq_group('W-FRI')\n    4000\n    ",
}

