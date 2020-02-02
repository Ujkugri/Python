# encoding: utf-8
# module pandas._libs.tslibs.conversion
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pandas\_libs\tslibs\conversion.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import pytz as pytz # C:\Program Files (x86)\Python37-32\lib\site-packages\pytz\__init__.py
from pandas._libs.tslibs.np_datetime import OutOfBoundsDatetime

from pandas._libs.tslibs.parsing import parse_datetime_string

import datetime as __datetime


# Variables with simple values

DAY_SECONDS = 86400

HOUR_SECONDS = 3600

# functions

def datetime_to_datetime64(*args, **kwargs): # real signature unknown
    """
    Convert ndarray of datetime-like objects to int64 array representing
        nanosecond timestamps.
    
        Parameters
        ----------
        values : ndarray[object]
    
        Returns
        -------
        result : ndarray[int64_t]
        inferred_tz : tzinfo or None
    """
    pass

def ensure_datetime64ns(*args, **kwargs): # real signature unknown
    """
    Ensure a np.datetime64 array has dtype specifically 'datetime64[ns]'
    
        Parameters
        ----------
        arr : ndarray
        copy : boolean, default True
    
        Returns
        -------
        result : ndarray with dtype datetime64[ns]
    """
    pass

def ensure_timedelta64ns(*args, **kwargs): # real signature unknown
    """
    Ensure a np.timedelta64 array has dtype specifically 'timedelta64[ns]'
    
        Parameters
        ----------
        arr : ndarray
        copy : boolean, default True
    
        Returns
        -------
        result : ndarray with dtype timedelta64[ns]
    """
    pass

def is_date_array_normalized(*args, **kwargs): # real signature unknown
    """
    Check if all of the given (nanosecond) timestamps are normalized to
        midnight, i.e. hour == minute == second == 0.  If the optional timezone
        `tz` is not None, then this is midnight for this timezone.
    
        Parameters
        ----------
        stamps : int64 ndarray
        tz : tzinfo or None
    
        Returns
        -------
        is_normalized : bool True if all stamps are normalized
    """
    pass

def localize_pydatetime(*args, **kwargs): # real signature unknown
    """
    Take a datetime/Timestamp in UTC and localizes to timezone tz.
    
        Parameters
        ----------
        dt : datetime or Timestamp
        tz : tzinfo, "UTC", or None
    
        Returns
        -------
        localized : datetime or Timestamp
    """
    pass

def normalize_date(*args, **kwargs): # real signature unknown
    """
    Normalize datetime.datetime value to midnight. Returns datetime.date as a
        datetime.datetime at midnight
    
        Parameters
        ----------
        dt : date, datetime, or Timestamp
    
        Returns
        -------
        normalized : datetime.datetime or Timestamp
    
        Raises
        ------
        TypeError : if input is not datetime.date, datetime.datetime, or Timestamp
    """
    pass

def normalize_i8_timestamps(*args, **kwargs): # real signature unknown
    """
    Normalize each of the (nanosecond) timezone aware timestamps in the given
        array by rounding down to the beginning of the day (i.e. midnight).
        This is midnight for timezone, `tz`.
    
        Parameters
        ----------
        stamps : int64 ndarray
        tz : tzinfo or None
    
        Returns
        -------
        result : int64 ndarray of converted of normalized nanosecond timestamps
    """
    pass

def pydt_to_i8(*args, **kwargs): # real signature unknown
    """
    Convert to int64 representation compatible with numpy datetime64; converts
        to UTC
    
        Parameters
        ----------
        pydt : object
    
        Returns
        -------
        i8value : np.int64
    """
    pass

def tz_convert(*args, **kwargs): # real signature unknown
    """
    Convert the values (in i8) from timezone1 to timezone2
    
        Parameters
        ----------
        vals : int64 ndarray
        tz1 : string / timezone object
        tz2 : string / timezone object
    
        Returns
        -------
        int64 ndarray of converted
    """
    pass

def tz_convert_single(*args, **kwargs): # real signature unknown
    """
    Convert the val (in i8) from timezone1 to timezone2
    
        This is a single timezone version of tz_convert
    
        Parameters
        ----------
        val : int64
        tz1 : string / timezone object
        tz2 : string / timezone object
    
        Returns
        -------
        converted: int64
    """
    pass

def tz_localize_to_utc(*args, **kwargs): # real signature unknown
    """
    Localize tzinfo-naive i8 to given time zone (using pytz). If
        there are ambiguities in the values, raise AmbiguousTimeError.
    
        Parameters
        ----------
        vals : ndarray[int64_t]
        tz : tzinfo or None
        ambiguous : str, bool, or arraylike
            When clocks moved backward due to DST, ambiguous times may arise.
            For example in Central European Time (UTC+01), when going from 03:00
            DST to 02:00 non-DST, 02:30:00 local time occurs both at 00:30:00 UTC
            and at 01:30:00 UTC. In such a situation, the `ambiguous` parameter
            dictates how ambiguous times should be handled.
    
            - 'infer' will attempt to infer fall dst-transition hours based on
              order
            - bool-ndarray where True signifies a DST time, False signifies a
              non-DST time (note that this flag is only applicable for ambiguous
              times, but the array must have the same length as vals)
            - bool if True, treat all vals as DST. If False, treat them as non-DST
            - 'NaT' will return NaT where there are ambiguous times
    
        nonexistent : {None, "NaT", "shift_forward", "shift_backward", "raise",
                       timedelta-like}
            How to handle non-existent times when converting wall times to UTC
    
            .. versionadded:: 0.24.0
    
        Returns
        -------
        localized : ndarray[int64_t]
    """
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# classes

class datetime_time(object):
    """
    time([hour[, minute[, second[, microsecond[, tzinfo]]]]]) --> a time object
    
    All arguments are optional. tzinfo may be None, or an instance of
    a tzinfo subclass. The remaining arguments may be ints.
    """
    def dst(self): # real signature unknown; restored from __doc__
        """ Return self.tzinfo.dst(self). """
        pass

    @classmethod
    def fromisoformat(cls, *args, **kwargs): # real signature unknown
        """ string -> time from time.isoformat() output """
        pass

    def isoformat(self, *args, **kwargs): # real signature unknown
        """
        Return string in ISO 8601 format, [HH[:MM[:SS[.mmm[uuu]]]]][+HH:MM].
        
        timespec specifies what components of the time to include.
        """
        pass

    def replace(self, *args, **kwargs): # real signature unknown
        """ Return time with new specified fields. """
        pass

    def strftime(self): # real signature unknown; restored from __doc__
        """ format -> strftime() style string. """
        pass

    def tzname(self): # real signature unknown; restored from __doc__
        """ Return self.tzinfo.tzname(self). """
        pass

    def utcoffset(self): # real signature unknown; restored from __doc__
        """ Return self.tzinfo.utcoffset(self). """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
        pass

    def __format__(self, *args, **kwargs): # real signature unknown
        """ Formats self with strftime. """
        pass

    def __getattribute__(self, *args, **kwargs): # real signature unknown
        """ Return getattr(self, name). """
        pass

    def __ge__(self, *args, **kwargs): # real signature unknown
        """ Return self>=value. """
        pass

    def __gt__(self, *args, **kwargs): # real signature unknown
        """ Return self>value. """
        pass

    def __hash__(self, *args, **kwargs): # real signature unknown
        """ Return hash(self). """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    def __le__(self, *args, **kwargs): # real signature unknown
        """ Return self<=value. """
        pass

    def __lt__(self, *args, **kwargs): # real signature unknown
        """ Return self<value. """
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __ne__(self, *args, **kwargs): # real signature unknown
        """ Return self!=value. """
        pass

    def __reduce_ex__(self, proto): # real signature unknown; restored from __doc__
        """ __reduce_ex__(proto) -> (cls, state) """
        pass

    def __reduce__(self): # real signature unknown; restored from __doc__
        """ __reduce__() -> (cls, state) """
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __str__(self, *args, **kwargs): # real signature unknown
        """ Return str(self). """
        pass

    fold = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    hour = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    microsecond = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    minute = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    second = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    tzinfo = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    max = datetime.time(23, 59, 59, 999999)
    min = datetime.time(0, 0)
    resolution = datetime.timedelta(microseconds=1)


class tzutc(__datetime.tzinfo):
    """
    This is a tzinfo object that represents the UTC time zone.
    
        **Examples:**
    
        .. doctest::
    
            >>> from datetime import *
            >>> from dateutil.tz import *
    
            >>> datetime.now()
            datetime.datetime(2003, 9, 27, 9, 40, 1, 521290)
    
            >>> datetime.now(tzutc())
            datetime.datetime(2003, 9, 27, 12, 40, 12, 156379, tzinfo=tzutc())
    
            >>> datetime.now(tzutc()).tzname()
            'UTC'
    
        .. versionchanged:: 2.7.0
            ``tzutc()`` is now a singleton, so the result of ``tzutc()`` will
            always return the same object.
    
            .. doctest::
    
                >>> from dateutil.tz import tzutc, UTC
                >>> tzutc() is tzutc()
                True
                >>> tzutc() is UTC
                True
    """
    def dst(self, dt): # reliably restored by inspect
        # no doc
        pass

    def fromutc(self, dt): # reliably restored by inspect
        """
        Fast track version of fromutc() returns the original ``dt`` object for
                any valid :py:class:`datetime.datetime` object.
        """
        pass

    def is_ambiguous(self, dt): # reliably restored by inspect
        """
        Whether or not the "wall time" of a given datetime is ambiguous in this
                zone.
        
                :param dt:
                    A :py:class:`datetime.datetime`, naive or time zone aware.
        
        
                :return:
                    Returns ``True`` if ambiguous, ``False`` otherwise.
        
                .. versionadded:: 2.6.0
        """
        pass

    def tzname(self, dt): # reliably restored by inspect
        # no doc
        pass

    def utcoffset(self, dt): # reliably restored by inspect
        # no doc
        pass

    def __eq__(self, other): # reliably restored by inspect
        # no doc
        pass

    def __init__(self): # real signature unknown; restored from __doc__
        pass

    def __ne__(self, other): # reliably restored by inspect
        # no doc
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        """ Helper for pickle. """
        pass

    def __repr__(self): # reliably restored by inspect
        # no doc
        pass

    __weakref__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """list of weak references to the object (if defined)"""


    _TzSingleton__instance = tzutc()
    __dict__ = None # (!) real value is 'mappingproxy({\'__module__\': \'dateutil.tz.tz\', \'__doc__\': "\\n    This is a tzinfo object that represents the UTC time zone.\\n\\n    **Examples:**\\n\\n    .. doctest::\\n\\n        >>> from datetime import *\\n        >>> from dateutil.tz import *\\n\\n        >>> datetime.now()\\n        datetime.datetime(2003, 9, 27, 9, 40, 1, 521290)\\n\\n        >>> datetime.now(tzutc())\\n        datetime.datetime(2003, 9, 27, 12, 40, 12, 156379, tzinfo=tzutc())\\n\\n        >>> datetime.now(tzutc()).tzname()\\n        \'UTC\'\\n\\n    .. versionchanged:: 2.7.0\\n        ``tzutc()`` is now a singleton, so the result of ``tzutc()`` will\\n        always return the same object.\\n\\n        .. doctest::\\n\\n            >>> from dateutil.tz import tzutc, UTC\\n            >>> tzutc() is tzutc()\\n            True\\n            >>> tzutc() is UTC\\n            True\\n    ", \'utcoffset\': <function tzutc.utcoffset at 0x09E3E8A0>, \'dst\': <function tzutc.dst at 0x09D563D8>, \'tzname\': <function tzutc.tzname at 0x09D56420>, \'is_ambiguous\': <function tzutc.is_ambiguous at 0x09D56468>, \'fromutc\': <function tzutc.fromutc at 0x09D564F8>, \'__eq__\': <function tzutc.__eq__ at 0x09E3EBB8>, \'__hash__\': None, \'__ne__\': <function tzutc.__ne__ at 0x09D56540>, \'__repr__\': <function tzutc.__repr__ at 0x09D56588>, \'__reduce__\': <method \'__reduce__\' of \'object\' objects>, \'__dict__\': <attribute \'__dict__\' of \'tzutc\' objects>, \'__weakref__\': <attribute \'__weakref__\' of \'tzutc\' objects>, \'_TzSingleton__instance\': tzutc()})'
    __hash__ = None


class _TSObject(object):
    # no doc
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    value = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default



# variables with complex values

nat_strings = None # (!) real value is "{'nan', 'NAN', 'nat', 'NaN', 'NAT', 'NaT'}"

NS_DTYPE = None # (!) real value is "dtype('<M8[ns]')"

TD_DTYPE = None # (!) real value is "dtype('<m8[ns]')"

UTC = pytz.UTC

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x09D6D8D0>'

__pyx_capi__ = {
    'convert_datetime_to_tsobject': None, # (!) real value is '<capsule object "struct __pyx_obj_6pandas_5_libs_6tslibs_10conversion__TSObject *(PyDateTime_DateTime *, PyObject *, struct __pyx_opt_args_6pandas_5_libs_6tslibs_10conversion_convert_datetime_to_tsobject *__pyx_optional_args)" at 0x09BB9E48>'
    'convert_to_tsobject': None, # (!) real value is '<capsule object "PyObject *(PyObject *, PyObject *, PyObject *, int, int, struct __pyx_opt_args_6pandas_5_libs_6tslibs_10conversion_convert_to_tsobject *__pyx_optional_args)" at 0x09BB9BA8>'
    'get_datetime64_nanos': None, # (!) real value is '<capsule object "__pyx_t_5numpy_int64_t (PyObject *)" at 0x09BB9E78>'
    'localize_pydatetime': None, # (!) real value is '<capsule object "PyDateTime_DateTime *(PyDateTime_DateTime *, PyObject *, int __pyx_skip_dispatch)" at 0x09BB9F50>'
    'maybe_datetimelike_to_i8': None, # (!) real value is '<capsule object "PyObject *(PyObject *)" at 0x09BB9F38>'
    'pydt_to_i8': None, # (!) real value is '<capsule object "__pyx_t_5numpy_int64_t (PyObject *, int __pyx_skip_dispatch)" at 0x09BB9E60>'
    'tz_convert_single': None, # (!) real value is '<capsule object "__pyx_t_5numpy_int64_t (__pyx_t_5numpy_int64_t, PyObject *, PyObject *, int __pyx_skip_dispatch)" at 0x09BB9E30>'
    'tz_convert_utc_to_tzlocal': None, # (!) real value is '<capsule object "__pyx_t_5numpy_int64_t (__pyx_t_5numpy_int64_t, PyDateTime_TZInfo *)" at 0x09BB9B18>'
}

__spec__ = None # (!) real value is "ModuleSpec(name='pandas._libs.tslibs.conversion', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x09D6D8D0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pandas\\\\_libs\\\\tslibs\\\\conversion.cp37-win32.pyd')"

__test__ = {}

