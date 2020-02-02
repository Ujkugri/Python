# encoding: utf-8
# module pandas._libs.index
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pandas\_libs\index.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import pandas._libs.algos as algos # C:\Program Files (x86)\Python37-32\lib\site-packages\pandas\_libs\algos.cp37-win32.pyd
import pandas._libs.hashtable as _hash # C:\Program Files (x86)\Python37-32\lib\site-packages\pandas\_libs\hashtable.cp37-win32.pyd
import pandas._libs.tslibs.period as periodlib # C:\Program Files (x86)\Python37-32\lib\site-packages\pandas\_libs\tslibs\period.cp37-win32.pyd
from pandas._libs.missing import checknull

from pandas._libs.tslibs.timedeltas import Timedelta

from pandas._libs.tslibs.timestamps import Timestamp

import datetime as __datetime


# Variables with simple values

_SIZE_CUTOFF = 1000000

# functions

def convert_scalar(*args, **kwargs): # real signature unknown
    pass

def get_value_at(*args, **kwargs): # real signature unknown
    pass

def get_value_box(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_BaseMultiIndexCodesEngine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_DatetimeEngine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Float32Engine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Float64Engine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_IndexEngine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int16Engine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int32Engine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int64Engine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Int8Engine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_ObjectEngine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_PeriodEngine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_TimedeltaEngine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_UInt16Engine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_UInt32Engine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_UInt64Engine(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_UInt8Engine(*args, **kwargs): # real signature unknown
    pass

# classes

class BaseMultiIndexCodesEngine(object):
    """
    Base class for MultiIndexUIntEngine and MultiIndexPyIntEngine, which
        represent each label in a MultiIndex as an integer, by juxtaposing the bits
        encoding each level, with appropriate offsets.
    
        For instance: if 3 levels have respectively 3, 6 and 1 possible values,
        then their labels can be represented using respectively 2, 3 and 1 bits,
        as follows:
         _ _ _ _____ _ __ __ __
        |0|0|0| ... |0| 0|a1|a0| -> offset 0 (first level)
         — — — ————— — —— —— ——
        |0|0|0| ... |0|b2|b1|b0| -> offset 2 (bits required for first level)
         — — — ————— — —— —— ——
        |0|0|0| ... |0| 0| 0|c0| -> offset 5 (bits required for first two levels)
         ‾ ‾ ‾ ‾‾‾‾‾ ‾ ‾‾ ‾‾ ‾‾
        and the resulting unsigned integer representation will be:
         _ _ _ _____ _ __ __ __ __ __ __
        |0|0|0| ... |0|c0|b2|b1|b0|a1|a0|
         ‾ ‾ ‾ ‾‾‾‾‾ ‾ ‾‾ ‾‾ ‾‾ ‾‾ ‾‾ ‾‾
    
        Offsets are calculated at initialization, labels are transformed by method
        _codes_to_ints.
    
        Keys are located by first locating each component against the respective
        level, then locating (the integer representation of) codes.
    """
    def get_indexer(self, *args, **kwargs): # real signature unknown
        pass

    def get_indexer_non_unique(self, *args, **kwargs): # real signature unknown
        pass

    def get_loc(self, *args, **kwargs): # real signature unknown
        pass

    def _extract_level_codes(self, *args, **kwargs): # real signature unknown
        """
        Map the requested list of (tuple) keys to their integer representations
                for searching in the underlying integer index.
        
                Parameters
                ----------
                target : list-like of keys
                    Each key is a tuple, with a label for each level of the index.
        
                Returns
                ------
                int_keys : 1-dimensional array of dtype uint64 or object
                    Integers representing one combination each
        """
        pass

    def __contains__(self, *args, **kwargs): # real signature unknown
        """ Return key in self. """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        """
        Parameters
                ----------
                levels : list-like of numpy arrays
                    Levels of the MultiIndex
                labels : list-like of numpy arrays of integer dtype
                    Labels of the MultiIndex
                offsets : numpy array of uint64 dtype
                    Pre-calculated offsets, one for each level of the index
        """
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass


class date(object):
    """ date(year, month, day) --> date object """
    def ctime(self): # real signature unknown; restored from __doc__
        """ Return ctime() style string. """
        pass

    @classmethod
    def fromisoformat(cls, *args, **kwargs): # real signature unknown
        """ str -> Construct a date from the output of date.isoformat() """
        pass

    @classmethod
    def fromordinal(cls, *args, **kwargs): # real signature unknown
        """ int -> date corresponding to a proleptic Gregorian ordinal. """
        pass

    @classmethod
    def fromtimestamp(cls, *args, **kwargs): # real signature unknown
        """ timestamp -> local date from a POSIX timestamp (like time.time()). """
        pass

    def isocalendar(self, *args, **kwargs): # real signature unknown
        """ Return a 3-tuple containing ISO year, week number, and weekday. """
        pass

    def isoformat(self, *args, **kwargs): # real signature unknown
        """ Return string in ISO 8601 format, YYYY-MM-DD. """
        pass

    def isoweekday(self, *args, **kwargs): # real signature unknown
        """
        Return the day of the week represented by the date.
        Monday == 1 ... Sunday == 7
        """
        pass

    def replace(self, *args, **kwargs): # real signature unknown
        """ Return date with new specified fields. """
        pass

    def strftime(self): # real signature unknown; restored from __doc__
        """ format -> strftime() style string. """
        pass

    def timetuple(self, *args, **kwargs): # real signature unknown
        """ Return time tuple, compatible with time.localtime(). """
        pass

    @classmethod
    def today(cls, *args, **kwargs): # real signature unknown
        """ Current date or datetime:  same as self.__class__.fromtimestamp(time.time()). """
        pass

    def toordinal(self, *args, **kwargs): # real signature unknown
        """ Return proleptic Gregorian ordinal.  January 1 of year 1 is day 1. """
        pass

    def weekday(self, *args, **kwargs): # real signature unknown
        """
        Return the day of the week represented by the date.
        Monday == 0 ... Sunday == 6
        """
        pass

    def __add__(self, *args, **kwargs): # real signature unknown
        """ Return self+value. """
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

    def __init__(self, year, month, day): # real signature unknown; restored from __doc__
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

    def __radd__(self, *args, **kwargs): # real signature unknown
        """ Return value+self. """
        pass

    def __reduce__(self): # real signature unknown; restored from __doc__
        """ __reduce__() -> (cls, state) """
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __rsub__(self, *args, **kwargs): # real signature unknown
        """ Return value-self. """
        pass

    def __str__(self, *args, **kwargs): # real signature unknown
        """ Return str(self). """
        pass

    def __sub__(self, *args, **kwargs): # real signature unknown
        """ Return self-value. """
        pass

    day = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    month = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    year = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    max = datetime.date(9999, 12, 31)
    min = datetime.date(1, 1, 1)
    resolution = datetime.timedelta(days=1)


class datetime(__datetime.date):
    """
    datetime(year, month, day[, hour[, minute[, second[, microsecond[,tzinfo]]]]])
    
    The year, month and day arguments are required. tzinfo may be None, or an
    instance of a tzinfo subclass. The remaining arguments may be ints.
    """
    def astimezone(self, *args, **kwargs): # real signature unknown
        """ tz -> convert to local time in new timezone tz """
        pass

    @classmethod
    def combine(cls, *args, **kwargs): # real signature unknown
        """ date, time -> datetime with same date and time fields """
        pass

    def ctime(self): # real signature unknown; restored from __doc__
        """ Return ctime() style string. """
        pass

    def date(self, *args, **kwargs): # real signature unknown
        """ Return date object with same year, month and day. """
        pass

    def dst(self): # real signature unknown; restored from __doc__
        """ Return self.tzinfo.dst(self). """
        pass

    @classmethod
    def fromisoformat(cls, *args, **kwargs): # real signature unknown
        """ string -> datetime from datetime.isoformat() output """
        pass

    @classmethod
    def fromtimestamp(cls, *args, **kwargs): # real signature unknown
        """ timestamp[, tz] -> tz's local time from POSIX timestamp. """
        pass

    def isoformat(self, *args, **kwargs): # real signature unknown
        """
        [sep] -> string in ISO 8601 format, YYYY-MM-DDT[HH[:MM[:SS[.mmm[uuu]]]]][+HH:MM].
        sep is used to separate the year from the time, and defaults to 'T'.
        timespec specifies what components of the time to include (allowed values are 'auto', 'hours', 'minutes', 'seconds', 'milliseconds', and 'microseconds').
        """
        pass

    @classmethod
    def now(cls, *args, **kwargs): # real signature unknown
        """
        Returns new datetime object representing current time local to tz.
        
          tz
            Timezone object.
        
        If no tz is specified, uses local timezone.
        """
        pass

    def replace(self, *args, **kwargs): # real signature unknown
        """ Return datetime with new specified fields. """
        pass

    @classmethod
    def strptime(cls): # real signature unknown; restored from __doc__
        """ string, format -> new datetime parsed from a string (like time.strptime()). """
        pass

    def time(self, *args, **kwargs): # real signature unknown
        """ Return time object with same time but with tzinfo=None. """
        pass

    def timestamp(self, *args, **kwargs): # real signature unknown
        """ Return POSIX timestamp as float. """
        pass

    def timetuple(self, *args, **kwargs): # real signature unknown
        """ Return time tuple, compatible with time.localtime(). """
        pass

    def timetz(self, *args, **kwargs): # real signature unknown
        """ Return time object with same time and tzinfo. """
        pass

    def tzname(self): # real signature unknown; restored from __doc__
        """ Return self.tzinfo.tzname(self). """
        pass

    @classmethod
    def utcfromtimestamp(cls, *args, **kwargs): # real signature unknown
        """ Construct a naive UTC datetime from a POSIX timestamp. """
        pass

    @classmethod
    def utcnow(cls, *args, **kwargs): # real signature unknown
        """ Return a new datetime representing UTC day and time. """
        pass

    def utcoffset(self): # real signature unknown; restored from __doc__
        """ Return self.tzinfo.utcoffset(self). """
        pass

    def utctimetuple(self, *args, **kwargs): # real signature unknown
        """ Return UTC time tuple, compatible with time.localtime(). """
        pass

    def __add__(self, *args, **kwargs): # real signature unknown
        """ Return self+value. """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
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

    def __init__(self, year, month, day, hour=None, minute=None, second=None, microsecond=None, tzinfo=None): # real signature unknown; restored from __doc__
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

    def __radd__(self, *args, **kwargs): # real signature unknown
        """ Return value+self. """
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

    def __rsub__(self, *args, **kwargs): # real signature unknown
        """ Return value-self. """
        pass

    def __str__(self, *args, **kwargs): # real signature unknown
        """ Return str(self). """
        pass

    def __sub__(self, *args, **kwargs): # real signature unknown
        """ Return self-value. """
        pass

    fold = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    hour = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    microsecond = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    minute = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    second = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    tzinfo = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    max = datetime.datetime(9999, 12, 31, 23, 59, 59, 999999)
    min = datetime.datetime(1, 1, 1, 0, 0)
    resolution = datetime.timedelta(microseconds=1)


class IndexEngine(object):
    # no doc
    def clear_mapping(self, *args, **kwargs): # real signature unknown
        pass

    def get_backfill_indexer(self, *args, **kwargs): # real signature unknown
        pass

    def get_indexer(self, *args, **kwargs): # real signature unknown
        pass

    def get_indexer_non_unique(self, *args, **kwargs): # real signature unknown
        """
        return an indexer suitable for takng from a non unique index
                    return the labels in the same order ast the target
                    and a missing indexer into the targets (which correspond
                    to the -1 indices in the results
        """
        pass

    def get_loc(self, *args, **kwargs): # real signature unknown
        pass

    def get_pad_indexer(self, *args, **kwargs): # real signature unknown
        pass

    def get_value(self, *args, **kwargs): # real signature unknown
        """ arr : 1-dimensional ndarray """
        pass

    def set_value(self, *args, **kwargs): # real signature unknown
        """ arr : 1-dimensional ndarray """
        pass

    def sizeof(self, *args, **kwargs): # real signature unknown
        """ return the sizeof our mapping """
        pass

    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

    def _call_monotonic(self, *args, **kwargs): # real signature unknown
        pass

    def __contains__(self, *args, **kwargs): # real signature unknown
        """ Return key in self. """
        pass

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

    def __sizeof__(self, *args, **kwargs): # real signature unknown
        pass

    is_mapping_populated = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    is_monotonic_decreasing = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    is_monotonic_increasing = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    is_unique = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    mapping = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    over_size_threshold = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    vgetter = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BA4E78>'


class Int64Engine(IndexEngine):
    # no doc
    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BA4E48>'


class DatetimeEngine(Int64Engine):
    # no doc
    def get_backfill_indexer(self, *args, **kwargs): # real signature unknown
        pass

    def get_indexer(self, *args, **kwargs): # real signature unknown
        pass

    def get_loc(self, *args, **kwargs): # real signature unknown
        pass

    def get_pad_indexer(self, *args, **kwargs): # real signature unknown
        pass

    def _call_monotonic(self, *args, **kwargs): # real signature unknown
        pass

    def __contains__(self, *args, **kwargs): # real signature unknown
        """ Return key in self. """
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BA4F20>'


class Float32Engine(IndexEngine):
    # no doc
    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BCE020>'


class Float64Engine(IndexEngine):
    # no doc
    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BA4FC8>'


class Int16Engine(IndexEngine):
    # no doc
    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BCE080>'


class Int32Engine(IndexEngine):
    # no doc
    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BCE050>'


class Int8Engine(IndexEngine):
    # no doc
    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BCE0B0>'


class ObjectEngine(IndexEngine):
    """ Index Engine for use with object-dtype Index, namely the base class Index """
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BA4EC0>'


class PeriodEngine(Int64Engine):
    # no doc
    def get_backfill_indexer(self, *args, **kwargs): # real signature unknown
        pass

    def get_indexer(self, *args, **kwargs): # real signature unknown
        pass

    def get_indexer_non_unique(self, *args, **kwargs): # real signature unknown
        pass

    def get_pad_indexer(self, *args, **kwargs): # real signature unknown
        pass

    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

    def _call_monotonic(self, *args, **kwargs): # real signature unknown
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BA4F80>'


class timedelta(object):
    """
    Difference between two datetime values.
    
    timedelta(days=0, seconds=0, microseconds=0, milliseconds=0, minutes=0, hours=0, weeks=0)
    
    All arguments are optional and default to 0.
    Arguments may be integers or floats, and may be positive or negative.
    """
    def total_seconds(self, *args, **kwargs): # real signature unknown
        """ Total seconds in the duration. """
        pass

    def __abs__(self, *args, **kwargs): # real signature unknown
        """ abs(self) """
        pass

    def __add__(self, *args, **kwargs): # real signature unknown
        """ Return self+value. """
        pass

    def __bool__(self, *args, **kwargs): # real signature unknown
        """ self != 0 """
        pass

    def __divmod__(self, *args, **kwargs): # real signature unknown
        """ Return divmod(self, value). """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
        pass

    def __floordiv__(self, *args, **kwargs): # real signature unknown
        """ Return self//value. """
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

    def __init__(self, days=0, seconds=0, microseconds=0, milliseconds=0, minutes=0, hours=0, weeks=0): # real signature unknown; restored from __doc__
        pass

    def __le__(self, *args, **kwargs): # real signature unknown
        """ Return self<=value. """
        pass

    def __lt__(self, *args, **kwargs): # real signature unknown
        """ Return self<value. """
        pass

    def __mod__(self, *args, **kwargs): # real signature unknown
        """ Return self%value. """
        pass

    def __mul__(self, *args, **kwargs): # real signature unknown
        """ Return self*value. """
        pass

    def __neg__(self, *args, **kwargs): # real signature unknown
        """ -self """
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __ne__(self, *args, **kwargs): # real signature unknown
        """ Return self!=value. """
        pass

    def __pos__(self, *args, **kwargs): # real signature unknown
        """ +self """
        pass

    def __radd__(self, *args, **kwargs): # real signature unknown
        """ Return value+self. """
        pass

    def __rdivmod__(self, *args, **kwargs): # real signature unknown
        """ Return divmod(value, self). """
        pass

    def __reduce__(self): # real signature unknown; restored from __doc__
        """ __reduce__() -> (cls, state) """
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __rfloordiv__(self, *args, **kwargs): # real signature unknown
        """ Return value//self. """
        pass

    def __rmod__(self, *args, **kwargs): # real signature unknown
        """ Return value%self. """
        pass

    def __rmul__(self, *args, **kwargs): # real signature unknown
        """ Return value*self. """
        pass

    def __rsub__(self, *args, **kwargs): # real signature unknown
        """ Return value-self. """
        pass

    def __rtruediv__(self, *args, **kwargs): # real signature unknown
        """ Return value/self. """
        pass

    def __str__(self, *args, **kwargs): # real signature unknown
        """ Return str(self). """
        pass

    def __sub__(self, *args, **kwargs): # real signature unknown
        """ Return self-value. """
        pass

    def __truediv__(self, *args, **kwargs): # real signature unknown
        """ Return self/value. """
        pass

    days = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """Number of days."""

    microseconds = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """Number of microseconds (>= 0 and less than 1 second)."""

    seconds = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """Number of seconds (>= 0 and less than 1 day)."""


    max = datetime.timedelta(days=999999999, seconds=86399, microseconds=999999)
    min = datetime.timedelta(days=-999999999)
    resolution = datetime.timedelta(microseconds=1)


class TimedeltaEngine(DatetimeEngine):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BA4F50>'


class UInt16Engine(IndexEngine):
    # no doc
    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BCE140>'


class UInt32Engine(IndexEngine):
    # no doc
    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BCE110>'


class UInt64Engine(IndexEngine):
    # no doc
    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BCE0E0>'


class UInt8Engine(IndexEngine):
    # no doc
    def _call_map_locations(self, *args, **kwargs): # real signature unknown
        pass

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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09BCE170>'


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x09BC78B0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pandas._libs.index', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x09BC78B0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pandas\\\\_libs\\\\index.cp37-win32.pyd')"

__test__ = {}

