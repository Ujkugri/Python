# encoding: utf-8
# module pandas._libs.lib
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pandas\_libs\lib.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import sys as sys # <module 'sys' (built-in)>
import warnings as warnings # C:\Program Files (x86)\Python37-32\lib\warnings.py
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
from pandas._libs.tslib import array_to_datetime

from pandas._libs.tslibs.nattype import NaT

import numbers as __numbers


# functions

def array_equivalent_object(*args, **kwargs): # real signature unknown
    """
    perform an element by element comparion on 1-d object arrays
            taking into account nan positions
    """
    pass

def astype_intsafe(*args, **kwargs): # real signature unknown
    pass

def astype_str(*args, **kwargs): # real signature unknown
    """
    Convert all elements in an array to string.
    
        Parameters
        ----------
        arr : ndarray
            The array whose elements we are casting.
        skipna : bool, default False
            Whether or not to coerce nulls to their stringified form
            (e.g. NaN becomes 'nan').
    
        Returns
        -------
        casted_arr : ndarray
            A new array with the input array's elements casted.
    """
    pass

def astype_unicode(*args, **kwargs): # real signature unknown
    """
    Convert all elements in an array to unicode.
    
        Parameters
        ----------
        arr : ndarray
            The array whose elements we are casting.
        skipna : bool, default False
            Whether or not to coerce nulls to their stringified form
            (e.g. NaN becomes 'nan').
    
        Returns
        -------
        casted_arr : ndarray
            A new array with the input array's elements casted.
    """
    pass

def clean_index_list(*args, **kwargs): # real signature unknown
    """ Utility used in pandas.core.index.ensure_index """
    pass

def count_level_2d(*args, **kwargs): # real signature unknown
    pass

def dicts_to_array(*args, **kwargs): # real signature unknown
    pass

def fast_multiget(*args, **kwargs): # real signature unknown
    pass

def fast_unique_multiple(*args, **kwargs): # real signature unknown
    """
    Generate a list of unique values from a list of arrays.
    
        Parameters
        ----------
        list : array-like
            A list of array-like objects
        sort : boolean
            Whether or not to sort the resulting unique list
    
        Returns
        -------
        unique_list : list of unique values
    """
    pass

def fast_unique_multiple_list(*args, **kwargs): # real signature unknown
    pass

def fast_unique_multiple_list_gen(*args, **kwargs): # real signature unknown
    """
    Generate a list of unique values from a generator of lists.
    
        Parameters
        ----------
        gen : generator object
            A generator of lists from which the unique list is created
        sort : boolean
            Whether or not to sort the resulting unique list
    
        Returns
        -------
        unique_list : list of unique values
    """
    pass

def fast_zip(*args, **kwargs): # real signature unknown
    """ For zipping multiple ndarrays into an ndarray of tuples """
    pass

def generate_bins_dt64(*args, **kwargs): # real signature unknown
    """ Int64 (datetime64) version of generic python version in groupby.py """
    pass

def generate_slices(*args, **kwargs): # real signature unknown
    pass

def get_level_sorter(*args, **kwargs): # real signature unknown
    """
    argsort for a single level of a multi-index, keeping the order of higher
        levels unchanged. `starts` points to starts of same-key indices w.r.t
        to leading levels; equivalent to:
            np.hstack([label[starts[i]:starts[i+1]].argsort(kind='mergesort')
                + starts[i] for i in range(len(starts) - 1)])
    """
    pass

def get_reverse_indexer(*args, **kwargs): # real signature unknown
    """
    Reverse indexing operation.
    
        Given `indexer`, make `indexer_inv` of it, such that::
    
            indexer_inv[indexer[x]] = x
    
        .. note:: If indexer is not unique, only first occurrence is accounted.
    """
    pass

def has_infs_f4(*args, **kwargs): # real signature unknown
    pass

def has_infs_f8(*args, **kwargs): # real signature unknown
    pass

def indices_fast(*args, **kwargs): # real signature unknown
    pass

def infer_datetimelike_array(*args, **kwargs): # real signature unknown
    """
    infer if we have a datetime or timedelta array
        - date: we have *only* date and maybe strings, nulls
        - datetime: we have *only* datetimes and maybe strings, nulls
        - timedelta: we have *only* timedeltas and maybe strings, nulls
        - nat: we do not have *any* date, datetimes or timedeltas, but do have
          at least a NaT
        - mixed: other objects (strings, a mix of tz-aware and tz-naive, or
                                actual objects)
    
        Parameters
        ----------
        arr : object array
    
        Returns
        -------
        string: {datetime, timedelta, date, nat, mixed}
    """
    pass

def infer_dtype(foo=None, bar=None): # real signature unknown; restored from __doc__
    """
    Efficiently infer the type of a passed val, or list-like
        array of values. Return a string describing the type.
    
        Parameters
        ----------
        value : scalar, list, ndarray, or pandas type
        skipna : bool, default False
            Ignore NaN values when inferring the type.
    
            .. versionadded:: 0.21.0
    
        Returns
        -------
        string describing the common type of the input data.
        Results can include:
    
        - string
        - unicode
        - bytes
        - floating
        - integer
        - mixed-integer
        - mixed-integer-float
        - decimal
        - complex
        - categorical
        - boolean
        - datetime64
        - datetime
        - date
        - timedelta64
        - timedelta
        - time
        - period
        - mixed
    
        Raises
        ------
        TypeError if ndarray-like but cannot infer the dtype
    
        Notes
        -----
        - 'mixed' is the catchall for anything that is not otherwise
          specialized
        - 'mixed-integer-float' are floats and integers
        - 'mixed-integer' are integers mixed with non-integers
    
        Examples
        --------
        >>> infer_dtype(['foo', 'bar'])
        'string'
    
        >>> infer_dtype(['a', np.nan, 'b'], skipna=True)
        'string'
    
        >>> infer_dtype(['a', np.nan, 'b'], skipna=False)
        'mixed'
    
        >>> infer_dtype([b'foo', b'bar'])
        'bytes'
    
        >>> infer_dtype([1, 2, 3])
        'integer'
    
        >>> infer_dtype([1, 2, 3.5])
        'mixed-integer-float'
    
        >>> infer_dtype([1.0, 2.0, 3.5])
        'floating'
    
        >>> infer_dtype(['a', 1])
        'mixed-integer'
    
        >>> infer_dtype([Decimal(1), Decimal(2.0)])
        'decimal'
    
        >>> infer_dtype([True, False])
        'boolean'
    
        >>> infer_dtype([True, False, np.nan])
        'mixed'
    
        >>> infer_dtype([pd.Timestamp('20130101')])
        'datetime'
    
        >>> infer_dtype([datetime.date(2013, 1, 1)])
        'date'
    
        >>> infer_dtype([np.datetime64('2013-01-01')])
        'datetime64'
    
        >>> infer_dtype([datetime.timedelta(0, 1, 1)])
        'timedelta'
    
        >>> infer_dtype(pd.Series(list('aabc')).astype('category'))
        'categorical'
    """
    pass

def is_bool(*args, **kwargs): # real signature unknown
    pass

def is_bool_array(*args, **kwargs): # real signature unknown
    pass

def is_complex(*args, **kwargs): # real signature unknown
    pass

def is_datetime64_array(*args, **kwargs): # real signature unknown
    pass

def is_datetime_array(*args, **kwargs): # real signature unknown
    pass

def is_datetime_with_singletz_array(*args, **kwargs): # real signature unknown
    """
    Check values have the same tzinfo attribute.
        Doesn't check values are datetime-like types.
    """
    pass

def is_date_array(*args, **kwargs): # real signature unknown
    pass

def is_decimal(*args, **kwargs): # real signature unknown
    pass

def is_float(*args, **kwargs): # real signature unknown
    pass

def is_float_array(*args, **kwargs): # real signature unknown
    pass

def is_integer(*args, **kwargs): # real signature unknown
    pass

def is_integer_array(*args, **kwargs): # real signature unknown
    pass

def is_interval(*args, **kwargs): # real signature unknown
    pass

def is_interval_array(*args, **kwargs): # real signature unknown
    pass

def is_period(*args, **kwargs): # real signature unknown
    """ Return a boolean if this is a Period object """
    pass

def is_period_array(*args, **kwargs): # real signature unknown
    pass

def is_scalar(dt): # real signature unknown; restored from __doc__
    """
    Return True if given value is scalar.
    
        Parameters
        ----------
        val : object
            This includes:
    
            - numpy array scalar (e.g. np.int64)
            - Python builtin numerics
            - Python builtin byte arrays and strings
            - None
            - datetime.datetime
            - datetime.timedelta
            - Period
            - decimal.Decimal
            - Interval
            - DateOffset
            - Fraction
            - Number
    
        Returns
        -------
        bool
            Return True if given object is scalar, False otherwise
    
        Examples
        --------
        >>> dt = pd.datetime.datetime(2018, 10, 3)
        >>> pd.is_scalar(dt)
        True
    
        >>> pd.api.types.is_scalar([2, 3])
        False
    
        >>> pd.api.types.is_scalar({0: 1, 2: 3})
        False
    
        >>> pd.api.types.is_scalar((0, 2))
        False
    
        pandas supports PEP 3141 numbers:
    
        >>> from fractions import Fraction
        >>> pd.api.types.is_scalar(Fraction(3, 5))
        True
    """
    pass

def is_string_array(*args, **kwargs): # real signature unknown
    pass

def is_timedelta_or_timedelta64_array(*args, **kwargs): # real signature unknown
    """ infer with timedeltas and/or nat/none """
    pass

def is_time_array(*args, **kwargs): # real signature unknown
    pass

def item_from_zerodim(*args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
    """
    If the value is a zerodim array, return the item it contains.
    
        Parameters
        ----------
        val : object
    
        Returns
        -------
        result : object
    
        Examples
        --------
        >>> item_from_zerodim(1)
        1
        >>> item_from_zerodim('foobar')
        'foobar'
        >>> item_from_zerodim(np.array(1))
        1
        >>> item_from_zerodim(np.array([1]))
        array([1])
    """
    pass

def map_infer(*args, **kwargs): # real signature unknown
    """
    Substitute for np.vectorize with pandas-friendly dtype inference
    
        Parameters
        ----------
        arr : ndarray
        f : function
    
        Returns
        -------
        mapped : ndarray
    """
    pass

def map_infer_mask(*args, **kwargs): # real signature unknown
    """
    Substitute for np.vectorize with pandas-friendly dtype inference
    
        Parameters
        ----------
        arr : ndarray
        f : function
    
        Returns
        -------
        mapped : ndarray
    """
    pass

def maybe_booleans_to_slice(*args, **kwargs): # real signature unknown
    pass

def maybe_convert_numeric(*args, **kwargs): # real signature unknown
    """
    Convert object array to a numeric array if possible.
    
        Parameters
        ----------
        values : ndarray
            Array of object elements to convert.
        na_values : set
            Set of values that should be interpreted as NaN.
        convert_empty : bool, default True
            If an empty array-like object is encountered, whether to interpret
            that element as NaN or not. If set to False, a ValueError will be
            raised if such an element is encountered and 'coerce_numeric' is False.
        coerce_numeric : bool, default False
            If initial attempts to convert to numeric have failed, whether to
            force conversion to numeric via alternative methods or by setting the
            element to NaN. Otherwise, an Exception will be raised when such an
            element is encountered.
    
            This boolean also has an impact on how conversion behaves when a
            numeric array has no suitable numerical dtype to return (i.e. uint64,
            int32, uint8). If set to False, the original object array will be
            returned. Otherwise, a ValueError will be raised.
    
        Returns
        -------
        numeric_array : array of converted object values to numerical ones
    """
    pass

def maybe_convert_objects(*args, **kwargs): # real signature unknown
    """ Type inference function-- convert object array to proper dtype """
    pass

def maybe_indices_to_slice(*args, **kwargs): # real signature unknown
    pass

def memory_usage_of_objects(*args, **kwargs): # real signature unknown
    """
    return the memory usage of an object array in bytes,
        does not include the actual bytes of the pointers
    """
    pass

def row_bool_subset(*args, **kwargs): # real signature unknown
    pass

def row_bool_subset_object(*args, **kwargs): # real signature unknown
    pass

def to_object_array(*args, **kwargs): # real signature unknown
    """
    Convert a list of lists into an object array.
    
        Parameters
        ----------
        rows : 2-d array (N, K)
            A list of lists to be converted into an array
        min_width : int
            The minimum width of the object array. If a list
            in `rows` contains fewer than `width` elements,
            the remaining elements in the corresponding row
            will all be `NaN`.
    
        Returns
        -------
        obj_array : numpy array of the object dtype
    """
    pass

def to_object_array_tuples(*args, **kwargs): # real signature unknown
    pass

def tuples_to_object_array(*args, **kwargs): # real signature unknown
    pass

def values_from_object(*args, **kwargs): # real signature unknown
    """ return my values or the object if we are say an ndarray """
    pass

# classes

class Validator(object):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E7590>'


class TemporalValidator(Validator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E76F8>'


class TimedeltaValidator(TemporalValidator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E7788>'


class AnyTimedeltaValidator(TimedeltaValidator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E77B8>'


class BoolValidator(Validator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E7620>'


class BytesValidator(Validator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E76C8>'


class DatetimeValidator(TemporalValidator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E7728>'


class Datetime64Validator(DatetimeValidator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E7758>'


class DateValidator(Validator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E77E8>'


class Decimal(object):
    """
    Construct a new Decimal object. 'value' can be an integer, string, tuple,
    or another Decimal object. If no value is given, return Decimal('0'). The
    context does not affect the conversion and is only passed to determine if
    the InvalidOperation trap is active.
    """
    def adjusted(self, *args, **kwargs): # real signature unknown
        """ Return the adjusted exponent of the number.  Defined as exp + digits - 1. """
        pass

    def as_integer_ratio(self): # real signature unknown; restored from __doc__
        """
        Decimal.as_integer_ratio() -> (int, int)
        
        Return a pair of integers, whose ratio is exactly equal to the original
        Decimal and with a positive denominator. The ratio is in lowest terms.
        Raise OverflowError on infinities and a ValueError on NaNs.
        """
        pass

    def as_tuple(self, *args, **kwargs): # real signature unknown
        """ Return a tuple representation of the number. """
        pass

    def canonical(self, *args, **kwargs): # real signature unknown
        """
        Return the canonical encoding of the argument.  Currently, the encoding
        of a Decimal instance is always canonical, so this operation returns its
        argument unchanged.
        """
        pass

    def compare(self, *args, **kwargs): # real signature unknown
        """
        Compare self to other.  Return a decimal value:
        
            a or b is a NaN ==> Decimal('NaN')
            a < b           ==> Decimal('-1')
            a == b          ==> Decimal('0')
            a > b           ==> Decimal('1')
        """
        pass

    def compare_signal(self, *args, **kwargs): # real signature unknown
        """ Identical to compare, except that all NaNs signal. """
        pass

    def compare_total(self, Decimal, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Compare two operands using their abstract representation rather than
        their numerical value.  Similar to the compare() method, but the result
        gives a total ordering on Decimal instances.  Two Decimal instances with
        the same numeric value but different representations compare unequal
        in this ordering:
        
            >>> Decimal('12.0').compare_total(Decimal('12'))
            Decimal('-1')
        
        Quiet and signaling NaNs are also included in the total ordering. The result
        of this function is Decimal('0') if both operands have the same representation,
        Decimal('-1') if the first operand is lower in the total order than the second,
        and Decimal('1') if the first operand is higher in the total order than the
        second operand. See the specification for details of the total order.
        
        This operation is unaffected by context and is quiet: no flags are changed
        and no rounding is performed. As an exception, the C version may raise
        InvalidOperation if the second operand cannot be converted exactly.
        """
        pass

    def compare_total_mag(self, y): # real signature unknown; restored from __doc__
        """
        Compare two operands using their abstract representation rather than their
        value as in compare_total(), but ignoring the sign of each operand.
        
        x.compare_total_mag(y) is equivalent to x.copy_abs().compare_total(y.copy_abs()).
        
        This operation is unaffected by context and is quiet: no flags are changed
        and no rounding is performed. As an exception, the C version may raise
        InvalidOperation if the second operand cannot be converted exactly.
        """
        pass

    def conjugate(self, *args, **kwargs): # real signature unknown
        """ Return self. """
        pass

    def copy_abs(self, *args, **kwargs): # real signature unknown
        """
        Return the absolute value of the argument.  This operation is unaffected by
        context and is quiet: no flags are changed and no rounding is performed.
        """
        pass

    def copy_negate(self, *args, **kwargs): # real signature unknown
        """
        Return the negation of the argument.  This operation is unaffected by context
        and is quiet: no flags are changed and no rounding is performed.
        """
        pass

    def copy_sign(self, Decimal, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Return a copy of the first operand with the sign set to be the same as the
        sign of the second operand. For example:
        
            >>> Decimal('2.3').copy_sign(Decimal('-1.5'))
            Decimal('-2.3')
        
        This operation is unaffected by context and is quiet: no flags are changed
        and no rounding is performed. As an exception, the C version may raise
        InvalidOperation if the second operand cannot be converted exactly.
        """
        pass

    def exp(self, *args, **kwargs): # real signature unknown
        """
        Return the value of the (natural) exponential function e**x at the given
        number.  The function always uses the ROUND_HALF_EVEN mode and the result
        is correctly rounded.
        """
        pass

    def fma(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Fused multiply-add.  Return self*other+third with no rounding of the
        intermediate product self*other.
        
            >>> Decimal(2).fma(3, 5)
            Decimal('11')
        """
        pass

    @classmethod
    def from_float(cls, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Class method that converts a float to a decimal number, exactly.
        Since 0.1 is not exactly representable in binary floating point,
        Decimal.from_float(0.1) is not the same as Decimal('0.1').
        
            >>> Decimal.from_float(0.1)
            Decimal('0.1000000000000000055511151231257827021181583404541015625')
            >>> Decimal.from_float(float('nan'))
            Decimal('NaN')
            >>> Decimal.from_float(float('inf'))
            Decimal('Infinity')
            >>> Decimal.from_float(float('-inf'))
            Decimal('-Infinity')
        """
        pass

    def is_canonical(self, *args, **kwargs): # real signature unknown
        """
        Return True if the argument is canonical and False otherwise.  Currently,
        a Decimal instance is always canonical, so this operation always returns
        True.
        """
        pass

    def is_finite(self, *args, **kwargs): # real signature unknown
        """
        Return True if the argument is a finite number, and False if the argument
        is infinite or a NaN.
        """
        pass

    def is_infinite(self, *args, **kwargs): # real signature unknown
        """
        Return True if the argument is either positive or negative infinity and
        False otherwise.
        """
        pass

    def is_nan(self, *args, **kwargs): # real signature unknown
        """
        Return True if the argument is a (quiet or signaling) NaN and False
        otherwise.
        """
        pass

    def is_normal(self, *args, **kwargs): # real signature unknown
        """
        Return True if the argument is a normal finite non-zero number with an
        adjusted exponent greater than or equal to Emin. Return False if the
        argument is zero, subnormal, infinite or a NaN.
        """
        pass

    def is_qnan(self, *args, **kwargs): # real signature unknown
        """ Return True if the argument is a quiet NaN, and False otherwise. """
        pass

    def is_signed(self, *args, **kwargs): # real signature unknown
        """
        Return True if the argument has a negative sign and False otherwise.
        Note that both zeros and NaNs can carry signs.
        """
        pass

    def is_snan(self, *args, **kwargs): # real signature unknown
        """ Return True if the argument is a signaling NaN and False otherwise. """
        pass

    def is_subnormal(self, *args, **kwargs): # real signature unknown
        """
        Return True if the argument is subnormal, and False otherwise. A number is
        subnormal if it is non-zero, finite, and has an adjusted exponent less
        than Emin.
        """
        pass

    def is_zero(self, *args, **kwargs): # real signature unknown
        """
        Return True if the argument is a (positive or negative) zero and False
        otherwise.
        """
        pass

    def ln(self, *args, **kwargs): # real signature unknown
        """
        Return the natural (base e) logarithm of the operand. The function always
        uses the ROUND_HALF_EVEN mode and the result is correctly rounded.
        """
        pass

    def log10(self, *args, **kwargs): # real signature unknown
        """
        Return the base ten logarithm of the operand. The function always uses the
        ROUND_HALF_EVEN mode and the result is correctly rounded.
        """
        pass

    def logb(self, *args, **kwargs): # real signature unknown
        """
        For a non-zero number, return the adjusted exponent of the operand as a
        Decimal instance.  If the operand is a zero, then Decimal('-Infinity') is
        returned and the DivisionByZero condition is raised. If the operand is
        an infinity then Decimal('Infinity') is returned.
        """
        pass

    def logical_and(self, *args, **kwargs): # real signature unknown
        """ Return the digit-wise 'and' of the two (logical) operands. """
        pass

    def logical_invert(self, *args, **kwargs): # real signature unknown
        """ Return the digit-wise inversion of the (logical) operand. """
        pass

    def logical_or(self, *args, **kwargs): # real signature unknown
        """ Return the digit-wise 'or' of the two (logical) operands. """
        pass

    def logical_xor(self, *args, **kwargs): # real signature unknown
        """ Return the digit-wise 'exclusive or' of the two (logical) operands. """
        pass

    def max(self, *args, **kwargs): # real signature unknown
        """
        Maximum of self and other.  If one operand is a quiet NaN and the other is
        numeric, the numeric operand is returned.
        """
        pass

    def max_mag(self, *args, **kwargs): # real signature unknown
        """
        Similar to the max() method, but the comparison is done using the absolute
        values of the operands.
        """
        pass

    def min(self, *args, **kwargs): # real signature unknown
        """
        Minimum of self and other. If one operand is a quiet NaN and the other is
        numeric, the numeric operand is returned.
        """
        pass

    def min_mag(self, *args, **kwargs): # real signature unknown
        """
        Similar to the min() method, but the comparison is done using the absolute
        values of the operands.
        """
        pass

    def next_minus(self, *args, **kwargs): # real signature unknown
        """
        Return the largest number representable in the given context (or in the
        current default context if no context is given) that is smaller than the
        given operand.
        """
        pass

    def next_plus(self, *args, **kwargs): # real signature unknown
        """
        Return the smallest number representable in the given context (or in the
        current default context if no context is given) that is larger than the
        given operand.
        """
        pass

    def next_toward(self, *args, **kwargs): # real signature unknown
        """
        If the two operands are unequal, return the number closest to the first
        operand in the direction of the second operand.  If both operands are
        numerically equal, return a copy of the first operand with the sign set
        to be the same as the sign of the second operand.
        """
        pass

    def normalize(self, *args, **kwargs): # real signature unknown
        """
        Normalize the number by stripping the rightmost trailing zeros and
        converting any result equal to Decimal('0') to Decimal('0e0').  Used
        for producing canonical values for members of an equivalence class.
        For example, Decimal('32.100') and Decimal('0.321000e+2') both normalize
        to the equivalent value Decimal('32.1').
        """
        pass

    def number_class(self, *args, **kwargs): # real signature unknown
        """
        Return a string describing the class of the operand.  The returned value
        is one of the following ten strings:
        
            * '-Infinity', indicating that the operand is negative infinity.
            * '-Normal', indicating that the operand is a negative normal number.
            * '-Subnormal', indicating that the operand is negative and subnormal.
            * '-Zero', indicating that the operand is a negative zero.
            * '+Zero', indicating that the operand is a positive zero.
            * '+Subnormal', indicating that the operand is positive and subnormal.
            * '+Normal', indicating that the operand is a positive normal number.
            * '+Infinity', indicating that the operand is positive infinity.
            * 'NaN', indicating that the operand is a quiet NaN (Not a Number).
            * 'sNaN', indicating that the operand is a signaling NaN.
        """
        pass

    def quantize(self, Decimal, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Return a value equal to the first operand after rounding and having the
        exponent of the second operand.
        
            >>> Decimal('1.41421356').quantize(Decimal('1.000'))
            Decimal('1.414')
        
        Unlike other operations, if the length of the coefficient after the quantize
        operation would be greater than precision, then an InvalidOperation is signaled.
        This guarantees that, unless there is an error condition, the quantized exponent
        is always equal to that of the right-hand operand.
        
        Also unlike other operations, quantize never signals Underflow, even if the
        result is subnormal and inexact.
        
        If the exponent of the second operand is larger than that of the first, then
        rounding may be necessary. In this case, the rounding mode is determined by the
        rounding argument if given, else by the given context argument; if neither
        argument is given, the rounding mode of the current thread's context is used.
        """
        pass

    def radix(self, base): # real signature unknown; restored from __doc__
        """
        Return Decimal(10), the radix (base) in which the Decimal class does
        all its arithmetic. Included for compatibility with the specification.
        """
        pass

    def remainder_near(self, *args, **kwargs): # real signature unknown
        """
        Return the remainder from dividing self by other.  This differs from
        self % other in that the sign of the remainder is chosen so as to minimize
        its absolute value. More precisely, the return value is self - n * other
        where n is the integer nearest to the exact value of self / other, and
        if two integers are equally near then the even one is chosen.
        
        If the result is zero then its sign will be the sign of self.
        """
        pass

    def rotate(self, *args, **kwargs): # real signature unknown
        """
        Return the result of rotating the digits of the first operand by an amount
        specified by the second operand.  The second operand must be an integer in
        the range -precision through precision. The absolute value of the second
        operand gives the number of places to rotate. If the second operand is
        positive then rotation is to the left; otherwise rotation is to the right.
        The coefficient of the first operand is padded on the left with zeros to
        length precision if necessary. The sign and exponent of the first operand are
        unchanged.
        """
        pass

    def same_quantum(self, *args, **kwargs): # real signature unknown
        """
        Test whether self and other have the same exponent or whether both are NaN.
        
        This operation is unaffected by context and is quiet: no flags are changed
        and no rounding is performed. As an exception, the C version may raise
        InvalidOperation if the second operand cannot be converted exactly.
        """
        pass

    def scaleb(self, *args, **kwargs): # real signature unknown
        """
        Return the first operand with the exponent adjusted the second.  Equivalently,
        return the first operand multiplied by 10**other. The second operand must be
        an integer.
        """
        pass

    def shift(self, *args, **kwargs): # real signature unknown
        """
        Return the result of shifting the digits of the first operand by an amount
        specified by the second operand.  The second operand must be an integer in
        the range -precision through precision. The absolute value of the second
        operand gives the number of places to shift. If the second operand is
        positive, then the shift is to the left; otherwise the shift is to the
        right. Digits shifted into the coefficient are zeros. The sign and exponent
        of the first operand are unchanged.
        """
        pass

    def sqrt(self, *args, **kwargs): # real signature unknown
        """
        Return the square root of the argument to full precision. The result is
        correctly rounded using the ROUND_HALF_EVEN rounding mode.
        """
        pass

    def to_eng_string(self, *args, **kwargs): # real signature unknown
        """
        Convert to an engineering-type string.  Engineering notation has an exponent
        which is a multiple of 3, so there are up to 3 digits left of the decimal
        place. For example, Decimal('123E+1') is converted to Decimal('1.23E+3').
        
        The value of context.capitals determines whether the exponent sign is lower
        or upper case. Otherwise, the context does not affect the operation.
        """
        pass

    def to_integral(self): # real signature unknown; restored from __doc__
        """
        Identical to the to_integral_value() method.  The to_integral() name has been
        kept for compatibility with older versions.
        """
        pass

    def to_integral_exact(self, *args, **kwargs): # real signature unknown
        """
        Round to the nearest integer, signaling Inexact or Rounded as appropriate if
        rounding occurs.  The rounding mode is determined by the rounding parameter
        if given, else by the given context. If neither parameter is given, then the
        rounding mode of the current default context is used.
        """
        pass

    def to_integral_value(self, *args, **kwargs): # real signature unknown
        """
        Round to the nearest integer without signaling Inexact or Rounded.  The
        rounding mode is determined by the rounding parameter if given, else by
        the given context. If neither parameter is given, then the rounding mode
        of the current default context is used.
        """
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

    def __ceil__(self, *args, **kwargs): # real signature unknown
        pass

    def __complex__(self, *args, **kwargs): # real signature unknown
        pass

    def __copy__(self, *args, **kwargs): # real signature unknown
        pass

    def __deepcopy__(self, *args, **kwargs): # real signature unknown
        pass

    def __divmod__(self, *args, **kwargs): # real signature unknown
        """ Return divmod(self, value). """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
        pass

    def __float__(self, *args, **kwargs): # real signature unknown
        """ float(self) """
        pass

    def __floordiv__(self, *args, **kwargs): # real signature unknown
        """ Return self//value. """
        pass

    def __floor__(self, *args, **kwargs): # real signature unknown
        pass

    def __format__(self, *args, **kwargs): # real signature unknown
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

    def __init__(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        pass

    def __int__(self, *args, **kwargs): # real signature unknown
        """ int(self) """
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

    def __pow__(self, *args, **kwargs): # real signature unknown
        """ Return pow(self, value, mod). """
        pass

    def __radd__(self, *args, **kwargs): # real signature unknown
        """ Return value+self. """
        pass

    def __rdivmod__(self, *args, **kwargs): # real signature unknown
        """ Return divmod(value, self). """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
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

    def __round__(self, *args, **kwargs): # real signature unknown
        pass

    def __rpow__(self, *args, **kwargs): # real signature unknown
        """ Return pow(value, self, mod). """
        pass

    def __rsub__(self, *args, **kwargs): # real signature unknown
        """ Return value-self. """
        pass

    def __rtruediv__(self, *args, **kwargs): # real signature unknown
        """ Return value/self. """
        pass

    def __sizeof__(self, *args, **kwargs): # real signature unknown
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

    def __trunc__(self, *args, **kwargs): # real signature unknown
        pass

    imag = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    real = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default



class FloatValidator(Validator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E75A8>'


class Number(object):
    """
    All numbers inherit from this class.
    
        If you just want to check if an argument x is a number, without
        caring what kind, use isinstance(x, Number).
    """
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    _abc_impl = None # (!) real value is '<_abc_data object at 0x031E2E60>'
    __abstractmethods__ = frozenset()
    __hash__ = None
    __slots__ = ()


class Fraction(__numbers.Rational):
    """
    This class implements rational numbers.
    
        In the two-argument form of the constructor, Fraction(8, 6) will
        produce a rational number equivalent to 4/3. Both arguments must
        be Rational. The numerator defaults to 0 and the denominator
        defaults to 1 so that Fraction(3) == 3 and Fraction() == 0.
    
        Fractions can also be constructed from:
    
          - numeric strings similar to those accepted by the
            float constructor (for example, '-2.3' or '1e10')
    
          - strings of the form '123/456'
    
          - float and Decimal instances
    
          - other Rational instances (including integers)
    """
    @classmethod
    def from_decimal(cls, *args, **kwargs): # real signature unknown
        """ Converts a finite Decimal instance to a rational number, exactly. """
        pass

    @classmethod
    def from_float(cls, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Converts a finite float to a rational number, exactly.
        
                Beware that Fraction.from_float(0.3) != Fraction(3, 10).
        """
        pass

    def limit_denominator(self, max_denominator=1000000): # reliably restored by inspect
        """
        Closest Fraction to self with denominator at most max_denominator.
        
                >>> Fraction('3.141592653589793').limit_denominator(10)
                Fraction(22, 7)
                >>> Fraction('3.141592653589793').limit_denominator(100)
                Fraction(311, 99)
                >>> Fraction(4321, 8765).limit_denominator(10000)
                Fraction(4321, 8765)
        """
        pass

    def _add(a, b): # reliably restored by inspect
        """ a + b """
        pass

    def _div(a, b): # reliably restored by inspect
        """ a / b """
        pass

    def _mul(a, b): # reliably restored by inspect
        """ a * b """
        pass

    def _operator_fallbacks(monomorphic_operator, fallback_operator): # reliably restored by inspect
        """
        Generates forward and reverse operators given a purely-rational
                operator and a function from the operator module.
        
                Use this like:
                __op__, __rop__ = _operator_fallbacks(just_rational_op, operator.op)
        
                In general, we want to implement the arithmetic operations so
                that mixed-mode operations either call an implementation whose
                author knew about the types of both arguments, or convert both
                to the nearest built in type and do the operation there. In
                Fraction, that means that we define __add__ and __radd__ as:
        
                    def __add__(self, other):
                        # Both types have numerators/denominator attributes,
                        # so do the operation directly
                        if isinstance(other, (int, Fraction)):
                            return Fraction(self.numerator * other.denominator +
                                            other.numerator * self.denominator,
                                            self.denominator * other.denominator)
                        # float and complex don't have those operations, but we
                        # know about those types, so special case them.
                        elif isinstance(other, float):
                            return float(self) + other
                        elif isinstance(other, complex):
                            return complex(self) + other
                        # Let the other type take over.
                        return NotImplemented
        
                    def __radd__(self, other):
                        # radd handles more types than add because there's
                        # nothing left to fall back to.
                        if isinstance(other, numbers.Rational):
                            return Fraction(self.numerator * other.denominator +
                                            other.numerator * self.denominator,
                                            self.denominator * other.denominator)
                        elif isinstance(other, Real):
                            return float(other) + float(self)
                        elif isinstance(other, Complex):
                            return complex(other) + complex(self)
                        return NotImplemented
        
        
                There are 5 different cases for a mixed-type addition on
                Fraction. I'll refer to all of the above code that doesn't
                refer to Fraction, float, or complex as "boilerplate". 'r'
                will be an instance of Fraction, which is a subtype of
                Rational (r : Fraction <: Rational), and b : B <:
                Complex. The first three involve 'r + b':
        
                    1. If B <: Fraction, int, float, or complex, we handle
                       that specially, and all is well.
                    2. If Fraction falls back to the boilerplate code, and it
                       were to return a value from __add__, we'd miss the
                       possibility that B defines a more intelligent __radd__,
                       so the boilerplate should return NotImplemented from
                       __add__. In particular, we don't handle Rational
                       here, even though we could get an exact answer, in case
                       the other type wants to do something special.
                    3. If B <: Fraction, Python tries B.__radd__ before
                       Fraction.__add__. This is ok, because it was
                       implemented with knowledge of Fraction, so it can
                       handle those instances before delegating to Real or
                       Complex.
        
                The next two situations describe 'b + r'. We assume that b
                didn't know about Fraction in its implementation, and that it
                uses similar boilerplate code:
        
                    4. If B <: Rational, then __radd_ converts both to the
                       builtin rational type (hey look, that's us) and
                       proceeds.
                    5. Otherwise, __radd__ tries to find the nearest common
                       base ABC, and fall back to its builtin type. Since this
                       class doesn't subclass a concrete type, there's no
                       implementation to fall back to, so we need to try as
                       hard as possible to return an actual value, or the user
                       will get a TypeError.
        """
        pass

    def _richcmp(self, other, op): # reliably restored by inspect
        """
        Helper for comparison operators, for internal use only.
        
                Implement comparison between a Rational instance `self`, and
                either another Rational instance or a float `other`.  If
                `other` is not a Rational instance or a float, return
                NotImplemented. `op` should be one of the six standard
                comparison operators.
        """
        pass

    def _sub(a, b): # reliably restored by inspect
        """ a - b """
        pass

    def __abs__(a): # reliably restored by inspect
        """ abs(a) """
        pass

    def __add__(a, b): # reliably restored by inspect
        """ a + b """
        pass

    def __bool__(a): # reliably restored by inspect
        """ a != 0 """
        pass

    def __ceil__(a): # reliably restored by inspect
        """ Will be math.ceil(a) in 3.0. """
        pass

    def __copy__(self): # reliably restored by inspect
        # no doc
        pass

    def __deepcopy__(self, memo): # reliably restored by inspect
        # no doc
        pass

    def __eq__(a, b): # reliably restored by inspect
        """ a == b """
        pass

    def __floordiv__(a, b): # reliably restored by inspect
        """ a // b """
        pass

    def __floor__(a): # reliably restored by inspect
        """ Will be math.floor(a) in 3.0. """
        pass

    def __ge__(a, b): # reliably restored by inspect
        """ a >= b """
        pass

    def __gt__(a, b): # reliably restored by inspect
        """ a > b """
        pass

    def __hash__(self): # reliably restored by inspect
        """ hash(self) """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        pass

    def __le__(a, b): # reliably restored by inspect
        """ a <= b """
        pass

    def __lt__(a, b): # reliably restored by inspect
        """ a < b """
        pass

    def __mod__(a, b): # reliably restored by inspect
        """ a % b """
        pass

    def __mul__(a, b): # reliably restored by inspect
        """ a * b """
        pass

    def __neg__(a): # reliably restored by inspect
        """ -a """
        pass

    @staticmethod # known case of __new__
# Error generating skeleton for function __new__: Function has keyword-only parameters or annotations, use getfullargspec() API which can support them

    def __pos__(a): # reliably restored by inspect
        """ +a: Coerces a subclass instance to Fraction """
        pass

    def __pow__(a, b): # reliably restored by inspect
        """
        a ** b
        
                If b is not an integer, the result will be a float or complex
                since roots are generally irrational. If b is an integer, the
                result will be rational.
        """
        pass

    def __radd__(b, a): # reliably restored by inspect
        """ a + b """
        pass

    def __reduce__(self): # reliably restored by inspect
        # no doc
        pass

    def __repr__(self): # reliably restored by inspect
        """ repr(self) """
        pass

    def __rfloordiv__(b, a): # reliably restored by inspect
        """ a // b """
        pass

    def __rmod__(b, a): # reliably restored by inspect
        """ a % b """
        pass

    def __rmul__(b, a): # reliably restored by inspect
        """ a * b """
        pass

    def __round__(self, ndigits=None): # reliably restored by inspect
        """
        Will be round(self, ndigits) in 3.0.
        
                Rounds half toward even.
        """
        pass

    def __rpow__(b, a): # reliably restored by inspect
        """ a ** b """
        pass

    def __rsub__(b, a): # reliably restored by inspect
        """ a - b """
        pass

    def __rtruediv__(b, a): # reliably restored by inspect
        """ a / b """
        pass

    def __str__(self): # reliably restored by inspect
        """ str(self) """
        pass

    def __sub__(a, b): # reliably restored by inspect
        """ a - b """
        pass

    def __truediv__(a, b): # reliably restored by inspect
        """ a / b """
        pass

    def __trunc__(a): # reliably restored by inspect
        """ trunc(a) """
        pass

    denominator = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    numerator = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    _denominator = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    _numerator = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    _abc_impl = None # (!) real value is '<_abc_data object at 0x09791E40>'
    __abstractmethods__ = frozenset()
    __slots__ = (
        '_numerator',
        '_denominator',
    )


class IntegerFloatValidator(Validator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E75D8>'


class IntegerValidator(Validator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E74B8>'


class IntervalValidator(Validator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E7878>'


class PeriodValidator(TemporalValidator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E7848>'


class Seen(object):
    """
    Class for keeping track of the types of elements
        encountered when trying to perform type conversions.
    """
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

    is_bool = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    is_float_or_complex = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    numeric_ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E7650>'


class StringValidator(Validator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E7668>'


class TimeValidator(Validator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E7818>'


class UnicodeValidator(Validator):
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

    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x096E7698>'


# variables with complex values

_TYPE_MAP = {
    'M': 'datetime64',
    'S': 'bytes',
    'U': 'string',
    'b': 'boolean',
    'bool': 'boolean',
    'c': 'complex',
    'categorical': 'categorical',
    'category': 'categorical',
    'complex128': 'complex',
    'datetime64[ns]': 'datetime64',
    'f': 'floating',
    'float16': 'floating',
    'float32': 'floating',
    'float64': 'floating',
    'i': 'integer',
    'int16': 'integer',
    'int32': 'integer',
    'int64': 'integer',
    'int8': 'integer',
    'm': 'timedelta64',
    'string': 'bytes',
    'timedelta64[ns]': 'timedelta64',
    'u': 'integer',
    'uint16': 'integer',
    'uint32': 'integer',
    'uint64': 'integer',
    'uint8': 'integer',
    'unicode': 'string',
}

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x096F92B0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pandas._libs.lib', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x096F92B0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pandas\\\\_libs\\\\lib.cp37-win32.pyd')"

__test__ = {
    'infer_dtype (line 1099)': "\n    Efficiently infer the type of a passed val, or list-like\n    array of values. Return a string describing the type.\n\n    Parameters\n    ----------\n    value : scalar, list, ndarray, or pandas type\n    skipna : bool, default False\n        Ignore NaN values when inferring the type.\n\n        .. versionadded:: 0.21.0\n\n    Returns\n    -------\n    string describing the common type of the input data.\n    Results can include:\n\n    - string\n    - unicode\n    - bytes\n    - floating\n    - integer\n    - mixed-integer\n    - mixed-integer-float\n    - decimal\n    - complex\n    - categorical\n    - boolean\n    - datetime64\n    - datetime\n    - date\n    - timedelta64\n    - timedelta\n    - time\n    - period\n    - mixed\n\n    Raises\n    ------\n    TypeError if ndarray-like but cannot infer the dtype\n\n    Notes\n    -----\n    - 'mixed' is the catchall for anything that is not otherwise\n      specialized\n    - 'mixed-integer-float' are floats and integers\n    - 'mixed-integer' are integers mixed with non-integers\n\n    Examples\n    --------\n    >>> infer_dtype(['foo', 'bar'])\n    'string'\n\n    >>> infer_dtype(['a', np.nan, 'b'], skipna=True)\n    'string'\n\n    >>> infer_dtype(['a', np.nan, 'b'], skipna=False)\n    'mixed'\n\n    >>> infer_dtype([b'foo', b'bar'])\n    'bytes'\n\n    >>> infer_dtype([1, 2, 3])\n    'integer'\n\n    >>> infer_dtype([1, 2, 3.5])\n    'mixed-integer-float'\n\n    >>> infer_dtype([1.0, 2.0, 3.5])\n    'floating'\n\n    >>> infer_dtype(['a', 1])\n    'mixed-integer'\n\n    >>> infer_dtype([Decimal(1), Decimal(2.0)])\n    'decimal'\n\n    >>> infer_dtype([True, False])\n    'boolean'\n\n    >>> infer_dtype([True, False, np.nan])\n    'mixed'\n\n    >>> infer_dtype([pd.Timestamp('20130101')])\n    'datetime'\n\n    >>> infer_dtype([datetime.date(2013, 1, 1)])\n    'date'\n\n    >>> infer_dtype([np.datetime64('2013-01-01')])\n    'datetime64'\n\n    >>> infer_dtype([datetime.timedelta(0, 1, 1)])\n    'timedelta'\n\n    >>> infer_dtype(pd.Series(list('aabc')).astype('category'))\n    'categorical'\n    ",
    'is_scalar (line 108)': '\n    Return True if given value is scalar.\n\n    Parameters\n    ----------\n    val : object\n        This includes:\n\n        - numpy array scalar (e.g. np.int64)\n        - Python builtin numerics\n        - Python builtin byte arrays and strings\n        - None\n        - datetime.datetime\n        - datetime.timedelta\n        - Period\n        - decimal.Decimal\n        - Interval\n        - DateOffset\n        - Fraction\n        - Number\n\n    Returns\n    -------\n    bool\n        Return True if given object is scalar, False otherwise\n\n    Examples\n    --------\n    >>> dt = pd.datetime.datetime(2018, 10, 3)\n    >>> pd.is_scalar(dt)\n    True\n\n    >>> pd.api.types.is_scalar([2, 3])\n    False\n\n    >>> pd.api.types.is_scalar({0: 1, 2: 3})\n    False\n\n    >>> pd.api.types.is_scalar((0, 2))\n    False\n\n    pandas supports PEP 3141 numbers:\n\n    >>> from fractions import Fraction\n    >>> pd.api.types.is_scalar(Fraction(3, 5))\n    True\n    ',
    'item_from_zerodim (line 172)': "\n    If the value is a zerodim array, return the item it contains.\n\n    Parameters\n    ----------\n    val : object\n\n    Returns\n    -------\n    result : object\n\n    Examples\n    --------\n    >>> item_from_zerodim(1)\n    1\n    >>> item_from_zerodim('foobar')\n    'foobar'\n    >>> item_from_zerodim(np.array(1))\n    1\n    >>> item_from_zerodim(np.array([1]))\n    array([1])\n\n    ",
}

