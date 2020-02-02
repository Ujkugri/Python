# encoding: utf-8
# module sklearn.utils.murmurhash
# from C:\Program Files (x86)\Python37-32\lib\site-packages\sklearn\utils\murmurhash.cp37-win32.pyd
# by generator 1.147
"""
Cython wrapper for MurmurHash3 non-cryptographic hash function

MurmurHash is an extensively tested and very fast hash function that has
good distribution properties suitable for machine learning use cases
such as feature hashing and random projections.

The original C++ code by Austin Appleby is released the public domain
and can be found here:

  https://code.google.com/p/smhasher/
"""

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py

# functions

def murmurhash3_32(*args, **kwargs): # real signature unknown
    """
    Compute the 32bit murmurhash3 of key at seed.
    
        The underlying implementation is MurmurHash3_x86_32 generating low
        latency 32bits hash suitable for implementing lookup tables, Bloom
        filters, count min sketch or feature hashing.
    
        Parameters
        ----------
        key : int32, bytes, unicode or ndarray with dtype int32
            the physical object to hash
    
        seed : int, optional default is 0
            integer seed for the hashing algorithm.
    
        positive : boolean, optional default is False
            True: the results is casted to an unsigned int
              from 0 to 2 ** 32 - 1
            False: the results is casted to a signed int
              from -(2 ** 31) to 2 ** 31 - 1
    """
    pass

def murmurhash3_bytes_array_s32(*args, **kwargs): # real signature unknown
    """ Compute 32bit murmurhash3 hashes of a key int array at seed. """
    pass

def murmurhash3_bytes_array_u32(*args, **kwargs): # real signature unknown
    """ Compute 32bit murmurhash3 hashes of a key int array at seed. """
    pass

def murmurhash3_bytes_s32(*args, **kwargs): # real signature unknown
    """ Compute the 32bit murmurhash3 of a bytes key at seed. """
    pass

def murmurhash3_bytes_u32(*args, **kwargs): # real signature unknown
    """ Compute the 32bit murmurhash3 of a bytes key at seed. """
    pass

def murmurhash3_int_s32(*args, **kwargs): # real signature unknown
    """ Compute the 32bit murmurhash3 of a int key at seed. """
    pass

def murmurhash3_int_u32(*args, **kwargs): # real signature unknown
    """ Compute the 32bit murmurhash3 of a int key at seed. """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0A48C830>'

__pyx_capi__ = {
    'murmurhash3_bytes_s32': None, # (!) real value is '<capsule object "__pyx_t_5numpy_int32_t (PyObject *, unsigned int, int __pyx_skip_dispatch)" at 0x0A3D4F50>'
    'murmurhash3_bytes_u32': None, # (!) real value is '<capsule object "__pyx_t_5numpy_uint32_t (PyObject *, unsigned int, int __pyx_skip_dispatch)" at 0x0A308DD0>'
    'murmurhash3_int_s32': None, # (!) real value is '<capsule object "__pyx_t_5numpy_int32_t (int, unsigned int, int __pyx_skip_dispatch)" at 0x0A308DB8>'
    'murmurhash3_int_u32': None, # (!) real value is '<capsule object "__pyx_t_5numpy_uint32_t (int, unsigned int, int __pyx_skip_dispatch)" at 0x0A308EC0>'
}

__spec__ = None # (!) real value is "ModuleSpec(name='sklearn.utils.murmurhash', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0A48C830>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\sklearn\\\\utils\\\\murmurhash.cp37-win32.pyd')"

__test__ = {}

