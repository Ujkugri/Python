# encoding: utf-8
# module pandas._libs.internals
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pandas\_libs\internals.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
from pandas._libs.algos import ensure_int64


# functions

def get_blkno_indexers(*args, **kwargs): # real signature unknown
    """
    Enumerate contiguous runs of integers in ndarray.
    
        Iterate over elements of `blknos` yielding ``(blkno, slice(start, stop))``
        pairs for each contiguous run found.
    
        If `group` is True and there is more than one run for a certain blkno,
        ``(blkno, array)`` with an array containing positions of all elements equal
        to blkno.
    
        Returns
        -------
        iter : iterator of (int, slice or array)
    """
    pass

def get_blkno_placements(*args, **kwargs): # real signature unknown
    """
    Parameters
        ----------
        blknos : array of int64
        blk_count : int
        group : bool
    
        Returns
        -------
        iterator
            yield (BlockPlacement, blkno)
    """
    pass

def indexer_as_slice(*args, **kwargs): # real signature unknown
    pass

def slice_getitem(*args, **kwargs): # real signature unknown
    pass

def slice_len(*args, **kwargs): # real signature unknown
    """
    Get length of a bounded slice.
    
        The slice must not have any "open" bounds that would create dependency on
        container size, i.e.:
        - if ``s.step is None or s.step > 0``, ``s.stop`` is not ``None``
        - if ``s.step < 0``, ``s.start`` is not ``None``
    
        Otherwise, the result is unreliable.
    """
    pass

def __pyx_unpickle_BlockPlacement(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# classes

class BlockPlacement(object):
    # no doc
    def add(self, *args, **kwargs): # real signature unknown
        pass

    def append(self, *args, **kwargs): # real signature unknown
        pass

    def delete(self, *args, **kwargs): # real signature unknown
        pass

    def isin(self, *args, **kwargs): # real signature unknown
        pass

    def sub(self, *args, **kwargs): # real signature unknown
        pass

    def __getitem__(self, *args, **kwargs): # real signature unknown
        """ Return self[key]. """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    def __iter__(self, *args, **kwargs): # real signature unknown
        """ Implement iter(self). """
        pass

    def __len__(self, *args, **kwargs): # real signature unknown
        """ Return len(self). """
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass

    def __str__(self, *args, **kwargs): # real signature unknown
        """ Return str(self). """
        pass

    as_array = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    as_slice = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    indexer = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    is_slice_like = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __pyx_vtable__ = None # (!) real value is '<capsule object NULL at 0x09C9E230>'


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x09C9CD10>'

__spec__ = None # (!) real value is "ModuleSpec(name='pandas._libs.internals', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x09C9CD10>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pandas\\\\_libs\\\\internals.cp37-win32.pyd')"

__test__ = {}

