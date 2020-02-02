# encoding: utf-8
# module scipy.linalg.cython_blas
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\linalg\cython_blas.cp37-win32.pyd
# by generator 1.147
"""
BLAS Functions for Cython
=========================

Usable from Cython via::

    cimport scipy.linalg.cython_blas

These wrappers do not check for alignment of arrays.
Alignment should be checked before these wrappers are used.

Raw function pointers (Fortran-style pointer arguments):

- caxpy
- ccopy
- cdotc
- cdotu
- cgbmv
- cgemm
- cgemv
- cgerc
- cgeru
- chbmv
- chemm
- chemv
- cher
- cher2
- cher2k
- cherk
- chpmv
- chpr
- chpr2
- crotg
- cscal
- csrot
- csscal
- cswap
- csymm
- csyr2k
- csyrk
- ctbmv
- ctbsv
- ctpmv
- ctpsv
- ctrmm
- ctrmv
- ctrsm
- ctrsv
- dasum
- daxpy
- dcabs1
- dcopy
- ddot
- dgbmv
- dgemm
- dgemv
- dger
- dnrm2
- drot
- drotg
- drotm
- drotmg
- dsbmv
- dscal
- dsdot
- dspmv
- dspr
- dspr2
- dswap
- dsymm
- dsymv
- dsyr
- dsyr2
- dsyr2k
- dsyrk
- dtbmv
- dtbsv
- dtpmv
- dtpsv
- dtrmm
- dtrmv
- dtrsm
- dtrsv
- dzasum
- dznrm2
- icamax
- idamax
- isamax
- izamax
- lsame
- sasum
- saxpy
- scasum
- scnrm2
- scopy
- sdot
- sdsdot
- sgbmv
- sgemm
- sgemv
- sger
- snrm2
- srot
- srotg
- srotm
- srotmg
- ssbmv
- sscal
- sspmv
- sspr
- sspr2
- sswap
- ssymm
- ssymv
- ssyr
- ssyr2
- ssyr2k
- ssyrk
- stbmv
- stbsv
- stpmv
- stpsv
- strmm
- strmv
- strsm
- strsv
- zaxpy
- zcopy
- zdotc
- zdotu
- zdrot
- zdscal
- zgbmv
- zgemm
- zgemv
- zgerc
- zgeru
- zhbmv
- zhemm
- zhemv
- zher
- zher2
- zher2k
- zherk
- zhpmv
- zhpr
- zhpr2
- zrotg
- zscal
- zswap
- zsymm
- zsyr2k
- zsyrk
- ztbmv
- ztbsv
- ztpmv
- ztpsv
- ztrmm
- ztrmv
- ztrsm
- ztrsv
"""

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>

# functions

def _test_cdotc(*args, **kwargs): # real signature unknown
    pass

def _test_cdotu(*args, **kwargs): # real signature unknown
    pass

def _test_dasum(*args, **kwargs): # real signature unknown
    pass

def _test_ddot(*args, **kwargs): # real signature unknown
    pass

def _test_dgemm(*args, **kwargs): # real signature unknown
    pass

def _test_dnrm2(*args, **kwargs): # real signature unknown
    pass

def _test_dzasum(*args, **kwargs): # real signature unknown
    pass

def _test_dznrm2(*args, **kwargs): # real signature unknown
    pass

def _test_icamax(*args, **kwargs): # real signature unknown
    pass

def _test_idamax(*args, **kwargs): # real signature unknown
    pass

def _test_isamax(*args, **kwargs): # real signature unknown
    pass

def _test_izamax(*args, **kwargs): # real signature unknown
    pass

def _test_sasum(*args, **kwargs): # real signature unknown
    pass

def _test_scasum(*args, **kwargs): # real signature unknown
    pass

def _test_scnrm2(*args, **kwargs): # real signature unknown
    pass

def _test_sdot(*args, **kwargs): # real signature unknown
    pass

def _test_snrm2(*args, **kwargs): # real signature unknown
    pass

def _test_zdotc(*args, **kwargs): # real signature unknown
    pass

def _test_zdotu(*args, **kwargs): # real signature unknown
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0F527450>'

__pyx_capi__ = {
    'caxpy': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46B938>'
    'ccopy': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46B7E8>'
    'cdotc': None, # (!) real value is '<capsule object "__pyx_t_float_complex (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46B9C8>'
    'cdotu': None, # (!) real value is '<capsule object "__pyx_t_float_complex (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BB48>'
    'cgbmv': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BBF0>'
    'cgemm': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46B950>'
    'cgemv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BC08>'
    'cgerc': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BC20>'
    'cgeru': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BC38>'
    'chbmv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BC50>'
    'chemm': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BAE8>'
    'chemv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BC68>'
    'cher': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BC80>'
    'cher2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BC98>'
    'cher2k': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_float_complex *, int *)" at 0x0F46BCB0>'
    'cherk': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_float_complex *, int *)" at 0x0F46BCC8>'
    'chpmv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BCE0>'
    'chpr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *)" at 0x0F46BCF8>'
    'chpr2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *)" at 0x0F46BD10>'
    'crotg': None, # (!) real value is '<capsule object "void (__pyx_t_float_complex *, __pyx_t_float_complex *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_float_complex *)" at 0x0F46BD28>'
    'cscal': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BD40>'
    'csrot': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *)" at 0x0F46BD58>'
    'csscal': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_float_complex *, int *)" at 0x0F46BD70>'
    'cswap': None, # (!) real value is '<capsule object "void (int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BD88>'
    'csymm': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BDA0>'
    'csyr2k': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BDB8>'
    'csyrk': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BDD0>'
    'ctbmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BDE8>'
    'ctbsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BE00>'
    'ctpmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BE18>'
    'ctpsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *)" at 0x0F46BE30>'
    'ctrmm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BE48>'
    'ctrmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BE60>'
    'ctrsm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_float_complex *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BE78>'
    'ctrsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_float_complex *, int *, __pyx_t_float_complex *, int *)" at 0x0F46BE90>'
    'dasum': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_d (int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F46BEA8>'
    'daxpy': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F46BEC0>'
    'dcabs1': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_d (__pyx_t_double_complex *)" at 0x0F46BED8>'
    'dcopy': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F46BEF0>'
    'ddot': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_d (int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F46BF08>'
    'dgbmv': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F46BF20>'
    'dgemm': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F46BF38>'
    'dgemv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F46BF50>'
    'dger': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F46BF68>'
    'dnrm2': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_d (int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F46BF80>'
    'drot': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *)" at 0x0F46BF98>'
    'drotg': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *)" at 0x0F46BFB0>'
    'drotm': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *)" at 0x0F46BFC8>'
    'drotmg': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *)" at 0x0F46BFE0>'
    'dsbmv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F46B650>'
    'dscal': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A020>'
    'dsdot': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_d (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A038>'
    'dspmv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A050>'
    'dspr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *)" at 0x0F52A068>'
    'dspr2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *)" at 0x0F52A080>'
    'dswap': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A098>'
    'dsymm': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A0B0>'
    'dsymv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A0C8>'
    'dsyr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A0E0>'
    'dsyr2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A0F8>'
    'dsyr2k': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A110>'
    'dsyrk': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A128>'
    'dtbmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A140>'
    'dtbsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A158>'
    'dtpmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A170>'
    'dtpsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A188>'
    'dtrmm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A1A0>'
    'dtrmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A1B8>'
    'dtrsm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A1D0>'
    'dtrsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A1E8>'
    'dzasum': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_d (int *, __pyx_t_double_complex *, int *)" at 0x0F52A200>'
    'dznrm2': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_d (int *, __pyx_t_double_complex *, int *)" at 0x0F52A218>'
    'icamax': None, # (!) real value is '<capsule object "int (int *, __pyx_t_float_complex *, int *)" at 0x0F52A230>'
    'idamax': None, # (!) real value is '<capsule object "int (int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, int *)" at 0x0F52A248>'
    'isamax': None, # (!) real value is '<capsule object "int (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A260>'
    'izamax': None, # (!) real value is '<capsule object "int (int *, __pyx_t_double_complex *, int *)" at 0x0F52A278>'
    'lsame': None, # (!) real value is '<capsule object "int (char *, char *)" at 0x0F52A290>'
    'sasum': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_s (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A2A8>'
    'saxpy': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A2C0>'
    'scasum': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_s (int *, __pyx_t_float_complex *, int *)" at 0x0F52A2D8>'
    'scnrm2': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_s (int *, __pyx_t_float_complex *, int *)" at 0x0F52A2F0>'
    'scopy': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A308>'
    'sdot': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_s (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A320>'
    'sdsdot': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_s (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A338>'
    'sgbmv': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A350>'
    'sgemm': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A368>'
    'sgemv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A380>'
    'sger': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A398>'
    'snrm2': None, # (!) real value is '<capsule object "__pyx_t_5scipy_6linalg_11cython_blas_s (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A3B0>'
    'srot': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *)" at 0x0F52A3C8>'
    'srotg': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *)" at 0x0F52A3E0>'
    'srotm': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *)" at 0x0F52A3F8>'
    'srotmg': None, # (!) real value is '<capsule object "void (__pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *)" at 0x0F52A410>'
    'ssbmv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A428>'
    'sscal': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A440>'
    'sspmv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A458>'
    'sspr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *)" at 0x0F52A470>'
    'sspr2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *)" at 0x0F52A488>'
    'sswap': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A4A0>'
    'ssymm': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A4B8>'
    'ssymv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A4D0>'
    'ssyr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A4E8>'
    'ssyr2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A500>'
    'ssyr2k': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A518>'
    'ssyrk': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A530>'
    'stbmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A548>'
    'stbsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A560>'
    'stpmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A578>'
    'stpsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A590>'
    'strmm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A5A8>'
    'strmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A5C0>'
    'strsm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A5D8>'
    'strsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *, __pyx_t_5scipy_6linalg_11cython_blas_s *, int *)" at 0x0F52A5F0>'
    'zaxpy': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A608>'
    'zcopy': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A620>'
    'zdotc': None, # (!) real value is '<capsule object "__pyx_t_double_complex (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A638>'
    'zdotu': None, # (!) real value is '<capsule object "__pyx_t_double_complex (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A650>'
    'zdrot': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_5scipy_6linalg_11cython_blas_d *)" at 0x0F52A668>'
    'zdscal': None, # (!) real value is '<capsule object "void (int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_double_complex *, int *)" at 0x0F52A680>'
    'zgbmv': None, # (!) real value is '<capsule object "void (char *, int *, int *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A698>'
    'zgemm': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A6B0>'
    'zgemv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A6C8>'
    'zgerc': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A6E0>'
    'zgeru': None, # (!) real value is '<capsule object "void (int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A6F8>'
    'zhbmv': None, # (!) real value is '<capsule object "void (char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A710>'
    'zhemm': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A728>'
    'zhemv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A740>'
    'zher': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A758>'
    'zher2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A770>'
    'zher2k': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_double_complex *, int *)" at 0x0F52A788>'
    'zherk': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_double_complex *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_double_complex *, int *)" at 0x0F52A7A0>'
    'zhpmv': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A7B8>'
    'zhpr': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *)" at 0x0F52A7D0>'
    'zhpr2': None, # (!) real value is '<capsule object "void (char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *)" at 0x0F52A7E8>'
    'zrotg': None, # (!) real value is '<capsule object "void (__pyx_t_double_complex *, __pyx_t_double_complex *, __pyx_t_5scipy_6linalg_11cython_blas_d *, __pyx_t_double_complex *)" at 0x0F52A800>'
    'zscal': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A818>'
    'zswap': None, # (!) real value is '<capsule object "void (int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A830>'
    'zsymm': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A848>'
    'zsyr2k': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A860>'
    'zsyrk': None, # (!) real value is '<capsule object "void (char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A878>'
    'ztbmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A890>'
    'ztbsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A8A8>'
    'ztpmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A8C0>'
    'ztpsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *)" at 0x0F52A8D8>'
    'ztrmm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A8F0>'
    'ztrmv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A908>'
    'ztrsm': None, # (!) real value is '<capsule object "void (char *, char *, char *, char *, int *, int *, __pyx_t_double_complex *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A920>'
    'ztrsv': None, # (!) real value is '<capsule object "void (char *, char *, char *, int *, __pyx_t_double_complex *, int *, __pyx_t_double_complex *, int *)" at 0x0F52A938>'
}

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.linalg.cython_blas', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0F527450>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\linalg\\\\cython_blas.cp37-win32.pyd')"

__test__ = {}

