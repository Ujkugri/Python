# encoding: utf-8
# module _cffi_backend
# from C:\Program Files (x86)\Python37-32\lib\site-packages\_cffi_backend.cp37-win32.pyd
# by generator 1.147
# no doc
# no imports

# Variables with simple values

FFI_CDECL = 1

FFI_DEFAULT_ABI = 1

FFI_STDCALL = 2

RTLD_GLOBAL = 0
RTLD_LAZY = 0
RTLD_LOCAL = 0
RTLD_NOW = 0

_WIN = 32

__version__ = '1.12.3'

# functions

def alignof(*args, **kwargs): # real signature unknown
    pass

def callback(*args, **kwargs): # real signature unknown
    pass

def cast(*args, **kwargs): # real signature unknown
    pass

def complete_struct_or_union(*args, **kwargs): # real signature unknown
    pass

def from_buffer(*args, **kwargs): # real signature unknown
    pass

def from_handle(*args, **kwargs): # real signature unknown
    pass

def gcp(*args, **kwargs): # real signature unknown
    pass

def getcname(*args, **kwargs): # real signature unknown
    pass

def getwinerror(*args, **kwargs): # real signature unknown
    pass

def get_errno(*args, **kwargs): # real signature unknown
    pass

def load_library(*args, **kwargs): # real signature unknown
    pass

def memmove(*args, **kwargs): # real signature unknown
    pass

def newp(*args, **kwargs): # real signature unknown
    pass

def newp_handle(*args, **kwargs): # real signature unknown
    pass

def new_array_type(*args, **kwargs): # real signature unknown
    pass

def new_enum_type(*args, **kwargs): # real signature unknown
    pass

def new_function_type(*args, **kwargs): # real signature unknown
    pass

def new_pointer_type(*args, **kwargs): # real signature unknown
    pass

def new_primitive_type(*args, **kwargs): # real signature unknown
    pass

def new_struct_type(*args, **kwargs): # real signature unknown
    pass

def new_union_type(*args, **kwargs): # real signature unknown
    pass

def new_void_type(*args, **kwargs): # real signature unknown
    pass

def rawaddressof(*args, **kwargs): # real signature unknown
    pass

def release(*args, **kwargs): # real signature unknown
    pass

def set_errno(*args, **kwargs): # real signature unknown
    pass

def sizeof(*args, **kwargs): # real signature unknown
    pass

def string(*args, **kwargs): # real signature unknown
    pass

def typeof(*args, **kwargs): # real signature unknown
    pass

def typeoffsetof(*args, **kwargs): # real signature unknown
    pass

def unpack(*args, **kwargs): # real signature unknown
    pass

def _get_common_types(*args, **kwargs): # real signature unknown
    pass

def _get_types(*args, **kwargs): # real signature unknown
    pass

def _init_cffi_1_0_external_module(*args, **kwargs): # real signature unknown
    pass

def _testbuff(*args, **kwargs): # real signature unknown
    pass

def _testfunc(*args, **kwargs): # real signature unknown
    pass

# classes

class buffer(object):
    """
    ffi.buffer(cdata[, byte_size]):
    Return a read-write buffer object that references the raw C data
    pointed to by the given 'cdata'.  The 'cdata' must be a pointer or an
    array.  Can be passed to functions expecting a buffer, or directly
    manipulated with:
    
        buf[:]          get a copy of it in a regular string, or
        buf[idx]        as a single character
        buf[:] = ...
        buf[idx] = ...  change the content
    """
    def __delitem__(self, *args, **kwargs): # real signature unknown
        """ Delete self[key]. """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
        pass

    def __getattribute__(self, *args, **kwargs): # real signature unknown
        """ Return getattr(self, name). """
        pass

    def __getitem__(self, *args, **kwargs): # real signature unknown
        """ Return self[key]. """
        pass

    def __ge__(self, *args, **kwargs): # real signature unknown
        """ Return self>=value. """
        pass

    def __gt__(self, *args, **kwargs): # real signature unknown
        """ Return self>value. """
        pass

    def __init__(self, cdata, byte_size=None): # real signature unknown; restored from __doc__
        pass

    def __len__(self, *args, **kwargs): # real signature unknown
        """ Return len(self). """
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

    def __setitem__(self, *args, **kwargs): # real signature unknown
        """ Set self[key] to value. """
        pass

    __hash__ = None


class FFI(object):
    # no doc
    def addressof(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Limited equivalent to the '&' operator in C:
        
        1. ffi.addressof(<cdata 'struct-or-union'>) returns a cdata that is a
        pointer to this struct or union.
        
        2. ffi.addressof(<cdata>, field-or-index...) returns the address of a
        field or array item inside the given structure or array, recursively
        in case of nested structures or arrays.
        
        3. ffi.addressof(<library>, "name") returns the address of the named
        function or global variable.
        """
        pass

    def alignof(self, *args, **kwargs): # real signature unknown
        """
        Return the natural alignment size in bytes of the argument.
        It can be a string naming a C type, or a 'cdata' instance.
        """
        pass

    def callback(self, *args, **kwargs): # real signature unknown
        """
        Return a callback object or a decorator making such a callback object.
        'cdecl' must name a C function pointer type.  The callback invokes the
        specified 'python_callable' (which may be provided either directly or
        via a decorator).  Important: the callback object must be manually
        kept alive for as long as the callback may be invoked from the C code.
        """
        pass

    def cast(self, *args, **kwargs): # real signature unknown
        """
        Similar to a C cast: returns an instance of the named C
        type initialized with the given 'source'.  The source is
        casted between integers or pointers of any type.
        """
        pass

    def def_extern(self, *args, **kwargs): # real signature unknown
        """
        A decorator.  Attaches the decorated Python function to the C code
        generated for the 'extern "Python"' function of the same name.
        Calling the C function will then invoke the Python function.
        
        Optional arguments: 'name' is the name of the C function, if
        different from the Python function; and 'error' and 'onerror'
        handle what occurs if the Python function raises an exception
        (see the docs for details).
        """
        pass

    def dlclose(self, *args, **kwargs): # real signature unknown
        """
        Close a library obtained with ffi.dlopen().  After this call, access to
        functions or variables from the library will fail (possibly with a
        segmentation fault).
        """
        pass

    def dlopen(self, *args, **kwargs): # real signature unknown
        """
        Load and return a dynamic library identified by 'name'.  The standard
        C library can be loaded by passing None.
        
        Note that functions and types declared with 'ffi.cdef()' are not
        linked to a particular library, just like C headers.  In the library
        we only look for the actual (untyped) symbols at the time of their
        first access.
        """
        pass

    def from_buffer(self, *args, **kwargs): # real signature unknown
        """
        Return a <cdata 'char[]'> that points to the data of the given Python
        object, which must support the buffer interface.  Note that this is
        not meant to be used on the built-in types str or unicode
        (you can build 'char[]' arrays explicitly) but only on objects
        containing large quantities of raw data in some other format, like
        'array.array' or numpy arrays.
        """
        pass

    def from_handle(self, *args, **kwargs): # real signature unknown
        """
        Cast a 'void *' back to a Python object.  Must be used *only* on the
        pointers returned by new_handle(), and *only* as long as the exact
        cdata object returned by new_handle() is still alive (somewhere else
        in the program).  Failure to follow these rules will crash.
        """
        pass

    def gc(self, *args, **kwargs): # real signature unknown
        """
        Return a new cdata object that points to the same data.
        Later, when this new cdata object is garbage-collected,
        'destructor(old_cdata_object)' will be called.
        
        The optional 'size' gives an estimate of the size, used to
        trigger the garbage collection more eagerly.  So far only used
        on PyPy.  It tells the GC that the returned object keeps alive
        roughly 'size' bytes of external memory.
        """
        pass

    def getctype(self, *args, **kwargs): # real signature unknown
        """
        Return a string giving the C type 'cdecl', which may be itself a
        string or a <ctype> object.  If 'replace_with' is given, it gives
        extra text to append (or insert for more complicated C types), like a
        variable name, or '*' to get actually the C type 'pointer-to-cdecl'.
        """
        pass

    def getwinerror(self, *args, **kwargs): # real signature unknown
        """
        Return either the GetLastError() or the error number given by the
        optional 'code' argument, as a tuple '(code, message)'.
        """
        pass

    def init_once(self, function, tag): # real signature unknown; restored from __doc__
        """
        init_once(function, tag): run function() once.  More precisely,
        'function()' is called the first time we see a given 'tag'.
        
        The return value of function() is remembered and returned by the current
        and all future init_once() with the same tag.  If init_once() is called
        from multiple threads in parallel, all calls block until the execution
        of function() is done.  If function() raises an exception, it is
        propagated and nothing is cached.
        """
        pass

    def integer_const(self, xxx): # real signature unknown; restored from __doc__
        """
        Get the value of an integer constant.
        
        'ffi.integer_const("xxx")' is equivalent to 'lib.xxx' if xxx names an
        integer constant.  The point of this function is limited to use cases
        where you have an 'ffi' object but not any associated 'lib' object.
        """
        pass

    def list_types(self, *args, **kwargs): # real signature unknown
        """
        Returns the user type names known to this FFI instance.
        This returns a tuple containing three lists of names:
        (typedef_names, names_of_structs, names_of_unions)
        """
        pass

    def memmove(self, dest, src, n): # real signature unknown; restored from __doc__
        """
        ffi.memmove(dest, src, n) copies n bytes of memory from src to dest.
        
        Like the C function memmove(), the memory areas may overlap;
        apart from that it behaves like the C function memcpy().
        
        'src' can be any cdata ptr or array, or any Python buffer object.
        'dest' can be any cdata ptr or array, or a writable Python buffer
        object.  The size to copy, 'n', is always measured in bytes.
        
        Unlike other methods, this one supports all Python buffer including
        byte strings and bytearrays---but it still does not support
        non-contiguous buffers.
        """
        pass

    def new(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Allocate an instance according to the specified C type and return a
        pointer to it.  The specified C type must be either a pointer or an
        array: ``new('X *')`` allocates an X and returns a pointer to it,
        whereas ``new('X[n]')`` allocates an array of n X'es and returns an
        array referencing it (which works mostly like a pointer, like in C).
        You can also use ``new('X[]', n)`` to allocate an array of a
        non-constant length n.
        
        The memory is initialized following the rules of declaring a global
        variable in C: by default it is zero-initialized, but an explicit
        initializer can be given which can be used to fill all or part of the
        memory.
        
        When the returned <cdata> object goes out of scope, the memory is
        freed.  In other words the returned <cdata> object has ownership of
        the value of type 'cdecl' that it points to.  This means that the raw
        data can be used as long as this object is kept alive, but must not be
        used for a longer time.  Be careful about that when copying the
        pointer to the memory somewhere else, e.g. into another structure.
        """
        pass

    def new_allocator(self, *args, **kwargs): # real signature unknown
        """
        Return a new allocator, i.e. a function that behaves like ffi.new()
        but uses the provided low-level 'alloc' and 'free' functions.
        
        'alloc' is called with the size as argument.  If it returns NULL, a
        MemoryError is raised.  'free' is called with the result of 'alloc'
        as argument.  Both can be either Python functions or directly C
        functions.  If 'free' is None, then no free function is called.
        If both 'alloc' and 'free' are None, the default is used.
        
        If 'should_clear_after_alloc' is set to False, then the memory
        returned by 'alloc' is assumed to be already cleared (or you are
        fine with garbage); otherwise CFFI will clear it.
        """
        pass

    def new_handle(self): # real signature unknown; restored from __doc__
        """
        Return a non-NULL cdata of type 'void *' that contains an opaque
        reference to the argument, which can be any Python object.  To cast it
        back to the original object, use from_handle().  You must keep alive
        the cdata object returned by new_handle()!
        """
        pass

    def NULL(self, *args, **kwargs): # real signature unknown
        pass

    def offsetof(self, *args, **kwargs): # real signature unknown
        """
        Return the offset of the named field inside the given structure or
        array, which must be given as a C type name.  You can give several
        field names in case of nested structures.  You can also give numeric
        values which correspond to array items, in case of an array type.
        """
        pass

    def release(self, cdata): # real signature unknown; restored from __doc__
        """
        Release now the resources held by a 'cdata' object from ffi.new(),
        ffi.gc() or ffi.from_buffer().  The cdata object must not be used
        afterwards.
        
        'ffi.release(cdata)' is equivalent to 'cdata.__exit__()'.
        
        Note that on CPython this method has no effect (so far) on objects
        returned by ffi.new(), because the memory is allocated inline with the
        cdata object and cannot be freed independently.  It might be fixed in
        future releases of cffi.
        """
        pass

    def sizeof(self, *args, **kwargs): # real signature unknown
        """
        Return the size in bytes of the argument.
        It can be a string naming a C type, or a 'cdata' instance.
        """
        pass

    def string(self, or_unicode_string): # real signature unknown; restored from __doc__
        """
        Return a Python string (or unicode string) from the 'cdata'.  If
        'cdata' is a pointer or array of characters or bytes, returns the
        null-terminated string.  The returned string extends until the first
        null character, or at most 'maxlen' characters.  If 'cdata' is an
        array then 'maxlen' defaults to its length.
        
        If 'cdata' is a pointer or array of wchar_t, returns a unicode string
        following the same rules.
        
        If 'cdata' is a single character or byte or a wchar_t, returns it as a
        string or unicode string.
        
        If 'cdata' is an enum, returns the value of the enumerator as a
        string, or 'NUMBER' if the value is out of range.
        """
        pass

    def typeof(self, *args, **kwargs): # real signature unknown
        """
        Parse the C type given as a string and return the
        corresponding <ctype> object.
        It can also be used on 'cdata' instance to get its C type.
        """
        pass

    def unpack(self, *args, **kwargs): # real signature unknown
        """
        Unpack an array of C data of the given length,
        returning a Python string/unicode/list.
        
        If 'cdata' is a pointer to 'char', returns a byte string.
        It does not stop at the first null.  This is equivalent to:
        ffi.buffer(cdata, length)[:]
        
        If 'cdata' is a pointer to 'wchar_t', returns a unicode string.
        'length' is measured in wchar_t's; it is not the size in bytes.
        
        If 'cdata' is a pointer to anything else, returns a list of
        'length' items.  This is a faster equivalent to:
        [cdata[i] for i in range(length)]
        """
        pass

    def __getattribute__(self, *args, **kwargs): # real signature unknown
        """ Return getattr(self, name). """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    errno = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """the value of 'errno' from/to the C calls"""


    buffer = buffer
    CData = None # (!) real value is "<class '_cffi_backend.CData'>"
    CType = None # (!) real value is "<class '_cffi_backend.CTypeDescr'>"
    error = None # (!) real value is "<class 'ffi.error'>"
    RTLD_GLOBAL = 0
    RTLD_LAZY = 0
    RTLD_LOCAL = 0
    RTLD_NOW = 0


class Lib(object):
    # no doc
    def __delattr__(self, *args, **kwargs): # real signature unknown
        """ Implement delattr(self, name). """
        pass

    def __dir__(self, *args, **kwargs): # real signature unknown
        pass

    def __getattribute__(self, *args, **kwargs): # real signature unknown
        """ Return getattr(self, name). """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setattr__(self, *args, **kwargs): # real signature unknown
        """ Implement setattr(self, name, value). """
        pass


# variables with complex values

_C_API = None # (!) real value is '<capsule object "cffi" at 0x03202740>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x02EB70B0>'

__spec__ = None # (!) real value is "ModuleSpec(name='_cffi_backend', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x02EB70B0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\_cffi_backend.cp37-win32.pyd')"

