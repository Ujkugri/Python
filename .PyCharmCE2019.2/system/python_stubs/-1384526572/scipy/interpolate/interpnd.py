# encoding: utf-8
# module scipy.interpolate.interpnd
# from C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\interpolate\interpnd.cp37-win32.pyd
# by generator 1.147
"""
Simple N-D interpolation

.. versionadded:: 0.9
"""

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import numpy as np # C:\Program Files (x86)\Python37-32\lib\site-packages\numpy\__init__.py
import scipy.spatial.qhull as qhull # C:\Program Files (x86)\Python37-32\lib\site-packages\scipy\spatial\qhull.cp37-win32.pyd
import warnings as warnings # C:\Program Files (x86)\Python37-32\lib\warnings.py

# functions

def estimate_gradients_2d_global(*args, **kwargs): # real signature unknown
    pass

def _ndim_coords_from_arrays(*args, **kwargs): # real signature unknown
    """ Convert a tuple of coordinate arrays to a (..., ndim)-shaped array. """
    pass

def __pyx_unpickle_Enum(*args, **kwargs): # real signature unknown
    pass

# classes

class NDInterpolatorBase(object):
    """
    Common routines for interpolators.
    
        .. versionadded:: 0.9
    """
    def _check_call_shape(self, *args, **kwargs): # real signature unknown
        pass

    def _scale_x(self, *args, **kwargs): # real signature unknown
        pass

    def __call__(self, *args, **kwargs): # real signature unknown
        """
        interpolator(xi)
        
                Evaluate interpolator at given points.
        
                Parameters
                ----------
                xi : ndarray of float, shape (..., ndim)
                    Points where to interpolate data at.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        """
        Check shape of points and values arrays, and reshape values to
                (npoints, nvalues).  Ensure the `points` and values arrays are
                C-contiguous, and of correct type.
        """
        pass

    __weakref__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """list of weak references to the object (if defined)"""


    __dict__ = None # (!) real value is "mappingproxy({'__module__': 'scipy.interpolate.interpnd', '__doc__': '\\n    Common routines for interpolators.\\n\\n    .. versionadded:: 0.9\\n\\n    ', '__init__': <cyfunction NDInterpolatorBase.__init__ at 0x0F87E648>, '_check_call_shape': <cyfunction NDInterpolatorBase._check_call_shape at 0x0F87E6B0>, '_scale_x': <cyfunction NDInterpolatorBase._scale_x at 0x0F87E718>, '__call__': <cyfunction NDInterpolatorBase.__call__ at 0x0F87E780>, '__dict__': <attribute '__dict__' of 'NDInterpolatorBase' objects>, '__weakref__': <attribute '__weakref__' of 'NDInterpolatorBase' objects>})"


class CloughTocher2DInterpolator(NDInterpolatorBase):
    """
    CloughTocher2DInterpolator(points, values, tol=1e-6)
    
        Piecewise cubic, C1 smooth, curvature-minimizing interpolant in 2D.
    
        .. versionadded:: 0.9
    
        Methods
        -------
        __call__
    
        Parameters
        ----------
        points : ndarray of floats, shape (npoints, ndims); or Delaunay
            Data point coordinates, or a precomputed Delaunay triangulation.
        values : ndarray of float or complex, shape (npoints, ...)
            Data values.
        fill_value : float, optional
            Value used to fill in for requested points outside of the
            convex hull of the input points.  If not provided, then
            the default is ``nan``.
        tol : float, optional
            Absolute/relative tolerance for gradient estimation.
        maxiter : int, optional
            Maximum number of iterations in gradient estimation.
        rescale : bool, optional
            Rescale points to unit cube before performing interpolation.
            This is useful if some of the input dimensions have
            incommensurable units and differ by many orders of magnitude.
    
        Notes
        -----
        The interpolant is constructed by triangulating the input data
        with Qhull [1]_, and constructing a piecewise cubic
        interpolating Bezier polynomial on each triangle, using a
        Clough-Tocher scheme [CT]_.  The interpolant is guaranteed to be
        continuously differentiable.
    
        The gradients of the interpolant are chosen so that the curvature
        of the interpolating surface is approximatively minimized. The
        gradients necessary for this are estimated using the global
        algorithm described in [Nielson83,Renka84]_.
    
        References
        ----------
        .. [1] http://www.qhull.org/
    
        .. [CT] See, for example,
           P. Alfeld,
           ''A trivariate Clough-Tocher scheme for tetrahedral data''.
           Computer Aided Geometric Design, 1, 169 (1984);
           G. Farin,
           ''Triangular Bernstein-Bezier patches''.
           Computer Aided Geometric Design, 3, 83 (1986).
    
        .. [Nielson83] G. Nielson,
           ''A method for interpolating scattered data based upon a minimum norm
           network''.
           Math. Comp., 40, 253 (1983).
    
        .. [Renka84] R. J. Renka and A. K. Cline.
           ''A Triangle-based C1 interpolation method.'',
           Rocky Mountain J. Math., 14, 223 (1984).
    """
    def _do_evaluate(self, *args, **kwargs): # real signature unknown
        pass

    def _evaluate_complex(self, *args, **kwargs): # real signature unknown
        pass

    def _evaluate_double(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass


class GradientEstimationWarning(Warning):
    # no doc
    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    __weakref__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """list of weak references to the object (if defined)"""



class LinearNDInterpolator(NDInterpolatorBase):
    """
    LinearNDInterpolator(points, values, fill_value=np.nan, rescale=False)
    
        Piecewise linear interpolant in N dimensions.
    
        .. versionadded:: 0.9
    
        Methods
        -------
        __call__
    
        Parameters
        ----------
        points : ndarray of floats, shape (npoints, ndims); or Delaunay
            Data point coordinates, or a precomputed Delaunay triangulation.
        values : ndarray of float or complex, shape (npoints, ...)
            Data values.
        fill_value : float, optional
            Value used to fill in for requested points outside of the
            convex hull of the input points.  If not provided, then
            the default is ``nan``.
        rescale : bool, optional
            Rescale points to unit cube before performing interpolation.
            This is useful if some of the input dimensions have
            incommensurable units and differ by many orders of magnitude.
    
        Notes
        -----
        The interpolant is constructed by triangulating the input data
        with Qhull [1]_, and on each triangle performing linear
        barycentric interpolation.
    
        References
        ----------
        .. [1] http://www.qhull.org/
    """
    def _do_evaluate(self, *args, **kwargs): # real signature unknown
        pass

    def _evaluate_complex(self, *args, **kwargs): # real signature unknown
        pass

    def _evaluate_double(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0F7B1ED0>'

__spec__ = None # (!) real value is "ModuleSpec(name='scipy.interpolate.interpnd', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0F7B1ED0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\scipy\\\\interpolate\\\\interpnd.cp37-win32.pyd')"

__test__ = {}

