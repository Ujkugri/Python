# encoding: utf-8
# module matplotlib._tri
# from C:\Program Files (x86)\Python37-32\lib\site-packages\matplotlib\_tri.cp37-win32.pyd
# by generator 1.147
# no doc
# no imports

# no functions
# classes

class TrapezoidMapTriFinder(object):
    """
    TrapezoidMapTriFinder(triangulation)
    
    Create a new C++ TrapezoidMapTriFinder object
    This should not be called directly, instead use the python class
    matplotlib.tri.TrapezoidMapTriFinder instead.
    """
    def find_many(self, x, y): # real signature unknown; restored from __doc__
        """
        find_many(x, y)
        
        Find indices of triangles containing the point coordinates (x, y)
        """
        pass

    def get_tree_stats(self): # real signature unknown; restored from __doc__
        """
        get_tree_stats()
        
        Return statistics about the tree used by the trapezoid map
        """
        pass

    def initialize(self): # real signature unknown; restored from __doc__
        """
        initialize()
        
        Initialize this object, creating the trapezoid map from the triangulation
        """
        pass

    def print_tree(self): # real signature unknown; restored from __doc__
        """
        print_tree()
        
        Print the search tree as text to stdout; useful for debug purposes
        """
        pass

    def __init__(self, triangulation): # real signature unknown; restored from __doc__
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass


class Triangulation(object):
    """
    Triangulation(x, y, triangles, mask, edges, neighbors)
    
    Create a new C++ Triangulation object
    This should not be called directly, instead use the python class
    matplotlib.tri.Triangulation instead.
    """
    def calculate_plane_coefficients(self, z, plane_coefficients): # real signature unknown; restored from __doc__
        """
        calculate_plane_coefficients(z, plane_coefficients)
        
        Calculate plane equation coefficients for all unmasked triangles
        """
        pass

    def get_edges(self): # real signature unknown; restored from __doc__
        """
        get_edges()
        
        Return edges array
        """
        pass

    def get_neighbors(self): # real signature unknown; restored from __doc__
        """
        get_neighbors()
        
        Return neighbors array
        """
        pass

    def set_mask(self, mask): # real signature unknown; restored from __doc__
        """
        set_mask(mask)
        
        Set or clear the mask array.
        """
        pass

    def __init__(self, x, y, triangles, mask, edges, neighbors): # real signature unknown; restored from __doc__
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass


class TriContourGenerator(object):
    """
    TriContourGenerator(triangulation, z)
    
    Create a new C++ TriContourGenerator object
    This should not be called directly, instead use the functions
    matplotlib.axes.tricontour and tricontourf instead.
    """
    def create_contour(self, level): # real signature unknown; restored from __doc__
        """
        create_contour(level)
        
        Create and return a non-filled contour.
        """
        pass

    def create_filled_contour(self, lower_level, upper_level): # real signature unknown; restored from __doc__
        """
        create_filled_contour(lower_level, upper_level)
        
        Create and return a filled contour
        """
        pass

    def __init__(self, triangulation, z): # real signature unknown; restored from __doc__
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x032C01F0>'

__spec__ = None # (!) real value is "ModuleSpec(name='matplotlib._tri', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x032C01F0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\matplotlib\\\\_tri.cp37-win32.pyd')"

