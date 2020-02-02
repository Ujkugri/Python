# encoding: utf-8
# module matplotlib.ft2font
# from C:\Program Files (x86)\Python37-32\lib\site-packages\matplotlib\ft2font.cp37-win32.pyd
# by generator 1.147
# no doc
# no imports

# Variables with simple values

BOLD = 2

EXTERNAL_STREAM = 1024

FAST_GLYPHS = 128

FIXED_SIZES = 2
FIXED_WIDTH = 4

GLYPH_NAMES = 512

HORIZONTAL = 16

ITALIC = 1

KERNING = 64

KERNING_DEFAULT = 0
KERNING_UNFITTED = 1
KERNING_UNSCALED = 2

LOAD_CROP_BITMAP = 64

LOAD_DEFAULT = 0

LOAD_FORCE_AUTOHINT = 32

LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH = 512

LOAD_IGNORE_TRANSFORM = 2048

LOAD_LINEAR_DESIGN = 8192

LOAD_MONOCHROME = 4096

LOAD_NO_AUTOHINT = 32768
LOAD_NO_BITMAP = 8
LOAD_NO_HINTING = 2
LOAD_NO_RECURSE = 1024
LOAD_NO_SCALE = 1

LOAD_PEDANTIC = 128
LOAD_RENDER = 4

LOAD_TARGET_LCD = 196608

LOAD_TARGET_LCD_V = 262144

LOAD_TARGET_LIGHT = 65536
LOAD_TARGET_MONO = 131072
LOAD_TARGET_NORMAL = 0

LOAD_VERTICAL_LAYOUT = 16

MULTIPLE_MASTERS = 256

SCALABLE = 1

SFNT = 8

VERTICAL = 32

__freetype_build_type__ = 'system'

__freetype_version__ = '2.9.1'

# no functions
# classes

class FT2Font(object):
    """
    FT2Font(ttffile)
    
    Create a new FT2Font object
    The following global font attributes are defined:
      num_faces              number of faces in file
      face_flags             face flags  (int type); see the ft2font constants
      style_flags            style flags  (int type); see the ft2font constants
      num_glyphs             number of glyphs in the face
      family_name            face family name
      style_name             face style name
      num_fixed_sizes        number of bitmap in the face
      scalable               face is scalable
    
    The following are available, if scalable is true:
      bbox                   face global bounding box (xmin, ymin, xmax, ymax)
      units_per_EM           number of font units covered by the EM
      ascender               ascender in 26.6 units
      descender              descender in 26.6 units
      height                 height in 26.6 units; used to compute a default
                             line spacing (baseline-to-baseline distance)
      max_advance_width      maximum horizontal cursor advance for all glyphs
      max_advance_height     same for vertical layout
      underline_position     vertical position of the underline bar
      underline_thickness    vertical thickness of the underline
      postscript_name        PostScript name of the font
    """
    def clear(self): # real signature unknown; restored from __doc__
        """
        clear()
        
        Clear all the glyphs, reset for a new set_text
        """
        pass

    def draw_glyphs_to_bitmap(self): # real signature unknown; restored from __doc__
        """
        draw_glyphs_to_bitmap()
        
        Draw the glyphs that were loaded by set_text to the bitmap
        The bitmap size will be automatically set to include the glyphs
        """
        pass

    def draw_glyph_to_bitmap(self, bitmap, x, y, glyph): # real signature unknown; restored from __doc__
        """
        draw_glyph_to_bitmap(bitmap, x, y, glyph)
        
        Draw a single glyph to the bitmap at pixel locations x,y
        Note it is your responsibility to set up the bitmap manually
        with set_bitmap_size(w,h) before this call is made.
        
        If you want automatic layout, use set_text in combinations with
        draw_glyphs_to_bitmap.  This function is intended for people who
        want to render individual glyphs at precise locations, eg, a
        a glyph returned by load_char
        """
        pass

    def get_bitmap_offset(self): # real signature unknown; restored from __doc__
        """
        x, y = get_bitmap_offset()
        
        Get the offset in 26.6 subpixels for the bitmap if ink hangs left or below (0, 0).
        Since matplotlib only supports left-to-right text, y is always 0.
        """
        pass

    def get_charmap(self): # real signature unknown; restored from __doc__
        """
        get_charmap()
        
        Returns a dictionary that maps the character codes of the selected charmap
        (Unicode by default) to their corresponding glyph indices.
        """
        pass

    def get_char_index(self): # real signature unknown; restored from __doc__
        """
        get_char_index()
        
        Given a character code, returns a glyph index.
        """
        pass

    def get_descent(self): # real signature unknown; restored from __doc__
        """
        d = get_descent()
        
        Get the descent of the current string set by set_text in 26.6 subpixels.
        The rotation of the string is accounted for.  To get the descent
        in pixels, divide this value by 64.
        """
        pass

    def get_glyph_name(self, index): # real signature unknown; restored from __doc__
        """
        get_glyph_name(index)
        
        Retrieves the ASCII name of a given glyph in a face.
        """
        pass

    def get_image(self, *args, **kwargs): # real signature unknown
        """
        get_path()
        
        Get the path data from the currently loaded glyph as a tuple of vertices, codes.
        """
        pass

    def get_kerning(self, left, right, mode): # real signature unknown; restored from __doc__
        """
        dx = get_kerning(left, right, mode)
        
        Get the kerning between left char and right glyph indices
        mode is a kerning mode constant
          KERNING_DEFAULT  - Return scaled and grid-fitted kerning distances
          KERNING_UNFITTED - Return scaled but un-grid-fitted kerning distances
          KERNING_UNSCALED - Return the kerning vector in original font units
        """
        pass

    def get_name_index(self, name): # real signature unknown; restored from __doc__
        """
        get_name_index(name)
        
        Returns the glyph index of a given glyph name.
        The glyph index 0 means `undefined character code'.
        """
        pass

    def get_num_glyphs(self): # real signature unknown; restored from __doc__
        """
        get_num_glyphs()
        
        Return the number of loaded glyphs
        """
        pass

    def get_path(self): # real signature unknown; restored from __doc__
        """
        get_path()
        
        Get the path data from the currently loaded glyph as a tuple of vertices, codes.
        """
        pass

    def get_ps_font_info(self): # real signature unknown; restored from __doc__
        """
        get_ps_font_info()
        
        Return the information in the PS Font Info structure.
        """
        pass

    def get_sfnt(self, name): # real signature unknown; restored from __doc__
        """
        get_sfnt(name)
        
        Get all values from the SFNT names table.  Result is a dictionary whosekey is the platform-ID, ISO-encoding-scheme, language-code, anddescription.
        """
        pass

    def get_sfnt_table(self, name): # real signature unknown; restored from __doc__
        """
        get_sfnt_table(name)
        
        Return one of the following SFNT tables: head, maxp, OS/2, hhea, vhea, post, or pclt.
        """
        pass

    def get_width_height(self): # real signature unknown; restored from __doc__
        """
        w, h = get_width_height()
        
        Get the width and height in 26.6 subpixels of the current string set by set_text
        The rotation of the string is accounted for.  To get width and height
        in pixels, divide these values by 64
        """
        pass

    def get_xys(self): # real signature unknown; restored from __doc__
        """
        get_xys()
        
        Get the xy locations of the current glyphs
        """
        pass

    def load_char(self, charcode, flags=None): # real signature unknown; restored from __doc__
        """
        load_char(charcode, flags=LOAD_FORCE_AUTOHINT)
        
        Load character with charcode in current fontfile and set glyph.
        The flags argument can be a bitwise-or of the LOAD_XXX constants.
        Return value is a Glyph object, with attributes
          width          # glyph width
          height         # glyph height
          bbox           # the glyph bbox (xmin, ymin, xmax, ymax)
          horiBearingX   # left side bearing in horizontal layouts
          horiBearingY   # top side bearing in horizontal layouts
          horiAdvance    # advance width for horizontal layout
          vertBearingX   # left side bearing in vertical layouts
          vertBearingY   # top side bearing in vertical layouts
          vertAdvance    # advance height for vertical layout
        """
        pass

    def load_glyph(self, glyphindex, flags=None): # real signature unknown; restored from __doc__
        """
        load_glyph(glyphindex, flags=LOAD_FORCE_AUTOHINT)
        
        Load character with glyphindex in current fontfile and set glyph.
        The flags argument can be a bitwise-or of the LOAD_XXX constants.
        Return value is a Glyph object, with attributes
          width          # glyph width
          height         # glyph height
          bbox           # the glyph bbox (xmin, ymin, xmax, ymax)
          horiBearingX   # left side bearing in horizontal layouts
          horiBearingY   # top side bearing in horizontal layouts
          horiAdvance    # advance width for horizontal layout
          vertBearingX   # left side bearing in vertical layouts
          vertBearingY   # top side bearing in vertical layouts
          vertAdvance    # advance height for vertical layout
        """
        pass

    def select_charmap(self, i): # real signature unknown; restored from __doc__
        """
        select_charmap(i)
        
        select charmap i where i is one of the FT_Encoding number
        """
        pass

    def set_charmap(self, i): # real signature unknown; restored from __doc__
        """
        set_charmap(i)
        
        Make the i-th charmap current
        """
        pass

    def set_size(self, ptsize, dpi): # real signature unknown; restored from __doc__
        """
        set_size(ptsize, dpi)
        
        Set the point size and dpi of the text.
        """
        pass

    def set_text(self, s, angle): # real signature unknown; restored from __doc__
        """
        set_text(s, angle)
        
        Set the text string and angle.
        You must call this before draw_glyphs_to_bitmap
        A sequence of x,y positions is returned
        """
        pass

    def __init__(self, ttffile): # real signature unknown; restored from __doc__
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    ascender = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    bbox = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    descender = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    face_flags = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    family_name = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    fname = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    height = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    max_advance_height = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    max_advance_width = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    num_charmaps = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    num_faces = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    num_fixed_sizes = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    num_glyphs = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    postscript_name = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    scalable = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    style_flags = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    style_name = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    underline_position = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    underline_thickness = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    units_per_EM = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default



class FT2Image(object):
    # no doc
    def as_array(self): # real signature unknown; restored from __doc__
        """
        x = image.as_array()
        
        Return the image buffer as a width x height numpy array of ubyte
        """
        pass

    def as_rgba_str(self): # real signature unknown; restored from __doc__
        """
        s = image.as_rgba_str()
        
        Return the image buffer as a RGBA string
        """
        pass

    def as_str(self): # real signature unknown; restored from __doc__
        """
        s = image.as_str()
        
        Return the image buffer as a string
        """
        pass

    def draw_rect(self, x0, y0, x1, y1): # real signature unknown; restored from __doc__
        """
        draw_rect(x0, y0, x1, y1)
        
        Draw a rect to the image.
        """
        pass

    def draw_rect_filled(self, x0, y0, x1, y1): # real signature unknown; restored from __doc__
        """
        draw_rect_filled(x0, y0, x1, y1)
        
        Draw a filled rect to the image.
        """
        pass

    def get_height(self, *args, **kwargs): # real signature unknown
        pass

    def get_width(self, *args, **kwargs): # real signature unknown
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x02DF8170>'

__spec__ = None # (!) real value is "ModuleSpec(name='matplotlib.ft2font', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x02DF8170>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\matplotlib\\\\ft2font.cp37-win32.pyd')"

