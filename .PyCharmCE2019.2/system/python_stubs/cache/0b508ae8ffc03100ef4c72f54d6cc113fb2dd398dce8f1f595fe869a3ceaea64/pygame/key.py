# encoding: utf-8
# module pygame.key
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\key.cp37-win32.pyd
# by generator 1.147
""" pygame module to work with the keyboard """
# no imports

# functions

def get_focused(): # real signature unknown; restored from __doc__
    """
    get_focused() -> bool
    true if the display is receiving keyboard input from the system
    """
    return False

def get_mods(): # real signature unknown; restored from __doc__
    """
    get_mods() -> int
    determine which modifier keys are being held
    """
    return 0

def get_pressed(): # real signature unknown; restored from __doc__
    """
    get_pressed() -> bools
    get the state of all keyboard buttons
    """
    pass

def get_repeat(): # real signature unknown; restored from __doc__
    """
    get_repeat() -> (delay, interval)
    see how held keys are repeated
    """
    pass

def name(key): # real signature unknown; restored from __doc__
    """
    name(key) -> string
    get the name of a key identifier
    """
    return ""

def set_mods(p_int): # real signature unknown; restored from __doc__
    """
    set_mods(int) -> None
    temporarily set which modifier keys are pressed
    """
    pass

def set_repeat(): # real signature unknown; restored from __doc__
    """
    set_repeat() -> None
    set_repeat(delay, interval) -> None
    control how held keys are repeated
    """
    pass

def set_text_input_rect(Rect): # real signature unknown; restored from __doc__
    """
    set_text_input_rect(Rect) -> None
    controls the position of the candidate list
    """
    pass

def start_text_input(): # real signature unknown; restored from __doc__
    """
    start_text_input() -> None
    start handling IME compositions
    """
    pass

def stop_text_input(): # real signature unknown; restored from __doc__
    """
    stop_text_input() -> None
    stop handling IME compositions
    """
    pass

# no classes
# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x03369ED0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.key', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x03369ED0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\key.cp37-win32.pyd')"

