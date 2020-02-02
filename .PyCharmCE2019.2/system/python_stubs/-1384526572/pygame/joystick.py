# encoding: utf-8
# module pygame.joystick
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\joystick.cp37-win32.pyd
# by generator 1.147
""" Pygame module for interacting with joysticks, gamepads, and trackballs. """
# no imports

# functions

def get_count(): # real signature unknown; restored from __doc__
    """
    get_count() -> count
    Returns the number of joysticks.
    """
    pass

def get_init(): # real signature unknown; restored from __doc__
    """
    get_init() -> bool
    Returns True if the joystick module is initialized.
    """
    return False

def init(): # real signature unknown; restored from __doc__
    """
    init() -> None
    Initialize the joystick module.
    """
    pass

def Joystick(id): # real signature unknown; restored from __doc__
    """
    Joystick(id) -> Joystick
    Create a new Joystick object.
    """
    pass

def quit(): # real signature unknown; restored from __doc__
    """
    quit() -> None
    Uninitialize the joystick module.
    """
    pass

def __PYGAMEinit__(*args, **kwargs): # real signature unknown
    """ auto initialize function for joystick """
    pass

# classes

class JoystickType(object):
    """
    Joystick(id) -> Joystick
    Create a new Joystick object.
    """
    def get_axis(self, axis_number): # real signature unknown; restored from __doc__
        """
        get_axis(axis_number) -> float
        get the current position of an axis
        """
        return 0.0

    def get_ball(self, ball_number): # real signature unknown; restored from __doc__
        """
        get_ball(ball_number) -> x, y
        get the relative position of a trackball
        """
        pass

    def get_button(self, button): # real signature unknown; restored from __doc__
        """
        get_button(button) -> bool
        get the current button state
        """
        return False

    def get_hat(self, hat_number): # real signature unknown; restored from __doc__
        """
        get_hat(hat_number) -> x, y
        get the position of a joystick hat
        """
        pass

    def get_id(self): # real signature unknown; restored from __doc__
        """
        get_id() -> int
        get the Joystick ID
        """
        return 0

    def get_init(self): # real signature unknown; restored from __doc__
        """
        get_init() -> bool
        check if the Joystick is initialized
        """
        return False

    def get_name(self): # real signature unknown; restored from __doc__
        """
        get_name() -> string
        get the Joystick system name
        """
        return ""

    def get_numaxes(self): # real signature unknown; restored from __doc__
        """
        get_numaxes() -> int
        get the number of axes on a Joystick
        """
        return 0

    def get_numballs(self): # real signature unknown; restored from __doc__
        """
        get_numballs() -> int
        get the number of trackballs on a Joystick
        """
        return 0

    def get_numbuttons(self): # real signature unknown; restored from __doc__
        """
        get_numbuttons() -> int
        get the number of buttons on a Joystick
        """
        return 0

    def get_numhats(self): # real signature unknown; restored from __doc__
        """
        get_numhats() -> int
        get the number of hat controls on a Joystick
        """
        return 0

    def init(self): # real signature unknown; restored from __doc__
        """
        init() -> None
        initialize the Joystick
        """
        pass

    def quit(self): # real signature unknown; restored from __doc__
        """
        quit() -> None
        uninitialize the Joystick
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass


# variables with complex values

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.joystick._PYGAME_C_API" at 0x00FCF1B8>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x02E29E10>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.joystick', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x02E29E10>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\joystick.cp37-win32.pyd')"

