# encoding: utf-8
# module pygame.cdrom
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\cdrom.cp37-win32.pyd
# by generator 1.147
""" pygame module for audio cdrom control """
# no imports

# functions

def CD(id): # real signature unknown; restored from __doc__
    """
    CD(id) -> CD
    class to manage a cdrom drive
    """
    pass

def get_count(): # real signature unknown; restored from __doc__
    """
    get_count() -> count
    number of cd drives on the system
    """
    pass

def get_init(): # real signature unknown; restored from __doc__
    """
    get_init() -> bool
    true if the cdrom module is initialized
    """
    return False

def init(): # real signature unknown; restored from __doc__
    """
    init() -> None
    initialize the cdrom module
    """
    pass

def quit(): # real signature unknown; restored from __doc__
    """
    quit() -> None
    uninitialize the cdrom module
    """
    pass

def __PYGAMEinit__(*args, **kwargs): # real signature unknown
    """ auto initialize function """
    pass

# classes

class CDType(object):
    """
    CD(id) -> CD
    class to manage a cdrom drive
    """
    def eject(self): # real signature unknown; restored from __doc__
        """
        eject() -> None
        eject or open the cdrom drive
        """
        pass

    def get_all(self): # real signature unknown; restored from __doc__
        """
        get_all() -> [(audio, start, end, length), ...]
        get all track information
        """
        pass

    def get_busy(self): # real signature unknown; restored from __doc__
        """
        get_busy() -> bool
        true if the drive is playing audio
        """
        return False

    def get_current(self): # real signature unknown; restored from __doc__
        """
        get_current() -> track, seconds
        the current audio playback position
        """
        pass

    def get_empty(self): # real signature unknown; restored from __doc__
        """
        get_empty() -> bool
        False if a cdrom is in the drive
        """
        return False

    def get_id(self, *args, **kwargs): # real signature unknown
        """
        get_init() -> bool
        true if this cd device initialized
        """
        pass

    def get_init(self): # real signature unknown; restored from __doc__
        """
        get_init() -> bool
        true if this cd device initialized
        """
        return False

    def get_name(self): # real signature unknown; restored from __doc__
        """
        get_name() -> name
        the system name of the cdrom drive
        """
        pass

    def get_numtracks(self): # real signature unknown; restored from __doc__
        """
        get_numtracks() -> count
        the number of tracks on the cdrom
        """
        pass

    def get_paused(self): # real signature unknown; restored from __doc__
        """
        get_paused() -> bool
        true if the drive is paused
        """
        return False

    def get_track_audio(self, track): # real signature unknown; restored from __doc__
        """
        get_track_audio(track) -> bool
        true if the cdrom track has audio data
        """
        return False

    def get_track_length(self, track): # real signature unknown; restored from __doc__
        """
        get_track_length(track) -> seconds
        length of a cdrom track
        """
        pass

    def get_track_start(self, track): # real signature unknown; restored from __doc__
        """
        get_track_start(track) -> seconds
        start time of a cdrom track
        """
        pass

    def init(self): # real signature unknown; restored from __doc__
        """
        init() -> None
        initialize a cdrom drive for use
        """
        pass

    def pause(self): # real signature unknown; restored from __doc__
        """
        pause() -> None
        temporarily stop audio playback
        """
        pass

    def play(self, *args, **kwargs): # real signature unknown
        """
        init() -> None
        initialize a cdrom drive for use
        """
        pass

    def quit(self): # real signature unknown; restored from __doc__
        """
        quit() -> None
        uninitialize a cdrom drive for use
        """
        pass

    def resume(self): # real signature unknown; restored from __doc__
        """
        resume() -> None
        unpause audio playback
        """
        pass

    def stop(self): # real signature unknown; restored from __doc__
        """
        stop() -> None
        stop audio playback
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass


# variables with complex values

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.cdrom._PYGAME_C_API" at 0x03DEF140>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x03E595F0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.cdrom', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x03E595F0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\cdrom.cp37-win32.pyd')"

