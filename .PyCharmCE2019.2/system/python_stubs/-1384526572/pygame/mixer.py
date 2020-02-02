# encoding: utf-8
# module pygame.mixer
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\mixer.cp37-win32.pyd
# by generator 1.147
""" pygame module for loading and playing sounds """

# imports
import pygame.mixer_music as music # C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\mixer_music.cp37-win32.pyd

# functions

def Channel(id): # real signature unknown; restored from __doc__
    """
    Channel(id) -> Channel
    Create a Channel object for controlling playback
    """
    pass

def fadeout(time): # real signature unknown; restored from __doc__
    """
    fadeout(time) -> None
    fade out the volume on all sounds before stopping
    """
    pass

def find_channel(force=False): # real signature unknown; restored from __doc__
    """
    find_channel(force=False) -> Channel
    find an unused channel
    """
    pass

def get_busy(): # real signature unknown; restored from __doc__
    """
    get_busy() -> bool
    test if any sound is being mixed
    """
    return False

def get_init(): # real signature unknown; restored from __doc__
    """
    get_init() -> (frequency, format, channels)
    test if the mixer is initialized
    """
    pass

def get_num_channels(): # real signature unknown; restored from __doc__
    """
    get_num_channels() -> count
    get the total number of playback channels
    """
    pass

def init(frequency=22050, size=-16, channels=2, buffer=4096, devicename=None, allowedchanges=None, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
    """
    init(frequency=22050, size=-16, channels=2, buffer=4096, devicename=None, allowedchanges=AUDIO_ALLOW_FREQUENCY_CHANGE | AUDIO_ALLOW_CHANNELS_CHANGE) -> None
    initialize the mixer module
    """
    pass

def pause(): # real signature unknown; restored from __doc__
    """
    pause() -> None
    temporarily stop playback of all sound channels
    """
    pass

def pre_init(frequency=22050, size=-16, channels=2, buffersize=4096, devicename=None): # real signature unknown; restored from __doc__
    """
    pre_init(frequency=22050, size=-16, channels=2, buffersize=4096, devicename=None) -> None
    preset the mixer init arguments
    """
    pass

def quit(): # real signature unknown; restored from __doc__
    """
    quit() -> None
    uninitialize the mixer
    """
    pass

def set_num_channels(count): # real signature unknown; restored from __doc__
    """
    set_num_channels(count) -> None
    set the total number of playback channels
    """
    pass

def set_reserved(count): # real signature unknown; restored from __doc__
    """
    set_reserved(count) -> None
    reserve channels from being automatically used
    """
    pass

def stop(): # real signature unknown; restored from __doc__
    """
    stop() -> None
    stop playback of all sound channels
    """
    pass

def unpause(): # real signature unknown; restored from __doc__
    """
    unpause() -> None
    resume paused playback of sound channels
    """
    pass

def __PYGAMEinit__(*args, **kwargs): # real signature unknown
    """ auto initialize for mixer """
    pass

# classes

class ChannelType(object):
    """
    Channel(id) -> Channel
    Create a Channel object for controlling playback
    """
    def fadeout(self, time): # real signature unknown; restored from __doc__
        """
        fadeout(time) -> None
        stop playback after fading channel out
        """
        pass

    def get_busy(self): # real signature unknown; restored from __doc__
        """
        get_busy() -> bool
        check if the channel is active
        """
        return False

    def get_endevent(self): # real signature unknown; restored from __doc__
        """
        get_endevent() -> type
        get the event a channel sends when playback stops
        """
        return type(*(), **{})

    def get_queue(self): # real signature unknown; restored from __doc__
        """
        get_queue() -> Sound
        return any Sound that is queued
        """
        return Sound

    def get_sound(self): # real signature unknown; restored from __doc__
        """
        get_sound() -> Sound
        get the currently playing Sound
        """
        return Sound

    def get_volume(self): # real signature unknown; restored from __doc__
        """
        get_volume() -> value
        get the volume of the playing channel
        """
        pass

    def pause(self): # real signature unknown; restored from __doc__
        """
        pause() -> None
        temporarily stop playback of a channel
        """
        pass

    def play(self, Sound, loops=0, maxtime=0, fade_ms=0): # real signature unknown; restored from __doc__
        """
        play(Sound, loops=0, maxtime=0, fade_ms=0) -> None
        play a Sound on a specific Channel
        """
        pass

    def queue(self, Sound): # real signature unknown; restored from __doc__
        """
        queue(Sound) -> None
        queue a Sound object to follow the current
        """
        pass

    def set_endevent(self): # real signature unknown; restored from __doc__
        """
        set_endevent() -> None
        set_endevent(type) -> None
        have the channel send an event when playback stops
        """
        pass

    def set_volume(self, value): # real signature unknown; restored from __doc__
        """
        set_volume(value) -> None
        set_volume(left, right) -> None
        set the volume of a playing channel
        """
        pass

    def stop(self): # real signature unknown; restored from __doc__
        """
        stop() -> None
        stop playback on a Channel
        """
        pass

    def unpause(self): # real signature unknown; restored from __doc__
        """
        unpause() -> None
        resume pause playback of a channel
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass


class SoundType(object):
    """
    Sound(filename) -> Sound
    Sound(file=filename) -> Sound
    Sound(buffer) -> Sound
    Sound(buffer=buffer) -> Sound
    Sound(object) -> Sound
    Sound(file=object) -> Sound
    Sound(array=object) -> Sound
    Create a new Sound object from a file or buffer object
    """
    def fadeout(self, time): # real signature unknown; restored from __doc__
        """
        fadeout(time) -> None
        stop sound playback after fading out
        """
        pass

    def get_length(self): # real signature unknown; restored from __doc__
        """
        get_length() -> seconds
        get the length of the Sound
        """
        pass

    def get_num_channels(self): # real signature unknown; restored from __doc__
        """
        get_num_channels() -> count
        count how many times this Sound is playing
        """
        pass

    def get_raw(self): # real signature unknown; restored from __doc__
        """
        get_raw() -> bytes
        return a bytestring copy of the Sound samples.
        """
        return b""

    def get_volume(self): # real signature unknown; restored from __doc__
        """
        get_volume() -> value
        get the playback volume
        """
        pass

    def play(self, loops=0, maxtime=0, fade_ms=0): # real signature unknown; restored from __doc__
        """
        play(loops=0, maxtime=0, fade_ms=0) -> Channel
        begin sound playback
        """
        pass

    def set_volume(self, value): # real signature unknown; restored from __doc__
        """
        set_volume(value) -> None
        set the playback volume for this Sound
        """
        pass

    def stop(self): # real signature unknown; restored from __doc__
        """
        stop() -> None
        stop sound playback
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    _samples_address = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """samples buffer address (readonly)"""

    __array_interface__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """Version 3"""

    __array_struct__ = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default
    """Version 3"""



Sound = SoundType


# variables with complex values

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.mixer._PYGAME_C_API" at 0x02F1FF68>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x031C6C50>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.mixer', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x031C6C50>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\mixer.cp37-win32.pyd')"

