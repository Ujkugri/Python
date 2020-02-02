# encoding: utf-8
# module pygame.mixer_music
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\mixer_music.cp37-win32.pyd
# by generator 1.147
""" pygame module for controlling streamed audio """
# no imports

# functions

def fadeout(time): # real signature unknown; restored from __doc__
    """
    fadeout(time) -> None
    stop music playback after fading out
    """
    pass

def get_busy(): # real signature unknown; restored from __doc__
    """
    get_busy() -> bool
    check if the music stream is playing
    """
    return False

def get_endevent(): # real signature unknown; restored from __doc__
    """
    get_endevent() -> type
    get the event a channel sends when playback stops
    """
    return type(*(), **{})

def get_pos(): # real signature unknown; restored from __doc__
    """
    get_pos() -> time
    get the music play time
    """
    pass

def get_volume(): # real signature unknown; restored from __doc__
    """
    get_volume() -> value
    get the music volume
    """
    pass

def load(filename): # real signature unknown; restored from __doc__
    """
    load(filename) -> None
    load(object) -> None
    Load a music file for playback
    """
    pass

def pause(): # real signature unknown; restored from __doc__
    """
    pause() -> None
    temporarily stop music playback
    """
    pass

def play(loops=0, start=0.0): # real signature unknown; restored from __doc__
    """
    play(loops=0, start=0.0) -> None
    Start the playback of the music stream
    """
    pass

def queue(filename): # real signature unknown; restored from __doc__
    """
    queue(filename) -> None
    queue a music file to follow the current
    """
    pass

def rewind(): # real signature unknown; restored from __doc__
    """
    rewind() -> None
    restart music
    """
    pass

def set_endevent(): # real signature unknown; restored from __doc__
    """
    set_endevent() -> None
    set_endevent(type) -> None
    have the music send an event when playback stops
    """
    pass

def set_pos(pos): # real signature unknown; restored from __doc__
    """
    set_pos(pos) -> None
    set position to play from
    """
    pass

def set_volume(value): # real signature unknown; restored from __doc__
    """
    set_volume(value) -> None
    set the music volume
    """
    pass

def stop(): # real signature unknown; restored from __doc__
    """
    stop() -> None
    stop the music playback
    """
    pass

def unpause(): # real signature unknown; restored from __doc__
    """
    unpause() -> None
    resume paused music
    """
    pass

# no classes
# variables with complex values

_MUSIC_POINTER = None # (!) real value is '<capsule object "pygame.music_mixer._MUSIC_POINTER" at 0x032E25A8>'

_QUEUE_POINTER = None # (!) real value is '<capsule object "pygame.music_mixer._QUEUE_POINTER" at 0x02F5FF38>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x031F3130>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.mixer_music', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x031F3130>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\mixer_music.cp37-win32.pyd')"

