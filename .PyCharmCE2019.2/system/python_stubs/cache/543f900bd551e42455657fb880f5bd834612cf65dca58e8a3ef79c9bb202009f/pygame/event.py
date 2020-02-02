# encoding: utf-8
# module pygame.event
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\event.cp37-win32.pyd
# by generator 1.147
""" pygame module for interacting with events and queues """
# no imports

# functions

def clear(eventtype=None): # real signature unknown; restored from __doc__
    """
    clear(eventtype=None) -> None
    clear(eventtype=None, pump=True) -> None
    remove all events from the queue
    """
    pass

def Event(type, dict): # real signature unknown; restored from __doc__
    """
    Event(type, dict) -> EventType instance
    Event(type, **attributes) -> EventType instance
    create a new event object
    """
    return EventType

def event_name(type): # real signature unknown; restored from __doc__
    """
    event_name(type) -> string
    get the string name from an event id
    """
    return ""

def get(eventtype=None): # real signature unknown; restored from __doc__
    """
    get(eventtype=None) -> Eventlist
    get(eventtype=None, pump=True) -> Eventlist
    get events from the queue
    """
    pass

def get_blocked(type): # real signature unknown; restored from __doc__
    """
    get_blocked(type) -> bool
    test if a type of event is blocked from the queue
    """
    return False

def get_grab(): # real signature unknown; restored from __doc__
    """
    get_grab() -> bool
    test if the program is sharing input devices
    """
    return False

def peek(eventtype=None): # real signature unknown; restored from __doc__
    """
    peek(eventtype=None) -> bool
    peek(eventtype=None, pump=True) -> bool
    test if event types are waiting on the queue
    """
    return False

def poll(): # real signature unknown; restored from __doc__
    """
    poll() -> EventType instance
    get a single event from the queue
    """
    return EventType

def post(Event): # real signature unknown; restored from __doc__
    """
    post(Event) -> None
    place a new event on the queue
    """
    pass

def pump(): # real signature unknown; restored from __doc__
    """
    pump() -> None
    internally process pygame event handlers
    """
    pass

def set_allowed(type): # real signature unknown; restored from __doc__
    """
    set_allowed(type) -> None
    set_allowed(typelist) -> None
    set_allowed(None) -> None
    control which events are allowed on the queue
    """
    pass

def set_blocked(type): # real signature unknown; restored from __doc__
    """
    set_blocked(type) -> None
    set_blocked(typelist) -> None
    set_blocked(None) -> None
    control which events are allowed on the queue
    """
    pass

def set_grab(bool): # real signature unknown; restored from __doc__
    """
    set_grab(bool) -> None
    control the sharing of input devices with other applications
    """
    pass

def wait(): # real signature unknown; restored from __doc__
    """
    wait() -> EventType instance
    wait for a single event from the queue
    """
    return EventType

# classes

class EventType(object):
    """
    Event(type, dict) -> EventType instance
    Event(type, **attributes) -> EventType instance
    create a new event object
    """
    def __bool__(self, *args, **kwargs): # real signature unknown
        """ self != 0 """
        pass

    def __delattr__(self, *args, **kwargs): # real signature unknown
        """ Implement delattr(self, name). """
        pass

    def __eq__(self, *args, **kwargs): # real signature unknown
        """ Return self==value. """
        pass

    def __getattribute__(self, *args, **kwargs): # real signature unknown
        """ Return getattr(self, name). """
        pass

    def __ge__(self, *args, **kwargs): # real signature unknown
        """ Return self>=value. """
        pass

    def __gt__(self, *args, **kwargs): # real signature unknown
        """ Return self>value. """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        pass

    def __le__(self, *args, **kwargs): # real signature unknown
        """ Return self<=value. """
        pass

    def __lt__(self, *args, **kwargs): # real signature unknown
        """ Return self<value. """
        pass

    def __ne__(self, *args, **kwargs): # real signature unknown
        """ Return self!=value. """
        pass

    def __repr__(self, *args, **kwargs): # real signature unknown
        """ Return repr(self). """
        pass

    def __setattr__(self, *args, **kwargs): # real signature unknown
        """ Implement setattr(self, name, value). """
        pass

    dict = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default

    type = property(lambda self: object(), lambda self, v: None, lambda self: None)  # default


    __dict__ = None # (!) real value is "mappingproxy({'__repr__': <slot wrapper '__repr__' of 'Event' objects>, '__getattribute__': <slot wrapper '__getattribute__' of 'Event' objects>, '__setattr__': <slot wrapper '__setattr__' of 'Event' objects>, '__delattr__': <slot wrapper '__delattr__' of 'Event' objects>, '__lt__': <slot wrapper '__lt__' of 'Event' objects>, '__le__': <slot wrapper '__le__' of 'Event' objects>, '__eq__': <slot wrapper '__eq__' of 'Event' objects>, '__ne__': <slot wrapper '__ne__' of 'Event' objects>, '__gt__': <slot wrapper '__gt__' of 'Event' objects>, '__ge__': <slot wrapper '__ge__' of 'Event' objects>, '__bool__': <slot wrapper '__bool__' of 'Event' objects>, '__dict__': <member '__dict__' of 'Event' objects>, 'type': <member 'type' of 'Event' objects>, 'dict': <member 'dict' of 'Event' objects>, '__doc__': 'Event(type, dict) -> EventType instance\\nEvent(type, **attributes) -> EventType instance\\ncreate a new event object', '__hash__': None})"
    __hash__ = None


# variables with complex values

_PYGAME_C_API = None # (!) real value is '<capsule object "pygame.event._PYGAME_C_API" at 0x0352F158>'

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x03599CB0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.event', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x03599CB0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\event.cp37-win32.pyd')"

