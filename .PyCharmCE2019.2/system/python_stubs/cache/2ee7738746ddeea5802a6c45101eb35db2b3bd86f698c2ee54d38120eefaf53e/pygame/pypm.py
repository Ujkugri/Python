# encoding: utf-8
# module pygame.pypm
# from C:\Program Files (x86)\Python37-32\lib\site-packages\pygame\pypm.cp37-win32.pyd
# by generator 1.147
# no doc

# imports
import builtins as __builtins__ # <module 'builtins' (built-in)>
import array as array # <module 'array' (built-in)>

# Variables with simple values

FALSE = 0

FILT_ACTIVE = 1
FILT_AFTERTOUCH = 768

FILT_CHANNEL_AFTERTOUCH = 256

FILT_CLOCK = 4
FILT_CONTROL = 2048
FILT_F9 = 16
FILT_FD = 32
FILT_MTC = 8192
FILT_NOTE = 128
FILT_PITCHBEND = 4096
FILT_PLAY = 8

FILT_POLY_AFTERTOUCH = 512

FILT_PROGRAM = 1024
FILT_REALTIME = 127
FILT_RESET = 64

FILT_SONG_POSITION = 16384
FILT_SONG_SELECT = 32768

FILT_SYSEX = 2
FILT_TICK = 16
FILT_TUNE = 65536
FILT_UNDEFINED = 48

TRUE = 1

__version__ = '0.0.6'

# functions

def Channel(*args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
    """
    Return Channel object for given MIDI channel number 1 - 16.
    
        Channel(<chan>) is used with ChannelMask on input MIDI streams.
    
        Example:
    
        To receive input on channels 1 and 10 on a MIDI stream called
        MidiIn::
    
            MidiIn.SetChannelMask(pypm.Channel(1) | pypm.Channel(10))
    
        .. note::
            PyPortMidi Channel function has been altered from
            the original PortMidi c call to correct for what
            seems to be a bug --- i.e. channel filters were
            all numbered from 0 to 15 instead of 1 to 16.
    """
    pass

def CountDevices(*args, **kwargs): # real signature unknown
    """ Return number of available MIDI (input and output) devices. """
    pass

def GetDefaultInputDeviceID(*args, **kwargs): # real signature unknown
    """
    Return the number of the default MIDI input device.
    
        See the PortMidi documentation on how the default device is set and
        determined.
    """
    pass

def GetDefaultOutputDeviceID(*args, **kwargs): # real signature unknown
    """
    Return the number of the default MIDI output device.
    
        See the PortMidi documentation on how the default device is set and
        determined.
    """
    pass

def GetDeviceInfo(*args, **kwargs): # real signature unknown
    """
    Return device info tuple for MIDI device given by device_no.
    
        The returned tuple has the following five items:
    
        * underlying MIDI API (string)
        * device name (string)
        * whether device can be opened as input (1) or not (0)
        * whether device can be opened as output (1) or not (0)
        * whether device is currently opened (1) or not (0)
    """
    pass

def GetErrorText(*args, **kwargs): # real signature unknown
    """ Return human-readable error message translated from error number. """
    pass

def Initialize(*args, **kwargs): # real signature unknown
    """
    Initialize PortMidi library.
    
        This function must be called once before any other function or class from
        this module can be used.
    """
    pass

def Terminate(*args, **kwargs): # real signature unknown
    """
    Terminate use of PortMidi library.
    
        Call this to clean up Midi streams when done.
    
        If you do not call this on Windows machines when you are done with MIDI,
        your system may crash.
    """
    pass

def Time(*args, **kwargs): # real signature unknown
    """ Return the current time in ms of the PortMidi timer. """
    pass

# classes

class Input(object):
    """
    Represents an input MIDI stream device.
    
        Takes the form::
    
            input = pypm.Input(input_device)
    """
    def Close(self, *args, **kwargs): # real signature unknown
        """
        Close the midi input device.
        
                PortMidi attempts to close open streams when the application exits --
                this is particularly difficult under Windows, so it is best to take
                care to close all devices explicitly.
        """
        pass

    def Poll(self, *args, **kwargs): # real signature unknown
        """
        Test whether input is available.
        
                Returns TRUE if input can be read, FALSE otherwise, or an error value.
        """
        pass

    def Read(self, *args, **kwargs): # real signature unknown
        """
        Read and return up to max_events events from input.
        
                Reads up to max_events midi events stored in the input buffer and
                returns them as a list in the following form::
        
                    [
                        [[status, data1, data2, data3], timestamp],
                        [[status, data1, data2, data3], timestamp],
                        ...
                    ]
        """
        pass

    def SetChannelMask(self, Channel, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Set channel mask to filter incoming messages based on channel.
        
                The mask is a 16-bit bitfield corresponding to appropriate channels
                Channel(<channel>) can assist in calling this function, i.e. to
                receive only input on channel 1, call this method like this::
        
                    SetChannelMask(Channel(1))
        
                Multiple channels should be OR'd together::
        
                    SetChannelMask(Channel(10) | Channel(11))
        
                .. note::
                    The PyPortMidi Channel function has been altered from the original
                    PortMidi C call to correct for what seems to be a bug --- i.e.
                    channel filters were all numbered from 0 to 15 instead of 1 to 16.
        """
        pass

    def SetFilter(self, filters): # real signature unknown; restored from __doc__
        """
        Set filters on an open input stream.
        
                Usage::
        
                    input.SetFilter(filters)
        
                Filters are used to drop selected input event types. By default, only
                active sensing messages are filtered. To prohibit, say, active sensing
                and sysex messages, call
        
                ::
        
                    input.SetFilter(FILT_ACTIVE | FILT_SYSEX);
        
                Filtering is useful when midi routing or midi thru functionality is
                being provided by the user application. For example, you may want to
                exclude timing messages (clock, MTC, start/stop/continue), while
                allowing note-related messages to pass. Or you may be using a sequencer
                or drum-machine for MIDI clock information but want to exclude any
                notes it may play.
        
                .. note::
                    SetFilter empties the buffer after setting the filter,
                    just in case anything got through.
        """
        pass

    def _check_open(self, *args, **kwargs): # real signature unknown
        """
        Check whether midi device is open, and if not, raises an error.
        
                Internal method, should be used only by other methods of this class.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        """ Instantiate MIDI input stream object. """
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass


class Output(object):
    """
    Represents an output MIDI stream device.
    
        Takes the form::
    
            output = pypm.Output(output_device, latency)
    
        latency is in ms. If latency == 0 then timestamps for output are ignored.
    """
    def Abort(self, *args, **kwargs): # real signature unknown
        """
        Terminate outgoing messages immediately.
        
                The caller should immediately close the output port after calling this
                method. This call may result in transmission of a partial midi message.
                There is no abort for Midi input because the user can simply ignore
                messages in the buffer and close an input device at any time.
        """
        pass

    def Close(self, *args, **kwargs): # real signature unknown
        """
        Close the midi output device, flushing any pending buffers.
        
                PortMidi attempts to close open streams when the application exits --
                this is particularly difficult under Windows, so it is best to take
                care to close all devices explicitly.
        """
        pass

    def Write(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Output a series of MIDI events given by data list n this device.
        
                Usage::
        
                    Write([
                        [[status, data1, data2, data3], timestamp],
                        [[status, data1, data2, data3], timestamp],
                        ...
                    ])
        
                The data1/2/3 items in each event are optional::
        
                   Write([[[0xc0, 0, 0], 20000]])
        
                is equivalent to::
        
                   Write([[[0xc0], 20000]])
        
                Example:
        
                Send program change 1 at time 20000 and send note 65 with velocity 100
                at 500 ms later::
        
                     Write([[[0xc0, 0, 0], 20000], [[0x90, 60, 100], 20500]])
        
                .. notes::
                    1. Timestamps will be ignored if latency == 0.
        
                    2. To get a note to play immediately, send the note on event with
                       the result from the Time() function as the timestamp.
        """
        pass

    def WriteShort(self, status, data1, data2): # real signature unknown; restored from __doc__
        """
        Output MIDI event of three bytes or less immediately on this device.
        
                Usage::
        
                    WriteShort(status, data1, data2)
        
                status must be a valid MIDI status byte, for example:
        
                0xCx = Program Change
                0xBx = Controller Change
                0x9x = Note On
        
                where x is the MIDI channel number 0 - 0xF.
        
                The data1 and data2 arguments are optional and assumed to be 0 if
                omitted.
        
                Example:
        
                Send note 65 on with velocity 100::
        
                     WriteShort(0x90, 65, 100)
        """
        pass

    def WriteSysEx(self, *args, **kwargs): # real signature unknown; NOTE: unreliably restored from __doc__ 
        """
        Output a timestamped system-exclusive MIDI message on this device.
        
                Usage::
        
                    WriteSysEx(<timestamp>, <msg>)
        
                <msg> can be a *list* or a *string*
        
                Example (assuming 'out' is an output MIDI stream):
        
                    out.WriteSysEx(0, '\xF0\x7D\x10\x11\x12\x13\xF7')
        
                This is equivalent to::
        
                    out.WriteSysEx(pypm.Time(),
                        [0xF0, 0x7D, 0x10, 0x11, 0x12, 0x13, 0xF7])
        """
        pass

    def _check_open(self, *args, **kwargs): # real signature unknown
        """
        Check whether midi device is open, and if not, raises an error.
        
                Internal method, should be used only by other methods of this class.
        """
        pass

    def __init__(self, *args, **kwargs): # real signature unknown
        """ Instantiate MIDI output stream object. """
        pass

    @staticmethod # known case of __new__
    def __new__(*args, **kwargs): # real signature unknown
        """ Create and return a new object.  See help(type) for accurate signature. """
        pass

    def __reduce__(self, *args, **kwargs): # real signature unknown
        pass

    def __setstate__(self, *args, **kwargs): # real signature unknown
        pass


# variables with complex values

__loader__ = None # (!) real value is '<_frozen_importlib_external.ExtensionFileLoader object at 0x0346E0F0>'

__spec__ = None # (!) real value is "ModuleSpec(name='pygame.pypm', loader=<_frozen_importlib_external.ExtensionFileLoader object at 0x0346E0F0>, origin='C:\\\\Program Files (x86)\\\\Python37-32\\\\lib\\\\site-packages\\\\pygame\\\\pypm.cp37-win32.pyd')"

__test__ = {}

