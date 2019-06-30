import tkinter as tk
from tkinter import ttk
import youtube_dl


def downloadMP3(Entries):
    for Entry in Entries:
        string = Entry[1].get()

        ydl_opts = {
            'format': 'bestaudio/best',
            'postprocessors': [{
                'key': 'FFmpegExtractAudio',
                'preferredcodec': 'mp3',
                'preferredquality': '1080',
            }],
            # Download path
            'outtmpl': 'C:/Users/Gentian/Music/%(title)s.%(ext)s'
        }
        with youtube_dl.YoutubeDL(ydl_opts) as ydl:
            ydl.download([string])


def downloadVid(Entries):
    for Entry in Entries:
        string = Entry[1].get()

        ydl_opts = {
            'format': 'bestvideo+bestaudio/best',
            'postprocessors': [{
                'key': 'FFmpegVideoConvertor',
                'preferedformat': 'mp4',  # one of avi, flv, mkv,mp4, ogg, webm #
            }],
            # Download path
            'outtmpl': 'C:/Users/Gentian/Videos/%(title)s.%(ext)s'
        }
        with youtube_dl.YoutubeDL(ydl_opts) as ydl:
            ydl.download([string])



def GUI():
    # Tinker simple GUI

    w = tk.Label(root, text="Youtube Downloader")
    w.pack()

    Entries = []
    for i in range(int(input('Wie viele '))):
        Entry = tk.Entry(root, bd=5, width=50)
        Entry.pack(side=tk.TOP)
        Entries.append((i, Entry))
    return Entries


if __name__ == '__main__':
    root = tk.Tk()
    ents = GUI()

    b1 = tk.Button(root, text="Download MP3", command=(lambda e=ents: downloadMP3(e)))
    b1.pack(side=tk.LEFT, padx=5, pady=5)

    b2 = tk.Button(root, text="Download MP4", command=(lambda e=ents: downloadVid(e)))
    b2.pack(side=tk.LEFT, padx=5, pady=5)

    b3 = tk.Button(root, text='Quit', command=root.quit)
    b3.pack(side=tk.LEFT, padx=5, pady=5)

    root.mainloop()
