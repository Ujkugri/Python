import tkinter as tk
from tkinter import Tk, Label, Button
import youtube_dl


class DownloaderGUI:
    def __init__(self, master):

        self.Entries = []

        self.master = master
        master.title("Musik/Videodownloader")

        self.label = Label(master, text="Wie viele Dateien willst du runterladen?")
        self.label.grid(row=0, columnspan=2)

        self.number = tk.Entry(master, bd=5, width=5)
        self.number.grid(row=0, column=2, padx=5, pady=5)

        self.greet_button = Button(master, text="OK", command=self.greet)
        self.greet_button.grid(row=1, column=0, padx=5, pady=5, sticky=tk.W + tk.E)

        self.close_button = Button(master, text="Close", command=master.quit)
        self.close_button.grid(row=1, column=1, padx=5, pady=5, sticky=tk.W + tk.E)

        self.download_mp3 = tk.Button(root, text="Download MP3", command=(lambda: downloadMP3(self.Entries)))
        self.download_mp3.grid(row=(100 + 4), column=0, padx=5, pady=5, sticky=tk.W + tk.E)

        self.download_vid = tk.Button(root, text="Download MP4", command=(lambda: downloadVid(self.Entries)))
        self.download_vid.grid(row=(100 + 4), column=1, padx=5, pady=5, sticky=tk.W + tk.E)

    def greet(self):
        global i
        w = tk.Label(root, text="Youtube Downloader")
        w.grid(row=2, column=0, columnspan=2, padx=5, pady=5)

        for i in range(int(self.number.get())):
            Entry = tk.Entry(root, bd=5, width=30)
            Entry.grid(row=(i + 3), column=0, padx=5, pady=5, columnspan=2)
            self.Entries.append((i, Entry))
        return self.Entries


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
                'preferedformat': 'mp4',  # one of avi, flv, mkv, mp4, ogg, webm #
            }],
            # Download path
            'outtmpl': 'C:/Users/Gentian/Videos/%(title)s.%(ext)s'
        }
        with youtube_dl.YoutubeDL(ydl_opts) as ydl:
            ydl.download([string])


if __name__ == '__main__':
    root = tk.Tk()
    my_gui = DownloaderGUI(root)
    root.mainloop()
