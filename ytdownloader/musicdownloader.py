import tkinter as tk
from tkinter import messagebox
from tkinter import filedialog
import youtube_dl


def downloadVid(Entries):
    for Entry in Entries:
        string = Entry[1].get()

        ydl_opts = {
            'format': 'bestaudio/best',
            'postprocessors': [{
                'key': 'FFmpegExtractAudio',
                'preferredcodec': 'mp3',
                'preferredquality': '320',
            }],
            # Download path
            'outtmpl': 'C:/Users/Gentian/Music/%(title)s.%(ext)s'
        }
        with youtube_dl.YoutubeDL(ydl_opts) as ydl:
            ydl.download([string])


def GUI():
    # Tinker simple GUI
    w = tk.Label(root, text="Youtube Downloader")
    w.pack()

    Entries = []
    for i in range(int(input('Wie viele '))):
        Entry = tk.Entry(root, bd=5)
        Entry.pack(side=tk.TOP)
        Entries.append((i, Entry))
    return Entries


if __name__ == '__main__':
    root = tk.Tk()
    ents = GUI()

    b1 = tk.Button(root, text="Download", fg="red", command=(lambda e=ents: downloadVid(e)))
    b1.pack(side=tk.LEFT, padx=5, pady=5)

    b2 = tk.Button(root, text='Quit', command=root.quit)
    b2.pack(side=tk.LEFT, padx=5, pady=5)

    root.mainloop()
    root.filename = tk.filedialog.asksaveasfilename(initialdir="/", title="Select file",
                                                    filetypes=(("jpeg files", "*.jpg"), ("all files", "*.*")))
