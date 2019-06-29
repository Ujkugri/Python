import tkinter as tk
import youtube_dl


def downloadVid():
    global Entry
    string = Entry.get()

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

#Tinker simple GUI
root = tk.Tk()

w = tk.Label(root, text="Youtube Downloader")
w.pack()

Entry = tk.Entry(root, bd=5)
Entry.pack(side=tk.TOP)

button = tk.Button(root, text="Download", fg="red", command=downloadVid)
button.pack(side=tk.BOTTOM)

root.mainloop()