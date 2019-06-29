import tkinter
from tkinter import messagebox
from tkinter import filedialog


class MyApp(tkinter.Frame):

    def __init__(self, master=None):
        tkinter.Frame.__init__(self, master)
        self.pack()
        self.setGUI()

    def setGUI(self):
        self.inputui = tkinter.Entry(self)
        self.inputui.pack(expand=True, fill="x", padx="30", pady="10")

        self.bnESC = tkinter.Button(self)
        self.bnESC["text"] = "Abbruch"
        self.bnESC["command"] = self.quit
        self.bnESC.pack(padx="10", pady="10", side="right")

        self.bnOpenfile = tkinter.Button(self)
        self.bnOpenfile["text"] = "Openfile"
        self.bnOpenfile["command"] = self.onopenfile
        self.bnOpenfile.pack(padx="10", pady="10", side="right")

        self.bnSavefile = tkinter.Button(self)
        self.bnSavefile["text"] = "Savefile"
        self.bnSavefile["command"] = self.onsavefile
        self.bnSavefile.pack(padx="10", pady="10", side="right")

        self.bnSelectdir = tkinter.Button(self)
        self.bnSelectdir["text"] = "Selectdir"
        self.bnSelectdir["command"] = self.onselectdir
        self.bnSelectdir.pack(padx="10", pady="10", side="right")

    def onopenfile(self):
        filename = filedialog.askopenfilename(title="Dateien laden", initialdir="D:\\progs\\Python\\UI\\",
                                              multiple=True,
                                              filetypes=(("Pythondateien", ".py"), ("Alle Dateien", "*.*")))
        if filename:
            messagebox.showinfo("Auswahl", filename)
        else:
            messagebox.showinfo("Auswahl", "Abbruch")

    def onsavefile(self):
        filename = filedialog.asksaveasfilename(initialfile="Beispiel.py", title="Dateien speichern",
                                                initialdir="D:\\progs\\Python\\UI\\",
                                                filetypes=(("Pythondateien", ".py"), ("Alle Dateien", "*.*")))
        if filename:
            messagebox.showinfo("Auswahl", filename)
        else:
            messagebox.showinfo("Auswahl", "Abbruch")

    def onselectdir(self):
        filename = filedialog.askdirectory(title="Verzeichnis auswählen", initialdir="D:\\progs\\Python")
        if filename:
            messagebox.showinfo("Auswahl", filename)
        else:
            messagebox.showinfo("Auswahl", "Abbruch")


root = tkinter.Tk()
root.title("Mein Fenster")
# root.geometry("200x100")
app = MyApp(root)
app.mainloop()
