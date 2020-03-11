import os
import itertools
import matplotlib.pyplot as plt
import numpy as np

txt = open('Prime.txt', 'a+')
maxsize = 10**6

with open('Prime.txt', 'a+'):
    for a in itertools.count():
        k = 0
        for i in range(2, a // 2 + 1):
            if a % i == 0:
                k = k + 1
        if k <= 0:
            if os.stat('Prime.txt').st_size > maxsize:
                txt.close()
                break
            if os.stat('Prime.txt').st_size < maxsize:
                txt.write(str(a) + '\n')

with plt.style.context('seaborn'):

    plt.scatter(np.loadtxt('Prime.txt'), range(len(np.loadtxt('Prime.txt'))), s=.1, label='Verteilung der Primzahlen')

    plt.legend(loc='upper left',
               prop={'weight': 'bold', 'size': 25},
               frameon=True,
               fancybox=True,
               shadow=True,
               facecolor='white')  # location of legend upper right (best option)

    plt.xlabel('Primzahl', fontsize=20, fontweight='bold')
    plt.ylabel('Häufigkeit', fontsize=20, fontweight='bold')
plt.grid(True)
plt.show()






