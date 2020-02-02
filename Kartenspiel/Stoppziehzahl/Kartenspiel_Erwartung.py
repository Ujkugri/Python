import numpy as np
import matplotlib.pyplot as plt


if __name__ == '__main__':
    number_of_draws = 40  # Number of allowed draws, not cards!
    with plt.style.context('seaborn'):
        erwartung = 0
        wahrscheinlichkeit = 1
        y = []
        for k in range(0, number_of_draws):
            erwartung = 5.5*(k+1)
            wahrscheinlichkeit *= (40 - k)/(52 - k)
            y.append(round(erwartung*wahrscheinlichkeit, 6))
        print(y)
        x = np.arange(1, len(y)+1, 1)  # arrange excludes stop

        plt.plot(x, y,
                 color='#dc7633',
                 linestyle='dashed',
                 marker='d',
                 markerfacecolor='#3498db',
                 label='Verteilung der Erwartungswerte')  # Limit for View is at bins=10^4!

        plt.legend(loc='upper right',
                   prop={'weight': 'bold', 'size': 20},
                   frameon=True,
                   fancybox=True,
                   shadow=True,
                   facecolor='white')  # location of legend upper right (best option)

        plt.title('Der maximale Erwartungswert liegt bei {:0.6f} Punkten.'.format(np.amax(y)) +
                  '\n Dies ist bei der Stopwurfzahl von {:0.0f} Würfen erreicht worden.'.format(
                      y.index(np.amax(y))+1),
                  size=20,
                  weight='bold')

        plt.tick_params(labelsize=15)
        plt.yticks(np.arange(0, 12, 2))  # arange excludes stop
        plt.xticks(np.arange(0, number_of_draws+1, 10))  # arange excludes stop

        plt.xlabel('Stoppwurfzahl', fontsize=20, fontweight='bold')
        plt.ylabel('Erwartungswert', fontsize=20, fontweight='bold')
    plt.grid(True)
    plt.show()
