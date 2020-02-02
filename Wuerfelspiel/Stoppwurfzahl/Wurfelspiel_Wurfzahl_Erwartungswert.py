import numpy as np
import matplotlib.pyplot as plt

if __name__ == '__main__':
    number_of_throws = 50
    with plt.style.context('seaborn'):
        x = np.arange(1, number_of_throws+1, 1)  # arrange excludes stop
        y = [round(4*k*(5/6)**k, 6) for k in range(1, number_of_throws+1)]
        print(y)

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
        plt.xticks(np.arange(0, number_of_throws+1, 10))  # arange excludes stop

        plt.xlabel('Stoppwurfzahl', fontsize=20, fontweight='bold')
        plt.ylabel('Erwartungswert', fontsize=20, fontweight='bold')
    plt.grid(True)
    plt.show()
