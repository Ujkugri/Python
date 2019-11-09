import numpy as np
import matplotlib.pyplot as plt


def dice(number_of_throws):
    k = True
    counter = 0
    result = []
    sum = 0
    while k:
        counter += 1
        die = np.random.randint(1, 7)  # rolls from 1 to 6
        sum += die
        if die == 1 or counter == number_of_throws:
            k = False
            if die == 1:
                sum = 0
        result.append(die)
    result = np.asarray(result)
    summe = np.sum(sum)
    return summe


if __name__ == '__main__':
    avg = []
    number_of_max_throws = 50
    for n in range(1, number_of_max_throws+1):
        number_of_throws = n  # Number of Throws
        number_of_simulation = 1000000  # Number of Simulations
        sim = np.array([dice(number_of_throws) for i in range(number_of_simulation)])
        average = np.sum(sim)/number_of_simulation
        avg.append(average)

    with plt.style.context('seaborn'):
        plt.plot(avg,
                 color='#dc7633',
                 linestyle='dashed',
                 marker='d',
                 markerfacecolor='#3498db',
                 label='Verteilung der Gesamtpuntzahlen')  # Limit for View is at bins=10^4!

        plt.legend(loc='upper right',
                   prop={'weight': 'bold', 'size': 25},
                   frameon=True,
                   fancybox=True,
                   shadow=True,
                   facecolor='white')  # location of legend upper right (best option)
        plt.title('Der maximale erreichte Durschschnittspunktzahl liegt '
                  + str(np.amax(avg))
                  + ' Punkten. \n Dies ist bei der Stopwurfzahl von '
                  + str(avg.index(np.amax(avg)))
                  + ' Würfen erreicht worden.',
                  size=25,
                  weight='bold')
        plt.tick_params(labelsize=20)
        plt.yticks(np.arange(0, 12, 1))

        plt.xlabel('Stopwurfzahl', fontsize=25, fontweight='bold')
        plt.ylabel('Häufigkeit', fontsize=25, fontweight='bold')
    plt.grid(True)
    plt.show()
