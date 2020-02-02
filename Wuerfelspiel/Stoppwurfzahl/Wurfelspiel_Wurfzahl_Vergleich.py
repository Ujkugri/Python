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
                sum -= 20
        result.append(die)
    result = np.asarray(result)
    summe = np.sum(sum)
    return summe


if __name__ == '__main__':
    avg = []
    number_of_max_throws = 50
    for n in range(1, number_of_max_throws+1):  # Numbering starts at 0 in Python
        number_of_throws = n  # Number of Throws
        number_of_simulation = 1000000  # 10000000 Number of Simulations
        sim = np.array([dice(number_of_throws) for i in range(number_of_simulation)])
        average = np.sum(sim)/number_of_simulation
        avg.append(average)
        print(average)

    with plt.style.context('seaborn'):
        y = avg
        x = np.arange(1, number_of_max_throws+1, 1)  # arange excludes stop
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
                      y.index(np.amax(y))+1),  # Numbering starts at 0 in Python
                  size=20,
                  weight='bold')

        plt.tick_params(labelsize=15)
        plt.yticks(np.arange(0, np.amax(y)+3, 2))  # arange excludes stop
        plt.xticks(np.arange(0, number_of_throws+1, 5))  # arange excludes stop

        plt.xlabel('Stoppwurfzahl', fontsize=20, fontweight='bold')
        plt.ylabel('Erwartungswert', fontsize=20, fontweight='bold')
    plt.grid(True)
    plt.show()
