import numpy as np
import matplotlib.pyplot as plt
import collections


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
#                print('Number of throws to get "1":', counter)
        result.append(die)

    result = np.asarray(result)
    summe = np.sum(sum)
#    print(result)
#    print(summe)
    return summe


if __name__ == '__main__':
    number_of_throws = 5  # Number of Throws
    number_of_simulation = 100000  # Number of Simulations
    sim = np.array([dice(number_of_throws) for i in range(number_of_simulation)])
    number_of_losses = collections.Counter(sim)[0]
    average = np.sum(sim)/number_of_simulation
    print(sim)
    print(number_of_losses)

    with plt.style.context('seaborn'):
        plt.hist(sim,
                 bins=4*number_of_throws*6,
                 color='#1b4f72',
                 label='Verteilung der Gesamtpuntzahlen')  # Limit for View is at bins=10^4!

        plt.legend(loc='upper right',
                   prop={'weight': 'bold', 'size': 25},
                   frameon=True,
                   fancybox=True,
                   shadow=True,
                   facecolor='white')  # location of legend upper right (best option)
        plt.title('Es wurde zu '
                  + str(number_of_losses/(number_of_simulation/100))
                  + '% das Spiel verloren. \n Dabei wurde eine durchschnittliche Punktzahl von '
                  + str(average)
                  + ' Punkten erreicht.',
                  size=25,
                  weight='bold')
        plt.tick_params(labelsize=20)
#        plt.xticks(np.arange(0, number_of_throws*6 + 1, 1))
#        plt.ylim(0, number_of_losses*1.25)

        plt.xlabel('Ausgewählter Kandidat', fontsize=25, fontweight='bold')
        plt.ylabel('Häufigkeit', fontsize=25, fontweight='bold')
    plt.grid(True)
    plt.show()

