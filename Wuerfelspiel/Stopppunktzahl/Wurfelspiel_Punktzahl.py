import numpy as np
import matplotlib.pyplot as plt
import collections


def dice(number_of_points):
    k = True
    result = []
    sum = 0
    while k:
        die = np.random.randint(1, 7)  # rolls from 1 to 6
        sum += die
        if die == 1 or sum >= number_of_points:
            k = False
            if die == 1:
                sum = 0
        result.append(die)

    result = np.asarray(result)
    summe = np.sum(sum)
    return summe


if __name__ == '__main__':
    number_of_points = 20  # Number of minimum points
    number_of_simulation = 10000000  # Number of Simulations

    sim = np.array([dice(number_of_points) for i in range(number_of_simulation)])
    expect = np.array(
            [collections.Counter(sim)[i] for i in [0, number_of_points, number_of_points+1, number_of_points+2,
                                                   number_of_points+3, number_of_points+4, number_of_points+5,]]
                    ) * 100 / number_of_simulation

    average = np.sum(sim) / number_of_simulation

    with plt.style.context('seaborn'):
        plt.bar(np.arange(len(expect)),
                height=expect,
                color='#1b4f72',
                label='Verteilung der Gewinne')  # Limit for View is at bins=10^4!

        for rect in plt.bar(np.arange(len(expect)), expect):
            height = rect.get_height()
            plt.text(rect.get_x() + rect.get_width()/2,
                     1.05*height,
                     '%0.2f' % height + '%',
                     fontweight='bold',
                     ha='center',
                     va='bottom'
                     )

        plt.legend(loc='upper right',
                       prop={'weight': 'bold', 'size': 25},
                       frameon=True,
                       fancybox=True,
                       shadow=True,
                       facecolor='white')  # location of legend upper right (best option)

        plt.title('Es wurde zu {:0.2f}% das Spiel verloren.'.format(
                collections.Counter(sim)[0] / (number_of_simulation / 100)),
                      size=20,
                      weight='bold')

        plt.tick_params(labelsize=15)
        plt.ylim(0, 71)
        plt.xticks(np.arange(len(expect)), [0,
                                            number_of_points, number_of_points+1, number_of_points+2,
                                            number_of_points+3,number_of_points+4,number_of_points+5,
                                            ])
            #
        plt.xlabel('Punktzahl', fontsize=20, fontweight='bold')
        plt.ylabel('Häufigkeit in [%]', fontsize=20, fontweight='bold')
    plt.grid(True)
    plt.show()
