import numpy as np
import random
import matplotlib.pyplot as plt
import collections


def cards(number_of_points):
    k = True
    counter = 0
    result = []
    sum = 0
    cards = [
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 11
            ] * 4  # 11 is for Queen, King and Knight, origignal cards=[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11,11,11]
    random.shuffle(cards)

    while k:
        counter += 1

        sum += cards[0]
        if cards[0] == 11 or sum >= number_of_points:
            k = False
            if cards[0] == 11:
                sum = 0

        result.append(cards[0])
        cards.remove(cards[0])

    result = np.asarray(result)
    summe = np.sum(sum)

    return summe


if __name__ == '__main__':
    number_of_points = 17  # Number of minimum points
    number_of_simulation = 100  # 10000000 Number of Simulations
    sim = np.array([cards(number_of_points) for i in range(number_of_simulation)])
    expect = np.array(
        [collections.Counter(sim)[i] for i in [0, number_of_points, number_of_points + 1, number_of_points + 2,
                                               number_of_points + 3, number_of_points + 4, number_of_points + 5,
                                               number_of_points + 6, number_of_points + 7, number_of_points + 8,
                                               number_of_points + 9
                                               ]]
    ) * 100 / number_of_simulation

    average = np.sum(sim) / number_of_simulation

    with plt.style.context('seaborn'):
        plt.bar(np.arange(len(expect)),
                height=expect,
                color='#1b4f72',
                label='Verteilung der Gewinne')  # Limit for View is at bins=10^4!

        for rect in plt.bar(np.arange(len(expect)), expect):
            height = rect.get_height()
            plt.text(rect.get_x() + rect.get_width() / 2,
                     1.05 * height,
                     '%0.2f' % height + '%',
                     fontweight='bold',
                     ha='center',
                     va='bottom'
                     )

        plt.legend(loc='upper right',
                   prop={'weight': 'bold', 'size': 20},
                   frameon=True,
                   fancybox=True,
                   shadow=True,
                   facecolor='white')  # location of legend upper right (best option)

        plt.title('Es wurde zu '
                  + str(round(expect[0], 2))
                  + '% das Spiel verloren. \n Dabei wurde eine durchschnittliche Punktzahl von '
                  + str(average)
                  + ' Punkten erreicht.',
                  size=20,
                  weight='bold')

        plt.tick_params(labelsize=15)
        plt.ylim(0, expect[0] * 1.25)
        plt.xticks(np.arange(len(expect)), [0,
                                            number_of_points, number_of_points + 1, number_of_points + 2,
                                            number_of_points + 3, number_of_points + 4, number_of_points + 5,
                                            number_of_points + 6, number_of_points + 7, number_of_points + 8,
                                            number_of_points + 9
                                            ])

        plt.xlabel('Gesamtpunktzahl', fontsize=20, fontweight='bold')
        plt.ylabel('Häufigkeit', fontsize=20, fontweight='bold')
    plt.grid(True)
    plt.show()
