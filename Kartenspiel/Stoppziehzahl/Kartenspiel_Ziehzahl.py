import numpy as np
import random
import matplotlib.pyplot as plt
import collections


def cards(number_of_draws):
    k = True
    counter = 0
    result = []
    sum = 0
    cards = [
                1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 11
            ] * 4  # 11 is for Queen, King and Knight
    random.shuffle(cards)

    while k:
        counter += 1

        sum += cards[0]
        if cards[0] == 11 or counter == number_of_draws:
            k = False
            if cards[0] == 11:
                sum = 0

        result.append(cards[0])
        cards.remove(cards[0])

    result = np.asarray(result)
    summe = np.sum(sum)
    return summe


if __name__ == '__main__':
    number_of_draws = 3  # Number of Throws
    number_of_simulation = 100000  # 10000000 Number of Simulations
    sim = np.array([cards(number_of_draws) for i in range(number_of_simulation)])
    expect = np.array(
        [collections.Counter(sim)[i] for i in range(0, number_of_draws*10+1)]) * 100 / number_of_simulation


    with plt.style.context('seaborn'):
        plt.bar(np.arange(len(expect)),
                height=expect,
                color='#1b4f72',
                label='Verteilung der Gewinne')  # Limit for View is at bins=10^4!

        plt.legend(loc='upper right',
                   prop={'weight': 'bold', 'size': 17},
                   frameon=True,
                   fancybox=True,
                   shadow=True,
                   facecolor='white')  # location of legend upper right (best option)

        plt.title('Es wurde zu {:0.2f}% das Spiel verloren.'.format(
                collections.Counter(sim)[0] / (number_of_simulation / 100))
                    ,
                    size=20,
                    weight='bold')

        plt.tick_params(labelsize=10)

        plt.xlabel('Gesamtpunktzahl', fontsize=20, fontweight='bold')
        plt.ylabel('Häufigkeit in [%]', fontsize=20, fontweight='bold')
    plt.grid(True)
    plt.show()
