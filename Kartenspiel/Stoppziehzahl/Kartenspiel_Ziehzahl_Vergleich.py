import numpy as np
import random
import matplotlib.pyplot as plt


def cards(number_of_draws):
    k = True
    counter = 0
    result = []
    sum = 0

    cards = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 11, 11] * 4  # 11 is for Queen, King and Knight
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

    summe = np.sum(sum)
    return summe


if __name__ == '__main__':
    avg = []
    number_of_max_draws = 50
    for n in range(1, number_of_max_draws+1):  # Numbering starts at 0 in Python
        number_of_draws = n  # Number of Draws
        number_of_simulation = 10000  # 10000000 Number of Simulations
        sim = np.array([cards(number_of_draws) for i in range(1, number_of_simulation)])
        average = np.sum(sim)/number_of_simulation
        avg.append(average)
        print(average)

    with plt.style.context('seaborn'):
        y = avg
        x = np.arange(1, number_of_max_draws+1, 1) # arange excludes stop
        plt.plot(x, y,
                 color='#dc7633',
                 linestyle='dashed',
                 marker='d',
                 markerfacecolor='#3498db',
                 label='Verteilung der Erwartungswerte')   # Limit for View is at bins=10^4!

        plt.legend(loc='upper right',
                   prop={'weight': 'bold', 'size': 20},
                   frameon=True,
                   fancybox=True,
                   shadow=True,
                   facecolor='white')  # location of legend upper right (best option)

        plt.title('Der maximale Erwartungswert liegt bei {:0.6f} Punkten.'.format(np.amax(y)) +
                  '\n Dies ist bei der Stopziehzahl von {:0.0f} Mal Ziehen erreicht worden.'.format(
                      y.index(np.amax(y))+1),  # Numbering starts at 0 in Python
                  size=20,
                  weight='bold')

        plt.tick_params(labelsize=15)
        plt.yticks(np.arange(0, np.amax(y)+3, 2))  # arange excludes stop
        plt.xticks(np.arange(0, number_of_max_draws+1, 5))  # arange excludes stop

        plt.xlabel('Stopziehzahl', fontsize=20, fontweight='bold')
        plt.ylabel('Erwartungswert', fontsize=20, fontweight='bold')
    plt.grid(True)
    plt.show()
