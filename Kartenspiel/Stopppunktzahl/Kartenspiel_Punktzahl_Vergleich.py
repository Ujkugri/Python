import numpy as np
import random
import matplotlib.pyplot as plt


def cards(number_of_points):
    k = True
    counter = 0
    result = []
    sum = 0
    # Original cards =  [1,2,3,4,5,6,7,8,9,10,11,11,11]*4
    cards = [1,2,3,4,5,6,7,8,9,10,11,11,11]*4
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
    avg = []
    number_of_max_points = 150  # Original 150
    for n in range(1, number_of_max_points+1):  # Numbering starts at 0 in Python
        number_of_points = n  # Number of Throws
        number_of_simulation = 100000  # 10000000 Number of Simulations
        sim = np.array([cards(number_of_points) for i in range(number_of_simulation)])
        average = np.sum(sim)/number_of_simulation
        avg.append(average)
        print(average)

    with plt.style.context('seaborn'):
        y = avg
        x = np.arange(1, number_of_max_points+1, 1)  # arange excludes stop
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

        plt.title('Der maximale Erwartungswert liegt bei '
                  + str(round(np.amax(y), 6))
                  + ' Punkten. \n Dies ist bei der Stoppunktzahl von '
                  + str(round(y.index(np.amax(y))+1, 6))
                  + ' Punkten erreicht worden.',
                  size=20,
                  weight='bold')

        plt.tick_params(labelsize=15)
        plt.yticks(np.arange(0, np.amax(y)+5, 2))  # arange excludes stop
        plt.xticks(np.arange(0, number_of_max_points+1, 10))  # arange excludes stop

        plt.xlabel('Stoppunktzahl', fontsize=20, fontweight='bold')
        plt.ylabel('Erwartungswert', fontsize=20, fontweight='bold')
    plt.grid(True)
    plt.show()
