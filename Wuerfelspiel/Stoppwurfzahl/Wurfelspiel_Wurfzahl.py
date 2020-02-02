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
        result.append(die)
    summe = np.sum(sum)
    return summe


if __name__ == '__main__':
    number_of_throws = 5  # Number of Throws
    number_of_simulation = 10000000  # 10000000 Number of Simulations
    sim = np.array([dice(number_of_throws) for i in range(number_of_simulation)])
    x_axis = np.array([i for i in range(number_of_throws*2, number_of_throws*6+1)])
    x_axis = np.append(0, x_axis)

    expect = np.array(
            [collections.Counter(sim)[i] for i in x_axis]
                    ) * 100 / number_of_simulation
    print(expect)

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
                collections.Counter(sim)[0] / (number_of_simulation / 100)),
                    size=20,
                    weight='bold')

        plt.tick_params(labelsize=10)
        plt.xticks(range(len(expect)), x_axis)

        plt.xlabel('Gesamtpunktzahl', fontsize=20, fontweight='bold')
        plt.ylabel('Häufigkeit in [%]', fontsize=20, fontweight='bold')

plt.grid(True)
plt.show()

