import numpy as np
import matplotlib.pyplot as plt


if __name__ == '__main__':

    x_axis = np.array([2, 6, 10, 14])
    Schuler_vor = [10, 14, 1, 15]
    Schuler_nach = [16, 14, 2, 8]
    Schulerinnen_vor = [10, 14, 0, 16]
    Schulerinnen_nach = [13, 15, 0, 12]

    with plt.style.context('seaborn'):
        plt.bar(x_axis-.5,
                height=Schulerinnen_vor,
                #color='#1b4f72',
                label='Strategie vorher'
                )  # Limit for View is at bins=10^4!

        plt.bar(x_axis+.5,
                height=Schulerinnen_nach,
                #color='#1b4f72',
                label='Strategie nachher'
                )  # Limit for View is at bins=10^4!

        plt.legend(loc='upper right',
                   prop={'weight': 'bold', 'size': 20},
                   frameon=True,
                   fancybox=True,
                   shadow=True,
                   facecolor='white')  # location of legend upper right (best option)

        plt.tick_params(labelsize=25)
        plt.xticks(x_axis, ['Punkte', 'Anzahl', 'Kombination', 'Spontan'])
        plt.yticks(range(0, 22, 5))

        plt.xlabel('Strategien der Schülerinnen', fontsize=25, fontweight='bold')
        plt.ylabel('Häufigkeit', fontsize=25, fontweight='bold')

plt.grid(True)
plt.show()
