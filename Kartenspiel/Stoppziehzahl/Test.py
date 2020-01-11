import numpy as np
import matplotlib.pyplot as plt
import collections

avg = [
    4.230769,
    6.470588,
    7.376471,
    7.426651,
    6.962485,
    6.221795,
    5.365171,
    4.496524,
    3.678974,
    2.946982,
    2.315486,
    1.786672,
    1.354893,
    1.010157,
    0.740529,
    0.533714,
    0.378048,
    0.263045,
    0.179661,
    0.120347,
    0.078978,
    0.050711,
    0.03181,
    0.019458,
    0.011582,
    0.006692,
    0.003742,
    0.002018,
    0.001045,
    0.000517,
    0.000243,
    0.000107,
    4.4e-05,
    1.7e-05,
    6e-06,
    2e-06,
    0.0,
    0.0,
    0.0,
    0.0
]

avg_real = [
    4.2286108,
    6.4693642,
    7.3754545,
    7.430655,
    6.9627496,
    6.2228944,
    5.3658326,
    4.4867741,
    3.6828311,
    2.9472526,
    2.3183832,
    1.7877374,
    1.3516779,
    1.0083368,
    0.7443455,
    0.5308985,
    0.3745173,
    0.2626675,
    0.1790011,
    0.1202524,
    0.0791583,
    0.0513659,
    0.03134,
    0.0183349,
    0.0111419,
    0.0066985,
    0.0035433,
    0.002097,
    0.0010587,
    0.0007098,
    0.0004405,
    3.49e-05,
    1.77e-05,
    1.68e-05,
    0,
    0,
    0,
    0,
    0,
    0

]

if __name__ == '__main__':

    with plt.style.context('seaborn'):
        y = avg_real
        x = np.arange(1, len(avg_real) + 1, 1)  # arange excludes stop

        y2 = avg[0:40]

        plt.plot(x, y,
                 color='#dc7633',
                 linestyle='dashed',
                 marker='P',
                 markerfacecolor='#3498db',
                 label='theoretische Erwartungswerte'
                 )  # Limit for View is at bins=10^4!

        plt.plot(x, y2,
                 color='#800000',
                 linestyle='dashed',
                 marker='X',
                 markerfacecolor='#008080',
                 label='simulierte Erwartungswerte'
                 )  # Limit for View is at bins=10^4!

        plt.title('Der maximale Erwartungswert liegt bei {:0.6f} Punkten.'.format(np.amax(y)) +
                  '\n Dies ist bei der Stoppziehzahl von {:0.0f} Zügen erreicht worden.'.format(
                      y.index(np.amax(y))+1),  # Numbering starts at 0 in Python
                  size=20,
                  weight='bold')

        plt.legend(loc='upper right',
                   prop={'weight': 'bold', 'size': 15},
                   frameon=True,
                   fancybox=True,
                   shadow=True,
                   facecolor='white')  # location of legend upper right (best option)

        plt.yticks(np.arange(0, np.amax(y)+3, 2))  # arange excludes stop
        plt.xticks(np.arange(0, len(y)+5, 10))  # arange excludes stop
        plt.tick_params(labelsize=15)
        plt.xlabel('Stoppziehzahl', fontsize=20, fontweight='bold')
        plt.ylabel('Erwartunswert', fontsize=20, fontweight='bold')

    plt.grid(True)
    plt.show()
