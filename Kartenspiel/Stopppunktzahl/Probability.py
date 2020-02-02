import numpy as np
import matplotlib.pyplot as plt
import collections

avg = [
0.07692307692307693,
0.08144796380090498,
0.08917043740573152,
0.09460399544433157,
0.10328685320281959,
0.10993720565149141,
0.11975959614615087,
0.12763750344655062,
0.13894576488533233,
0.14828604807040563,
0.08421637693716912,
0.08333667364808756,
0.0848702035903424,
0.08301217116273091,
0.08357698557062428,
0.08046853706085695,
0.07953809348704428,
0.0749192097260486,
0.07210060687872638,
0.06529695882483295,
0.06017119162577961,
0.05816875041014905,
0.05621253519221202,
0.053951715167627014,
0.051676630551473667,
0.04910076033564793,
0.04677245162025962,
0.04411354338086778,
0.04179615917583688,
0.039474432138603126,
0.0375708285681343,
0.03583566200079015,
0.0340834830646647,
0.03240692617748429,
0.030721075790561568,
0.029133733262747052,
0.027604662429548624,
0.02614998756784291,
0.0247859454268233,
0.023507013775409324,
0.02229175448682209,
0.021130766274270796,
0.02001384161698558,
0.018950362415193264,
0.017932160440746567,
0.01697033346589648,
0.016054794087303873,
0.015187601620818263,
0.01436559395965475,
0.013583706759374975
]


with plt.style.context('seaborn'):
    y = avg
    x = np.arange(1, len(avg)+1, 1)  # arange excludes stop
    plt.plot(x, y,
             color='#dc7633',
             linestyle='dashed',
             marker='d',
             markerfacecolor='#3498db',
             label='Verteilung der Durchschnittspunktzahlen')  # Limit for View is at bins=10^4!

    plt.legend(loc='upper right',
               prop={'weight': 'bold', 'size': 15},
               frameon=True,
               fancybox=True,
               shadow=True,
               facecolor='white')  # location of legend upper right (best option)

    plt.title('Die maximal erreichte Durchschnittspunktzahl liegt bei '
              + str(round(np.amax(avg),6))
              + ' Punkten. \n Dies ist bei der Stoppunktzahl von '
              + str(avg.index(np.amax(avg)) + 1)
              + ' Punkten erreicht worden.',
              size=20,
              weight='bold')

    plt.xlabel('Stoppunktzahl', fontsize=25, fontweight='bold')
    plt.ylabel('Durchschnittspunktzahl', fontsize=25, fontweight='bold')
plt.grid(True)
plt.show()
