import numpy as np
import matplotlib.pyplot as plt


def find_k_opt(N):
    # N Candidates
    candidates = np.arange(1, N+1)
    # Shuffle Candidates
    np.random.shuffle(candidates)

    # smallest k such that 1/k + 1/(k+1) + ... + 1/(n-1) <= 1
    k = N
    summation = 0
    while summation <= 1:
        if k == 1:
            return 1
        k -= 1
        summation += 1/k
    return k + 1


if __name__ == '__main__':

    # Important parameters
    N = 100  # Number of Candidates
    #

    sim = np.array([find_k_opt(i) for i in range(1, N+2)])
    opt = np.array([i/np.e for i in range(1, N+2)])

    with plt.style.context(('seaborn')):
        simplot = plt.plot(sim, label='ermitteltes k$_{opt}$', color='#3498db')
        optplot = plt.plot(opt, label='Aus Annäherung N/e', color='#dc7633')

        plt.legend(loc='upper center',
                   prop={'weight': 'bold', 'size': 25},
                   frameon=True,
                   fancybox=True,
                   shadow=True,
                   facecolor='white')  # location of legend upper left (best option)
        plt.tick_params(labelsize=25)
        plt.xlabel('Ausgewählter Kandidat', fontsize=25, fontweight='bold')
        plt.ylabel('Errechnetes k$_{opt}$', fontsize=25, fontweight='bold')
    plt.grid(True)
    plt.show()
