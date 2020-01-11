import numpy as np
import matplotlib.pyplot as plt
import collections


def find_k_opt(N):
    # smallest k such that 1/k + 1/(k+1) + ... + 1/(n-1) <= 1
    k = N
    summation = 0
    while summation <= 1:
        if k == 1:
            return 1
        k -= 1
        summation += 1 / k
    return k + 1


def choose_candidate(N):
    """ Choose a candidate from a list of n candidates using the optimal strategy. """

    candidates = np.arange(1, N + 1)
    np.random.shuffle(candidates)

    stop = 37  # either an interger or find_k_opt(N)
    best_from_rejected = np.max(candidates[:stop])
    rest = candidates[stop:]

    try:
        return rest[rest > best_from_rejected][0]
    except IndexError:
        return candidates[-1]


if __name__ == '__main__':
    N = 100  # Number of Candidates
    number_of_trials = 10000 * N
    sim = np.array([choose_candidate(N) for i in range(number_of_trials)])
    expect = np.array(
        [collections.Counter(sim)[i] for i in range(1, N+1)]) * 100 / number_of_trials
    with plt.style.context('seaborn'):
        plt.bar(np.arange(len(expect)),
                height=expect,
                color='#1b4f72',
                label='Verteilung der ausgewählten Bewerberinnen')  # Limit for View is at bins=10^4!

        plt.legend(loc='upper left',
                   prop={'weight': 'bold', 'size': 20},
                   frameon=True,
                   fancybox=True,
                   shadow=True,
                   facecolor='white')  # location of legend upper left (best option)
        plt.title('Es wurde zu {:0.2f}% die beste Bewerberin genommen.'.format(
            collections.Counter(sim)[N] / (100 * N)),
            size=20,
            weight='bold')
        plt.tick_params(labelsize=15)
        plt.xlabel('Ausgewählte Kandidatin', fontsize=20, fontweight='bold')
        plt.ylabel('Häufigkeit in [%]', fontsize=20, fontweight='bold')
    plt.grid(True)
    plt.show()
