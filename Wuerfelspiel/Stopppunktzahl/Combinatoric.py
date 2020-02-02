import collections


def probability(n, s, t):
    prev = {0: 1}  # previous roll is 0 for first time
    for q in range(n):
        cur = collections.defaultdict(int)  # current probability
        for r, times in prev.items():
            for i in range(2, s + 1):
                # if r occured `times` times in the last iteration then
                # r+i have `times` more possibilities for the current iteration.
                cur[r + i] += times

        prev = cur  # use this for the next iteration
    return cur[t] / s ** n


if __name__ == '__main__':
    s=6
    expectancy = 10 / 3
    for t in range(1, 150):
        prob = 0
        for n in range(1, 100):
            prob += probability(n,s,t)

        expectancy += ((10 / 3) - t * (1 / 6)) * prob
        print(round(prob,6), round(expectancy,6))

