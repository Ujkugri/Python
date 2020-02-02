import numpy as np


def probability(cards, target, with_replacement=False):
    def _a(idx, l, r, t, w):  # w boolean for replacement True ore False
        if t == sum(l):
            r.append(len(l))
        elif t < sum(l):
            return
        for u in range(idx, len(cards)):
            _a(u if w else (u+1), l + [cards[u]], r, t, w)
        return r
    return _a(0, [], [], target, with_replacement)


if __name__ == '__main__':
    s = 52  # amount of cards in your deck
    cards = [c for c in range(1, 11)] * 4
    prob = 0
    expect = 220 / 52

    for target in range(51, 151):  # run till 150 points
        prob = probability(cards, target, with_replacement=False)
        percentage = 0
        for i in range(len(prob)):
            percentage += np.math.factorial((prob[i])) * np.math.factorial(s-(prob[i])) / (np.math.factorial(s))

        # Expectancy Numbering start in Python at 0 for index
        expect += ( (220-(target-1)) - (target) * (12)) * percentage/s

        print(percentage)
        print(expect)
