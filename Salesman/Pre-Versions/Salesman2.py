import random, numpy, math, copy, matplotlib.pyplot as plt
a = 50
cities = [random.sample(range(100), 2) for x in range(a)]
tour = random.sample(range(a), a)
for temperature in numpy.logspace(0, 5, num=10000000)[::-1]:
    [i, j] = sorted(random.sample(range(a), 2))
    newTour = tour[:i] + tour[j:j+1] + tour[i+1:j] + tour[i:i+1] + tour[j+1:]
    if math.exp((sum([math.sqrt(sum([(cities[tour[(k+1) % a]][d] - cities[tour[k % a]][d])**2 for d in [0, 1]])) for k in [j, j-1, i, i-1]]) - sum([math.sqrt(sum([(cities[newTour[(k+1) % a]][d] - cities[newTour[k % a]][d])**2 for d in [0, 1]])) for k in [j, j-1, i, i-1]])) / temperature) > random.random():
        tour = copy.copy(newTour)
plt.plot([cities[tour[i % a]][0] for i in range(a+1)], [cities[tour[i % a]][1] for i in range(a+1)], 'xb-')
plt.show()
