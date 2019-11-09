import matplotlib.pyplot as plt
import numpy as np
import operator
import pandas as pd
import random


import numpy as np


class City:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def distance(self, city):
        xDis = abs(self.x - city.x)
        yDis = abs(self.y - city.y)
        distance = np.sqrt((xDis ** 2) + (yDis ** 2))
        return distance

    def __repr__(self):
        return "(" + str(self.x) + "," + str(self.y) + ")"


class Fitness:
    def __init__(self, route):
        self.route = route
        self.distance = 0
        self.fitness = 0.0

    def routeDistance(self):
        if self.distance == 0:
            pathDistance = 0
            for i in range(0, len(self.route)):
                fromCity = self.route[i]
                toCity = None
                if i + 1 < len(self.route):
                    toCity = self.route[i + 1]
                else:
                    toCity = self.route[0]
                pathDistance += fromCity.distance(toCity)
            self.distance = pathDistance
        return self.distance

    def routeFitness(self):
        if self.fitness == 0:
            self.fitness = 1 / float(self.routeDistance())
        return self.fitness


def createRoute(cityList):
    route = random.sample(cityList, len(cityList))
    return route


def initialPopulation(popSize, cityList):
    population = []

    for i in range(0, popSize):
        population.append(createRoute(cityList))
    return population


def rankRoutes(population):
    fitnessResults = {}
    for i in range(0,len(population)):
        fitnessResults[i] = Fitness(population[i]).routeFitness()
    return sorted(fitnessResults.items(), key = operator.itemgetter(1), reverse = True)


def selection(popRanked, eliteSize):
    selectionResults = []
    df = pd.DataFrame(np.array(popRanked), columns=["Index", "Fitness"])
    df['cum_sum'] = df.Fitness.cumsum()
    df['cum_perc'] = 100 * df.cum_sum / df.Fitness.sum()

    for i in range(0, eliteSize):
        selectionResults.append(popRanked[i][0])
    for i in range(0, len(popRanked) - eliteSize):
        pick = 100 * random.random()
        for i in range(0, len(popRanked)):
            if pick <= df.iat[i, 3]:
                selectionResults.append(popRanked[i][0])
                break
    return selectionResults


def matingPool(population, selectionResults):
    matingpool = []
    for i in range(0, len(selectionResults)):
        index = selectionResults[i]
        matingpool.append(population[index])
    return matingpool


def breed(parent1, parent2):
    child = []
    childP1 = []
    childP2 = []

    geneA = int(random.random() * len(parent1))
    geneB = int(random.random() * len(parent1))

    startGene = min(geneA, geneB)
    endGene = max(geneA, geneB)

    for i in range(startGene, endGene):
        childP1.append(parent1[i])

    childP2 = [item for item in parent2 if item not in childP1]

    child = childP1 + childP2
    return child


def breedPopulation(matingpool, eliteSize):
    children = []
    length = len(matingpool) - eliteSize
    pool = random.sample(matingpool, len(matingpool))

    for i in range(0, eliteSize):
        children.append(matingpool[i])

    for i in range(0, length):
        child = breed(pool[i], pool[len(matingpool) - i - 1])
        children.append(child)
    return children


def mutate(individual, mutationRate):
    for swapped in range(len(individual)):
        if (random.random() < mutationRate):
            swapWith = int(random.random() * len(individual))

            city1 = individual[swapped]
            city2 = individual[swapWith]

            individual[swapped] = city2
            individual[swapWith] = city1
    return individual


def mutatePopulation(population, mutationRate):
    mutatedPop = []

    for ind in range(0, len(population)):
        mutatedInd = mutate(population[ind], mutationRate)
        mutatedPop.append(mutatedInd)
    return mutatedPop


def nextGeneration(currentGen, eliteSize, mutationRate):
    popRanked = rankRoutes(currentGen)
    selectionResults = selection(popRanked, eliteSize)
    matingpool = matingPool(currentGen, selectionResults)
    children = breedPopulation(matingpool, eliteSize)
    nextGeneration = mutatePopulation(children, mutationRate)
    return nextGeneration


city_list = []
city_count = 30

# Create and add our cities
# city = City(60, 200)
# city_list.append(city)
# city2 = City(180, 200)
# city_list.append(city2)
# city3 = City(80, 180)
# city_list.append(city3)
# city4 = City(140, 180)
# city_list.append(city4)
# city5 = City(20, 160)
# city_list.append(city5)
# city6 = City(100, 160)
# city_list.append(city6)
# city7 = City(200, 160)
# city_list.append(city7)
# city8 = City(140, 140)
# city_list.append(city8)
# city9 = City(40, 120)
# city_list.append(city9)
# city10 = City(100, 120)
# city_list.append(city10)
# city11 = City(180, 100)
# city_list.append(city11)
# city12 = City(60, 80)
# city_list.append(city12)
# city13 = City(120, 80)
# city_list.append(city13)
# city14 = City(180, 60)
# city_list.append(city14)
# city15 = City(20, 40)
# city_list.append(city15)
# city16 = City(80, 80)
# city_list.append(city16)
# city17 = City(200, 40)
# city_list.append(city17)
# city18 = City(20, 20)
# city_list.append(city18)
# city19 = City(60, 20)
# city_list.append(city19)
# city20 = City(160, 20)
# city_list.append(city20)

for i in range(0, city_count):
    city_list.append(City(x=int(random.random() * 200), y=int(random.random() * 200)))


def geneticAlgorithmPlot(population, popSize, eliteSize, mutationRate, generations):

    pop = initialPopulation(popSize, population)
    progress = []
    progress.append(1 / rankRoutes(pop)[0][1])

    for i in range(0, generations):
        pop = nextGeneration(pop, eliteSize, mutationRate)
        progress.append(1 / rankRoutes(pop)[0][1])
        print('Generation ' + str(i))
        print(progress[i])

    bestRouteIndex = rankRoutes(pop)[0][0]
    bestRoute = pop[bestRouteIndex]

    plt.subplot(121)
    plt.plot(progress)
    plt.ylabel('Distance')
    plt.xlabel('Generation')
    plt.title('Traveling Salesman Problem')

    plt.subplot(122)
    plt.plot([bestRoute[i % city_count].x for i in range(city_count + 1)],
             [bestRoute[i % city_count].y for i in range(city_count + 1)], 'xb-')
    plt.title('Traveling Salesman Problem V2')
    plt.show()


geneticAlgorithmPlot(population=city_list, popSize=900, eliteSize=25, mutationRate=0.001, generations=300)

