import neat
import pickle
import sys
from FlapPyBird.flappy import FlappyBirdApp


# Driver for NEAT solution to FlapPyBird
def evolutionary_driver():
    config = neat.Config(neat.DefaultGenome, neat.DefaultReproduction,
                         neat.DefaultSpeciesSet, neat.DefaultStagnation,
                         'config')

    # Create the population, which is the top-level object for a NEAT run.
    p = neat.Population(config)

    # Add a stdout reporter to show progress in the terminal.
    p.add_reporter(neat.StdOutReporter(True))

    # Run until we archive n.
    winner = p.run(eval_genomes)

    # Save the winner.
    pickle.dump(winner, open('winner.pkl', 'wb'))

    print('\nBest genome:\n{!s}'.format(winner))


def eval_genomes(genomes, config):
    global score, distance, energy
    idx, genomes = zip(*genomes)

    flappy = FlappyBirdApp(genomes, config)
    flappy.play()
    results = flappy.crash_info

    for result, genomes in results:

        score = result['score']
        distance = result['distance']
        energy = result['energy']

        genomes.fitness = (1000*score + 0.2*distance - 1.5*energy)/1000
    print('The top score was', score, 'with a distance of', distance, 'and', energy, 'energy used.')


def main():
    if len(sys.argv) > 1:
        evolutionary_driver()
    else:
        evolutionary_driver()


if __name__ == "__main__":
    main()
