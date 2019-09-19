"""Simulate the Monty Hall problem.

"""

import random
import matplotlib.pyplot as plt


def simulate(num_doors, switch):
    """(int, bool): bool

    Carry out the game for one contestant.  If 'switch' is True,
    the contestant will switch their chosen door when offered the chance.
    Returns a Boolean value telling whether the simulated contestant won.
    """
    # Doors are numbered from 0 up to num_doors-1 (inclusive).
    # Randomly choose the door hiding the prize.
    winning_door = random.randint(0, num_doors-1)
    # The contestant picks a random door, too.
    choice = random.randint(0, num_doors-1)
    # The host opens all but two doors.
    closed_doors = list(range(num_doors))

    open_doors = 3

    while len(closed_doors) > open_doors:
        # Randomly choose a door to open.
        door_to_remove = random.choice(closed_doors)

        # The host will never open the winning door, or the door
        # chosen by the contestant.
        if door_to_remove == winning_door or door_to_remove == choice:
            continue

        # Remove the door from the list of closed doors.
        closed_doors.remove(door_to_remove)

    # There are always two doors remaining.
    assert len(closed_doors) == open_doors

    # Does the contestant want to switch their choice?
    if switch:
        # There are two closed doors left.  The contestant will never
        # choose the same door, so we'll remove that door as a choice.
        available_doors = list(closed_doors) # Make a copy of the list.
        available_doors.remove(choice)

        # Change choice to the only door available.
        choice = available_doors.pop()

    # Did the contestant win?
    won = (choice == winning_door)
    return won


def main():
    # Carry out the trials
    x_axe = []
    y_axe_switch = []
    y_axe_stay = []
    winning_non_switchers = 0
    winning_switchers = 0
    counter = 0
    num_doors = 4
    trials = 100000

    print('Simulating {} trials...'.format(trials))
    for i in range(trials):
        counter += 1
        # First, do a trial where the contestant never switches.
        won = simulate(num_doors, switch=False)
        if won:
            winning_non_switchers += 1
        y_axe_stay.append(winning_non_switchers / counter)

        # Next, try one where the contestant switches.
        won = simulate(num_doors, switch=True)
        if won:
            winning_switchers += 1
        y_axe_switch.append(winning_switchers / counter)
        x_axe.append(counter)

    print('    Switching won {0:5} times out of {1} ({2}% of the time)'.format(
            winning_switchers, 100,
            (winning_switchers / trials * 100 ) ))
    print('Not switching won {0:5} times out of {1} ({2}% of the time)'.format(
            winning_non_switchers, 100,
            (winning_non_switchers / trials * 100 ) ))



    plt.plot(x_axe, y_axe_switch, y_axe_stay)
    plt.ylabel('Erfolgswahrscheinlichkeit')
    plt.xlabel('Anzahl Simulationen')
    plt.show()

if __name__ == '__main__':
    main()