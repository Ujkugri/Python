from __future__ import print_function
import os
import random
import neat

win = 0
loose = 0
game = 1


def clear():
    if os.name == 'nt':
        os.system('CLS')
    if os.name == 'posix':
        os.system('clear')


def calc_hand(hand):
    global win, loose, game
    non_aces = [c for c in hand if c != 'A']
    aces = [c for c in hand if c == 'A']

    sum = 0

    for card in non_aces:
        if card in 'JQK':
            sum += 10
        else:
            sum += int(card)

    for card in aces:
        if sum <= 10:
            sum += 11
        else:
            sum += 1

    return sum


while True:
    cards = [
                '2', '3', '4', '5', '6', '7', '8', '9', '10', 'J', 'Q', 'K', 'A',
            ] * 4

    random.shuffle(cards)

    dealer = []
    player = [cards.pop(), cards.pop()]

    dealer.append(cards.pop())
    dealer.append(cards.pop())

    first_hand = True
    standing = False

    clear()
    print("\n\033[1;37;40mWELCOME TO BLACKJACK! Game #" + str(game) + "\n")
    print("-" * 30 + "\n")
    print("    \033[1;32;40mWINS:  \033[1;37;40m%s   \033[1;31;40mLOSSES:  \033[1;37;40m%s\n" % (win, loose))
    print("-" * 30 + "\n")

    while True:
        os.system('cls' if os.name == 'nt' else 'clear')

        player_score = calc_hand(player)
        dealer_score = calc_hand(dealer)

        if standing:
            print('Dealer Cards: [{}] ({})'.format(']['.join(dealer), dealer_score))
        else:
            print('Dealer Cards: [{}][?]'.format(dealer[0]))

        print('Your Cards:   [{}] ({})'.format(']['.join(player), player_score))
        print('')

        if standing:
            if dealer_score > 21:
                print('Dealer busted, you win!')
                win += 1
                game += 1
            elif player_score == dealer_score:
                print('Push, nobody wins')
                game += 1
            elif player_score > dealer_score:
                print('You beat the dealer, you win!')
                win += 1
                game += 1
            else:
                print('You lose :(')
                loose += 1
                game += 1
            print('')
            input('Play again? Hit enter to continue')
            break

        if first_hand and player_score == 21:
            print('Blackjack! Nice!')
            print('')
            input('Play again? Hit enter to continue')
            win += 1
            game += 1
            break

        if player_score > 21:
            print('You busted!')
            print('')
            input('Play again? Hit enter to continue')
            loose += 1
            game += 1
            break

        print('What would you like to do?')
        print(' [1] Hit')
        print(' [2] Stand')
        print(' [0] Quit')

        print('')
        choice = input('\033[1;37;40mYour choice: \n')
        print('')

        first_hand = False

        if choice == '1':
            player.append(cards.pop())
        elif choice == '2':
            standing = True
            while calc_hand(dealer) < 17:
                dealer.append(cards.pop())
        elif choice == '0':
            print('You played ' + str(game-1) + ' Games.')
            print('You´ve won ' + str(win) + ' Games and lost ' + str(loose) + ' Games.')
            quit()

        if game > 100:
            print('You played the Limit of' + str(game - 1) + ' Games.')
            print('You´ve won ' + str(win) + ' Games and lost ' + str(loose) + ' Games.')
            crash_info = [win, loose]
            quit()
