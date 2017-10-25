# -*- coding: utf-8 -*-
"""
Created on Sat Oct 21 16:32:08 2017

@author: lei
"""

from Assignment2 import deck
from Assignment2 import card
royal = ['10','jack','king','queen','ace']
def test1(newDeck):
    NUMBER_OF_TRIALS = 1000
    total = 0
    for i in range(0,NUMBER_OF_TRIALS):
        newCard= newDeck.draw()
        total += int(newCard.get_number())
        newDeck.add(newCard)
        newDeck.shuffle()
        
    print('Average card face value out of', NUMBER_OF_TRIALS, 'trials is',total / NUMBER_OF_TRIALS)

def draw5(newDeck):
    for i in range(0,5):
        newCard = newDeck.draw()
        if not(newCard.get_face() in royal and newCard.get_suit() == 'heart'):
            return False
    return True
        
def test2():
    NUMBER_OF_SUCCESS = 100
    trial = 0
    for i in range(0,NUMBER_OF_SUCCESS):
        success = False
        while not success:
            newDeck = deck(0,13,4)
            newDeck.shuffle()
            trial += 1
            while not newDeck.is_empty():
                found = draw5(newDeck)
                if found == True:
                    success = True  
                    break
        print(trial,i)
    print(NUMBER_OF_SUCCESS / trial)
    
def main():
    
    newDeck = deck(0,13,4)
    newDeck.shuffle()
    
    #print(newDeck)
    test1(newDeck)

    test2()
        
if __name__ == '__main__':
    main()