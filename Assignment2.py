# -*- coding: utf-8 -*-
"""
Created on Sun Aug 27 22:07:39 2017

@author: lei
"""
__card_order__ = ['ace','2','3','4','5','6','7','8','9','10','jack','queen','king']
__suit__ = ['heart','diamond','spade','club']
import random
class card():
    def __init__(self,card_suit='',card_number=''):
        assert (card_number in __card_order__ and card_suit in __suit__) or (card_number == ''), 'Invalid Card'
        self.number = card_number
        self.suit = card_suit
        self.colour = 'red' if card_suit in ['heart','diamond'] else 'black'
        
    def __str__(self):
        return self.suit[0] + (self.number[0] if self.number != '10' else '10')
     
    def __lt__(self,other):
        return (__card_order__.index(other.number) - __card_order__.index(self.number) == 1) and \
        self.colour != other.colour
        
    def __gt__(self,other):
        return (__card_order__.index(self.number) - __card_order__.index(other.number) == 1) and \
        self.colour != other.colour
    
    def __eq__(self,other):
        return self.number == other.number and self.suit == other.suit
        
    def get_suit(self):
        return self.suit
        
    def get_colour(self):
        return self.colour
    
    def get_face(self):
        return self.number
        
    def get_number(self):
        if self.number == 'ace':
            return 1
        elif self.number == 'jack':
            return 11
        elif self.number == 'queen':
            return 12
        elif self.number == 'king':
            return 13
        else:
            return int(self.number)
        
    def is_ace(self):
        if self.number == 'ace':
            return True
        else:
            return False
    
    def is_king(self):
        if self.number == 'king':
            return True
        else:
            return False
    
    def is_none(self):
        return self.suit == '' and self.number == ''
        
    def oneup(self,other):
        if self.suit == other.suit and \
        (__card_order__.index(self.number) - __card_order__.index(other.number) == 1):
            return True
        else:
            return False
            
class deck():  
    def __init__(self,start,end,no_of_suit,**kwargs):
                
        self.list = []        
        random_suit_list = []
        assert no_of_suit <= 4, 'no of suit must be less than 5'
        for i in random.sample([0,1,2,3],no_of_suit):
            random_suit_list.append(__suit__[i])
        for key, value in kwargs.items():        
            if key == 'given_suit' and value != '':
                random_suit_list = []
                random_suit_list.append(value)
        for suit in random_suit_list:
            for i in range(start,end):
                newcard = card(suit,__card_order__[i])   
                self.list.append(newcard)
    
    def __str__(self):
        temp = ''
        for i in self.list:
            temp = temp + (str(i) + '\n')
        return temp
    
    def __iter__(self):
        return iter(self.list)
        
    def __len__(self):
        return len(self.list)
    
    def __contains__(self, card):
        return card in self.list 
        
    def __getitem__(self, key):
        return self.list[key]
        
    def is_empty(self):
        if len(self) == 0:
            return True
        else:
            return False
            
    def shuffle(self):
        random.shuffle(self.list)
    
    def add(self,newcard):
        self.list.append(newcard)
        
    def draw(self):
        if not self.is_empty():
            tempcard = self.list[len(self.list) - 1]
            del self.list[len(self.list) - 1]
            return tempcard
        else:
            return card()
    
    def top(self):
        if len(self.list) > 0:
            return self.list[-1]
        else:
            return card()
            
class freecell():
    def __init__(self):
        mydeck = deck(0,13,4)
        mydeck.shuffle()
        no_of_decks = 8
        self.gamedeck = []
        for i in range(no_of_decks):
            self.gamedeck.append(deck(0,0,0))
        
        for i in range(len(mydeck)):
            if i < 7:
                self.gamedeck[0].add(mydeck[i])
            elif i < 14:
                self.gamedeck[1].add(mydeck[i])
            elif i < 21:
                self.gamedeck[2].add(mydeck[i])
            elif i < 28:
                self.gamedeck[3].add(mydeck[i])
            elif i < 34:
                self.gamedeck[4].add(mydeck[i])
            elif i < 40:
                self.gamedeck[5].add(mydeck[i])
            elif i < 46:
                self.gamedeck[6].add(mydeck[i])
            else:
                self.gamedeck[7].add(mydeck[i])
        self.foundations = []
        self.reserves = []
    
    def __str__(self):
        max_len = 0
        temp = ''
        for i in range(len(self.gamedeck)):
            if len(self.gamedeck[i]) > max_len:
                max_len = len(self.gamedeck[i])
        for i in range(max_len):
            for deck in self.gamedeck:
                if len(deck) > i:
                    temp = temp + str(deck[i]) + '\t'
                else:
                    temp = temp + '      ' + '\t'
            temp = temp + '\n'
        
        temp = temp + 'Foundations: '
        for card in self.foundations:
            temp = temp + str(card) + '\t'
        temp = temp + '\n'
        temp = temp + 'Reserves: '
        for card in self.reserves:
            temp = temp + str(card) + '\t'
        temp = temp + '\n'
        return temp
            
    def move_to_reserve(self,mycard):
        if len(self.reserves) >= 4:
            print('Reserve is Full')
            return False
        self.reserves.append(mycard)
        return True
        
    def move_to_deck(self,mycard,deck_no):
        if self.gamedeck[deck_no].top().is_none():
            self.gamedeck[deck_no].add(mycard)
        elif mycard < self.gamedeck[deck_no].top():
            self.gamedeck[deck_no].add(mycard)
            return True
        else:
            print('Invalid move')
            return False
    
    def move_from_reserve(self,reserve_no):
        try:
            tempcard = self.reserves[reserve_no]       
            del self.reserves[reserve_no]
            return tempcard
        except:
            print('No Card in Reserve ' + str(reserve_no + 1))
            return card()
    
    def move_from_deck(self,deck_no):
        if len(self.gamedeck[deck_no]) == 0:
            print('No Cards on Deck ' + str(deck_no))            
            return card()
        return self.gamedeck[deck_no].draw()
            
    def move_to_foundation(self,mycard,foundation_no = 0):
        if mycard.is_ace():
            self.foundations.append(mycard)
            return True
        else:
            try:
                if mycard.oneup(self.foundations[foundation_no]):
                    self.foundations[foundation_no] = mycard
                    return True
                else:
                    print('Invalid move')
                    return False
            except:
                print('Invalid move')
                return False
    
    def won(self):
        count = 0
        for mycard in self.foundations:
            if mycard.is_king():
                count += 1
        if count == 4:
            return True
        else:
            return False
            
def main():
    mygame = freecell()
    print(mygame)
    while not mygame.won():
        print('Where do you want to draw card?')
        valid_move = False
        mycard = None
        while valid_move == False:
            move1 = int(input('1. Deck\t2. Reserve: '))
            if move1 == 1:
                position1 = int(input('Please enter position of deck from 1 to 8: '))
                assert position1 < 9 and position1 > 0, 'Invalid Selection'
                mycard = mygame.move_from_deck(position1 - 1)
            elif move1 == 2:
                position1 = int(input('Please enter position of reserve from 1 to 4: '))
                assert position1 < 5 and position1 > 0, 'Invalid Selection'
                mycard = mygame.move_from_reserve(position1 - 1)
            else:
                mycard = card()
            if not mycard.is_none():
                valid_move = True
            else:
                print('Invalid Selection')
                
        print('You have card ' + str(mycard))
        valid_move = False
        print('Where do you want to put the card?')
        while valid_move == False:        
            move2 = int(input('1. Deck\t2. Foundation\t3.Reserve: '))
            if move2 == 1:
                position2 = int(input('Please enter position of deck from 1 to 8: '))
                assert position1 < 9 and position1 > 0, 'Invalid Selection'
                valid_move = mygame.move_to_deck(mycard,position2 - 1)
            elif move2 == 2:
                if mycard.is_ace():
                    valid_move = mygame.move_to_foundation(mycard)
                else:
                    position2 = int(input('Please enter position of Foundation from 1 to 4: '))
                    assert position1 < 5 and position1 > 0, 'Invalid Selection'
                    valid_move = mygame.move_to_foundation(mycard,position2 - 1)
            elif move2 == 3:
                valid_move = mygame.move_to_reserve(mycard)
            else:
                print('Invalid Selection')
        print('\n' + 'Current deck after move')
        print(mygame)
        
    print('Congratualations you have won!')

if __name__ == '__main__':
    main()
