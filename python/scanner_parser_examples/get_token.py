# -*- coding: utf-8 -*-

# Implementation of a scanner by encoding a Deterministic
# Finite Automaton as a Transition Matrix
# Author: Dr. Santiago Conant

import sys

# tokens
INT = 100  # Integer number
FLT = 101  # Floating point number
OPB = 102  # Binary operator
LRP = 103  # Delimiter: left parenthesis
RRP = 104  # Delimiter: right parenthesis
END = 105  # End of input
ERR = 200  # Lexical error: unknown word

# Transition matrix: DFA coding
# [row, column] = [non-final state, transition]
# States > 99 are final (ACCEPTORS)
# Special case: State 200 = ERROR
#      dig  op  (   ) rare spc  .   $
MT = [[  1,OPB,LRP,RRP,  4,  0,  4,END], # state 0 - initial state
      [  1,INT,INT,INT,  4,INT,  2,INT], # state 1 - integer digits
      [  3,  4,  4,  4,  4,ERR,  4,  4], # state 2 - first floating decimal
      [  3,FLT,FLT,FLT,  4,FLT,  4,FLT], # state 3 - float remaining decimals
      [  4,  4,  4,  4,  4,ERR,  4,  4]] # state 4 - error state

# Character filter: returns the column number of the transition array
# according to the given character
def filter(c):
    """Returns the column number associated with the given character type(c)"""
    if c == '0' or c == '1' or c == '2' or \
       c == '3' or c == '4' or c == '5' or \
       c == '6' or c == '7' or c == '8' or c == '9': # digits
        return 0
    elif c == '+' or c == '-' or c == '*' or \
         c == '/': # operators
        return 1
    elif c == '(': # delimiter (
        return 2
    elif c == ')': # delimiter )
        return 3
    elif c == ' ' or ord(c) == 9 or ord(c) == 10 or ord(c) == 13: # white character
        return 5
    elif c == '.': # dot
        return 6
    elif c == '$': # end of input
        return 7
    else: # rare character
        return 4

_c = None    # next character
_leer = True # indicates if it is required to read a character from the standard input

# Main function: implement lexical analysis
def get_token():
    """Implements a lexical analyzer: read characters from standard input"""
    global _c, _leer
    edo = 0 # state number in the automaton
    lexema = "" # word that generates the token
    while (True):
        while edo < 100:    # while the state is neither ACCEPTOR nor ERROR
            if _leer: _c = sys.stdin.read(1)
            else: _leer = True
            edo = MT[edo][filter(_c)]
            if edo < 100 and edo != 0: lexema += _c
        if edo == INT:    
            _leer = False # the next character has already been read
            print("Integer", lexema)
            return INT
        elif edo == FLT:   
            _leer = False # the next character has already been read
            print("Float", lexema)
            return FLT
        elif edo == OPB:   
            lexema += _c  # the last character forms the lexeme
            print("Operator", lexema)
            return OPB
        elif edo == LRP:   
            lexema += _c  # the last character forms the lexeme
            print("Delimiter", lexema)
            return LRP
        elif edo == RRP:  
            lexema += _c  # the last character forms the lexeme
            print("Delimiter", lexema)
            return RRP
        elif edo == END:
            print("End of expression")
            return END
        else:   
            leer = False # last character is not rare
            print("ERROR! illegal word", lexema)
            return ERR
            
        
    

