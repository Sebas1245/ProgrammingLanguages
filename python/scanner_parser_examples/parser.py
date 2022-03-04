# Implementation of a parser
# Recognize expressions using grammar:
# EXP -> EXP op EXP | (EXP) | cte
# which was modified to remove ambiguity to:
# EXP  -> cte EXP1 | (EXP) EXP1
# EXP1 -> op EXP EXP1 | empty
# the lexical token (delimiters, constants, and operators)
# are recognized by the scanner
#
# Author: Dr. Santiago Conant

import sys
import get_token as scanner

# Match and get the next token
def match(expectedtoken):
    global token
    if token == expectedtoken:
        token = scanner.get_token()
    else:
        error("wrong token")

# Main function: implement parsing
def parser():
    global token 
    token = scanner.get_token() # initialize with the first token
    exp()
    if token == scanner.END:
        print("Well constructed expression!!")
    else:
        error("Expression wrongly terminated")

# Module that recognizes expressions
def exp():
    if token == scanner.INT or token == scanner.FLT:
        match(token) # match constants
        exp1()
    elif token == scanner.LRP:
        match(token) # match delimiter (
        exp()
        match(scanner.RRP)
        exp1()
    else:
        error("Expression badly started")

# Helper module for expression recognition
def exp1():
    if token == scanner.OPB:
        match(token) # match operator
        exp()
        exp1()

# End with an error message
def error(message):
    print("ERROR:", message)
    sys.exit(1)
    
        
