import sys

import scanner

# Match and get the next token
def match(expectedtoken):
    global token
    if token == expectedtoken:
        token = scanner.get_token()
    else:
        print_error(">>LEXICAL ERROR<<")

# Main function: implement parsing
def parser():
    global token 
    token = scanner.get_token() # initialize with the first token
    tarski()
    if token == scanner.END:
        print(">>CORRECT INPUT<<")
    else:
        print_error(">>SYNTAX ")

def tarski():
    sentence()
    if token == scanner.END:
        match(token)
    else:
        print_error(">>SYNTAX ERROR<<")

def sentence():
    if token == scanner.OBJ:
        match(token)
        match(scanner.LPS)
        term()
        match(scanner.RPS)
        sentence1()
    elif token == scanner.LPS:
        match(token)
        sentence()
        match(scanner.RPS)
        sentence1()
    elif token == scanner.NEG:
        match(token)
        sentence()
        sentence1()
    elif token == scanner.QTF:
        match(token)
        match(scanner.VAR)
        match(scanner.PRD)
        sentence()
        sentence1()
    else:
        term()
        match(scanner.ASG)
        term()
        sentence1()

def sentence1():
    if token == scanner.BOP:
        match(token)
        sentence()
        sentence1()

def term():
    if token == scanner.VAR:
        match(token)
    elif token == scanner.CNT:
        match(token)

def print_error(err_message):
    print(err_message)
    sys.exit(1)

parser()