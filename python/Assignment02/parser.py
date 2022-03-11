import sys

import scanner

# Match and get the next token
def match(expectedtoken):
    global token
    if token == expectedtoken:
        token = scanner.get_token()
    else:
        error()

# Main function: implement parsing
def parser():
    global token 
    token = scanner.get_token() # initialize with the first token
    print(token)
    tarski()
    if token == scanner.END:
        print(">>CORRECT INPUT<<")
    else:
        error(">>SYNTAX ERROR<<")

def tarski():
    sentence()
    if token == scanner.END:
        match(token)
    else:
        print(">>SYNTAX ERROR<<")

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
        print("token matched quantifier")
        match(token)
        match(scanner.VAR)
        match(scanner.PRD)
        print("token match period")
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

def error():
    print(">>SYNTAX ERROR<<")
    sys.exit(1)

parser()