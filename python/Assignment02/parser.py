# Implementation of a parser for Tarsk's World
# Authors:
# Sebastián Saldaña A01570274, Ana Elisa Estrada A01251091 and Estefanía Charles A01283472

import sys

import get_token as scanner

# Match and get the next token
def match(expectedtoken):
    global token, lexeme
    if token == expectedtoken:
        print_token(token, lexeme)
        token, lexeme = scanner.get_token()
    else:
        print_error(">>SYNTAX ERROR<<")

# Main function: implement parsing
def parser():
    global token, lexeme
    token, lexeme = scanner.get_token()# initialize with the first token
    sentence()
    if token == scanner.END:
        print(">>CORRECT INPUT<<")
    else:
        print_error(">>SYNTAX ERROR<<")

def sentence():
    if token == scanner.OBJ:
        match(token)
        match(scanner.LPS)
        term()
        match(scanner.RPS)
        sentence1()
    elif token == scanner.REL:
        match(token)
        match(scanner.LPS)
        term()
        match(scanner.COM)
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

def print_token(token, lexeme=" "):
    if token == scanner.VAR:    
        print("[VAR]", lexeme)
    elif token == scanner.CNT:   
        print("[CNT]", lexeme)
    elif token == scanner.OBJ:   
        print("[OBJ]", lexeme)
    elif token == scanner.REL:   
        print("[REL]", lexeme)
    elif token == scanner.QTF:  
        print("[QTF]", lexeme)
    elif token == scanner.NEG:
        print("[NEG]", lexeme)
    elif token == scanner.BOP:
        print("[BOP]", lexeme)
    elif token == scanner.ASG:
        print("[ASG]", lexeme)
    elif token == scanner.LPS:
        print("[LPS]", lexeme)
    elif token == scanner.RPS:
        print("[RPS]", lexeme)
    elif token == scanner.COM:
        print("[COM]", lexeme)
    elif token == scanner.PRD:
        print("[PRD]", lexeme)
    elif token == scanner.END:
        print("[END]", lexeme)

parser()