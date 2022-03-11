# Implementation of a scanner
import sys


# tokens
VAR = 100
CNT = 101
OBJ = 102
REL = 103
QTF = 104
NEG = 105
BOP = 106
ASG = 107 
END = 108
LPS = 109
RPS = 110
COM = 111
PRD = 112
ERR = 200

# Transition matrix: DFA coding
# [row, column] = [non-final state, transition]
# States > 99 are final (ACCEPTORS)
# Special case: State 200 = ERROR
#      
TRANSITION_MATRIX = [
    #   0   1   2     3   4  5   6   7   8   9  10   11  12  13  14  15  16   17 18  19  20  21  22 23  24   25   26  27
    #   let LET dig   _   @  C   T   D   L   R   F   B   U   E   ~   &   |    -   >   <   =   $  (   )   ,   .   spc rare
    [     1,  2,  7,  7,  3,  2,  2,  2,  2,  2,  2,  2,  2,  2,NEG,BOP,BOP, 4,   7,  5,ASG,END,LPS,RPS,COM,PRD,  0,  7], # state 0 - initial state 
    [     1,  7,  1,  1,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7, 7,   7,  7,VAR,VAR,VAR,VAR,VAR,VAR,VAR,  7], # state 1 - recieved undercase letter 
    [     7,  2,  2,  2,  7,  2,  2,  2,  2,  2,  2,  2,  2,  2,  7,  7,  7, 7,   7,  7,CNT,CNT,CNT,CNT,CNT,CNT,CNT,  7], # state 2 - recieved uppercase letter
    [     7,  7,  7,  7,  7,OBJ,OBJ,OBJ,REL,REL,REL,REL,QTF,QTF,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7], # state 3 - looking for objects, relations or quantifiers
    [     7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,BOP,  7,  7,  7,  7,  7,  7,  7,  7,  7], # state 4 - dash character
    [     7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  6,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7], # state 5 - < character
    [     7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,BOP,  7,  7,  7,  7,  7,  7,  7,  7,  7], # state 6 - dash after <
    [     7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,ERR,  7], # state 7 - error state
]

# Character filter: returns the column number of the transition array
# according to the given character
def filter(c):
    """Returns the column number associated with the given character type(c)"""
    if ord(c) >= ord('a') and ord(c) <= ord('z'): # lowercase letter
        return 0
    elif ord(c) >= ord('A') and ord(c) <= ord('Z'): # uppercase letter
        if c == 'C':
            return 5
        elif c == 'T':
            return 6
        elif c == 'D':
            return 7
        elif c == 'L':
            return 8
        elif c == 'R':
            return 9
        elif c == 'F':
            return 10
        elif c == 'B':
            return 11
        elif c == 'U':
            return 12
        elif c == 'E':
            return 13
        else:
            return 1
    elif c == '0' or c == '1' or c == '2' or \
       c == '3' or c == '4' or c == '5' or \
       c == '6' or c == '7' or c == '8' or c == '9': # digits
       return 2
    elif c == '_':
        return 3
    elif c == '@':
        return 4
    elif c == '~':
        return 14
    elif c == '&':
        return 15
    elif c == '|':
        return 16
    elif c == '-':
        return 17
    elif c == '>':
        return 18
    elif c == '<':
        return 19
    elif c == '=':
        return 20
    elif c == '$':
        return 21
    elif c == '(':
        return 22
    elif c == ')':
        return 23
    elif c == ',':
        return 24
    elif c == '.':
        return 25
    elif c == ' ' or ord(c) == 9 or ord(c) == 10 or ord(c) == 13: # white character
        return 26
    else:
        return 27

# Get token function: implement lexical analysis in a way that can be used by the parser
def get_token():
    """Implements a lexical analyzer: read characters from standard input"""
    state = 0 # state number in the automaton
    lexeme = "" # word that generates the token
    tokens = []
    read = True # indicates if it is required to read a character from standard input
    while (True):
        while state < 100:    # while the state is neither ACCEPTOR nor ERROR
            if read: c = sys.stdin.read(1)
            else: read = True
            state = TRANSITION_MATRIX[state][filter(c)]
            if state < 100 and state != 0: lexeme += c
        if state == VAR:    
            read = False # the next character has already been read
            print("[VAR]", lexeme)
            return VAR
        elif state == CNT:   
            read = False # the next character has already been read
            print("[CNT]", lexeme)
            return CNT
        elif state == OBJ:   
            lexeme += c  # the last character forms the lexeme
            print("[OBJ]", lexeme)
            return OBJ
        elif state == REL:   
            lexeme += c  # the last character forms the lexeme
            print("[REL]", lexeme)
            return REL
        elif state == QTF:  
            lexeme += c  # the last character forms the lexeme
            print("[QTF]", lexeme)
            return QTF
        elif state == NEG:
            lexeme += c  # the last character forms the lexeme
            print("[NEG]", lexeme)
            return NEG
        elif state == BOP:
            lexeme += c  # the last character forms the lexeme
            print("[BOP]", lexeme)
            return BOP
        elif state == ASG:
            lexeme += c  # the last character forms the lexeme
            print("[ASG]", lexeme)
            return ASG
        elif state == LPS:
            lexeme += c  # the last character forms the lexeme
            print("[LPS]", lexeme)
            return LPS
        elif state == RPS:
            lexeme += c  # the last character forms the lexeme
            print("[RPS]", lexeme)
            return RPS
        elif state == COM:
            lexeme += c  # the last character forms the lexeme
            print("[COM]", lexeme)
            return COM
        elif state == PRD:
            lexeme += c  # the last character forms the lexeme
            print("[PRD]", lexeme)
            return PRD
        elif state == ERR:   
            read = False # last character is not rare
            return ERR
        tokens.append(state)
        if state == END: 
            print("[END] $")
            return END
        lexeme = ""
        state = 0

# Main function: implement lexical analysis
def scanner():
    """Implements a lexical analyzer: read characters from standard input"""
    state = 0 # state number in the automaton
    lexeme = "" # word that generates the token
    tokens = []
    read = True # indicates if it is required to read a character from standard input
    while (True):
        while state < 100:    # while the state is neither ACCEPTOR nor ERROR
            if read: c = sys.stdin.read(1)
            else: read = True
            state = TRANSITION_MATRIX[state][filter(c)]
            if state < 100 and state != 0: lexeme += c
        if state == VAR:    
            read = False # the next character has already been read
            print("[VAR]", lexeme)
        elif state == CNT:   
            read = False # the next character has already been read
            print("[CNT]", lexeme)
        elif state == OBJ:   
            lexeme += c  # the last character forms the lexeme
            print("[OBJ]", lexeme)
        elif state == REL:   
            lexeme += c  # the last character forms the lexeme
            print("[REL]", lexeme)
        elif state == QTF:  
            lexeme += c  # the last character forms the lexeme
            print("[QTF]", lexeme)
        elif state == NEG:
            lexeme += c  # the last character forms the lexeme
            print("[NEG]", lexeme)
        elif state == BOP:
            lexeme += c  # the last character forms the lexeme
            print("[BOP]", lexeme)
        elif state == ASG:
            lexeme += c  # the last character forms the lexeme
            print("[ASG]", lexeme)
        elif state == LPS:
            lexeme += c  # the last character forms the lexeme
            print("[LPS]", lexeme)
        elif state == RPS:
            lexeme += c  # the last character forms the lexeme
            print("[RPS]", lexeme)
        elif state == COM:
            lexeme += c  # the last character forms the lexeme
            print("[COM]", lexeme)
        elif state == PRD:
            lexeme += c  # the last character forms the lexeme
            print("[PRD]", lexeme)
        elif state == ERR:   
            read = False # last character is not rare
            return ERR
        tokens.append(state)
        if state == END: 
            print("[END] $")
            print(">>CORRECT INPUT<<")
            return tokens
        lexeme = ""
        state = 0

scanner()