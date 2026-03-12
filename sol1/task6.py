#!/bin/python3
import sys

# tested with following
# Valid words:
# aazend
# aaaaazend
# aeeazend
# aazzfend
# aazzaeeend
# aazxxend
# aaazfexend
# aeaazzaxend
# aaaeeazfexend
# aaaeeaazzafexxend

# Invalid words:
# azend
# aazzzend
# aazefend
# aazxeend
# aaend
# aazdend
# aazendz
# aazfnend
# aezend
# aazzaaend



class ParseException(Exception):
    pass

lookahead = None  # the index of the letter of the word we checking rn
input = None   # the whole word we are checking

# Check if the current lookahead matches a terminal without consuming it
def accept(terminal):
    return lookahead < len(input) and terminal == input[lookahead]

# Same check but for 1 index further lookahead than accept()
def accept_2(terminal):
    return lookahead + 1 < len(input) and terminal == input[lookahead + 1]

# increase the lookahead index by 1
def match(terminal):
    global lookahead
    if accept(terminal):
        lookahead += 1
    else:
        raise ParseException()

def S():
    if accept('a'):
        match('a')  # S → a A X a B
        A()
        X()
        match('a')
        B()
    else:
        raise ParseException()

def A():
    # prevent over comsuming a in case X is empty in AXaB (B -> zUZ)
    if accept('a') and not accept_2('z'):  # A → a A
        match('a')
        A()
    # else A → ε (do nothing)

def X():
    # prevent over comsuming e in case Y is empty in XYend
    if accept('e') and not accept_2('n'):  # X → e X
        match('e')
        X()
    # else X → ε (do nothing)

def B():
    if accept('z'):
        match('z')  # B → z U Z
        U()
        Z()
    else:
        raise ParseException()

def U():
    if accept('z'):  # U → z U'
        match('z')
        U_strich()
    # else U → ε

def U_strich():
    if accept('a'):  # U' → a
        match('a')
    # else U' → ε

def Z():
    F()          # Z → F X Y e n d
    X()
    Y()
    match('e')
    match('n')
    match('d')

def F():
    if accept('f'):  # F → f
        match('f')
    # else F → ε

def Y():
    if accept('x'):  # Y → x Y
        match('x')
        Y()
    # else Y → ε

def parse(word):
    """
    Parses the input word based on the starting symbol S
    """
    global lookahead
    global input

    lookahead = 0
    input = word

    try:
        S()
        if lookahead == len(input):
            return True
        else:
            return False
    except ParseException:
        return False


def main():
    S = A # set S to A

    if parse(sys.argv[1]):
        print(f"True")
        sys.exit(0)
    else:
        print(f"False")
        sys.exit(1)

if __name__ == '__main__':
    main()
