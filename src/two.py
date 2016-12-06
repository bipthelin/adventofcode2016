#!/usr/bin/env python
# -*- coding: utf-8 -*-
# --- Day 2: Bathroom Security ---
#
# You arrive at Easter Bunny Headquarters under cover of darkness. However,
# you left in such a rush that you forgot to use the bathroom! Fancy office
# buildings like this one usually have keypad locks on their bathrooms, so you
# search the front desk for the code.
#
# "In order to improve security," the document you find says, "bathroom codes
# will no longer be written down. Instead, please memorize and follow the
# procedure below to access the bathrooms."
#
# The document goes on to explain that each button to be pressed can be
# found by starting on the previous button and moving to adjacent buttons
# on the keypad: U moves up, D moves down, L moves left, and R moves right.
# Each line of instructions corresponds to one button, starting at the
# previous button (or, for the first line, the "5" button); press whatever
# button you're on at the end of each line. If a move doesn't lead to
# a button, ignore it.
#
# You can't hold it much longer, so you decide to figure out the code as
# you walk to the bathroom. You picture a keypad like this:
#
#    1 2 3
#    4 5 6
#    7 8 9
#    Suppose your instructions are:
#
#    ULL
#    RRDDD
#    LURDL
#    UUUUD
#    You start at "5" and move up (to "2"), left (to "1"), and left
#    (you can't, and stay on "1"), so the first button is 1.
#    Starting from the previous button ("1"), you move right twice (to "3")
#    and then down three times (stopping at "9" after two moves and
#    ignoring the third), ending up with 9.
#    Continuing from "9", you move left, up, right, down, and left,
#    ending with 8.
#    Finally, you move up four times (stopping at "2"), then down once,
#    ending with 5.
#    So, in this example, the bathroom code is 1985.
#
#    Your puzzle input is the instructions from the document you found at the
#    front desk. What is the bathroom code?
#
input = [ ("RDLRUUULRRDLRLLRLDDUDLULULDDULUDRRUURLRLLUULDURRULLRULDRRDLLULLRLL"
           "DRLDDRRRRLLRLURRRDRDULRDUULDDDULURUDDRRRUULUDRLLUUURLUDRUUUDRDUULL"
           "RLLUDDRURRDDDRDLUUURLRLLUDRURDUDUULDDLLRDURULLLURLDURLUUULDULDDULU"
           "LLLRRUDLRUURDRDLLURLUDULDUUUURRLDLUDRULUDLDLLDRLDDDRRLLDUDLLRRDDDR"
           "LUDURLLLDRUDDLDDRRLUDRRDUDLRRLULDULURULDULUULDRLLDRUUDDRLLUDRULLRR"
           "RLRDLRLUDLRULDRDLRDRLRULUDUURRUUULLDDDDUDDLDDDDRRULRDLRDDULLDLDLLD"
           "LLDLLDRRDDDRDDLRRDDDRLLLLURRDLRRLDRURDDURDULDDRUURUDUDDDRDRDDRLRRL"
           "RULLDRLDLURLRLRUDURRRDLLLUDRLRDLLDDDLLUDRLDRRUUDUUDULDULLRDLUDUURL"
           "DDRUDR")
        , ("URULDDLDDUDLLURLUUUUUULUDRRRDDUDURDRUURLLDRURLUULUDRDRLLDRLDULRULU"
           "URUURRLRRDRUUUDLLLLRUDDLRDLLDUDLLRRURURRRUDLRLRLLRULRLRLRDLRLLRRUD"
           "DRLRUDULDURDLDLLLRDRURURRULLLDLLRRDRLLDUUDLRUUDDURLLLDUUDLRDDURRDR"
           "RULLDRLRDULRRLLRLLLLUDDDRDRULRRULLRRUUDULRRRUDLLUUURDUDLLLURRDDUDL"
           "DLRLURDDRRRULRRUDRDRDULURULRUDULRRRLRUDLDDDDRUULURDRRDUDLULLRUDDRR"
           "RLUDLRURUURDLDURRDUUULUURRDULLURLRUUUUULULLDRURULDURDDRRUDLRLRRLLL"
           "LDDUURRULLURURRLLDRRDDUUDLLUURRDRLLLLRLUDUUUDLRLRRLDURDRURLRLRULUR"
           "LDULLLRRUUUDLLRRDUUULULDLLDLRRRDUDDLRULLULLULLULRU")
        , ("DURUUDULRRLULLLDDUDDLRRDURURRRDDRRURDRURDRLULDUDUDUULULDDUURDDULRD"
           "UDUDRRURDRDDRLDRDRLDULDDULRULLDULURLUUDUDULRDDRRLURLLRRDLLDLDURULU"
           "DUDULDRLLRRRUDRRDDDRRDRUUURLDLURDLRLLDUULLRULLDDDDRULRRLRDLDLRLUUR"
           "UUULRDUURURLRUDRDDDRRLLRLLDLRULUULULRUDLUDULDLRDDDDDRURDRLRDULRRUL"
           "RDURDDRRUDRUDLUDLDLRUDLDDRUUULULUULUUUDUULDRRLDUDRRDDLRUULURLRLULR"
           "URDDLLULLURLUDLULRLRRDDDDDRLURURURDRURRLLLLURLDDURLLURDULURUUDLURU"
           "URDLUUULLLLLRRDUDLLDLUUDURRRURRUUUDRULDDLURUDDRRRDRDULURURLLDULLRD"
           "DDRRLLRRRDRLUDURRDLLLLDDDDLUUURDDDDDDLURRURLLLUURRUDLRLRRRURULDRRL"
           "ULD")
        , ("LLUUURRDUUDRRLDLRUDUDRLRDLLRDLLDRUULLURLRRLLUDRLDDDLLLRRRUDULDLLLD"
           "RLURDRLRRLURUDULLRULLLURRRRRDDDLULURUDLDUDULRRLUDDURRLULRRRDDUULRU"
           "RRUULUURDRLLLDDRDDLRRULRDRDRLRURULDULRRDRLDRLLDRDURUUULDLLLRDRRRLR"
           "DLLUDRDRLURUURDLRDURRLUDRUDLURDRURLRDLULDURDDURUUDRLULLRLRLDDUDLLU"
           "UUURLRLRDRLRRRURLRULDULLLLDLRRRULLUUDLDURUUUDLULULRUDDLLDLDLRLDDUD"
           "URDRLLRRLRRDDUDRRRURDLRLUUURDULDLURULUDULRRLDUDLDDDUUDRDUULLDDRLRL"
           "LRLLLLURDDRURLDDULLULURLRDUDRDDURLLLUDLLLLLUDRDRDLURRDLUDDLDLLDDLU"
           "DRRDDLULRUURDRULDDDLLRLDRULURLRURRDDDRLUUDUDRLRRUDDLRDLDULULDDUDUR"
           "RRURULRDDDUUDULLULDDRDUDRRDRDRDLRRDURURRRRURULLLRRLR")
        , ("URLULLLDRDDULRRLRLUULDRUUULDRRLLDDDLDUULLDRLULRRDRRDDDRRDLRRLLDDRD"
           "ULLRRLLUDUDDLDRDRLRDLRDRDDUUDRLLRLULLULRDRDDLDDDRLURRLRRDLUDLDDDLR"
           "DLDLLULDDRRDRRRULRUUDUULDLRRURRLLDRDRRDDDURUDRURLUDDDDDDLLRLURULUR"
           "UURDDUDRLDRDRLUUUULURRRRDRDULRDDDDRDLLULRURLLRDULLUUDULULLLLRDRLLR"
           "RRLLRUDUUUULDDRULUDDDRRRULUDURRLLDURRDULUDRUDDRUURURURLRDULURDDDLU"
           "RRDLDDLRUDUUDULLURURDLDURRDRDDDLRRDLLULUDDDRDLDRDRRDRURRDUDRUURLRD"
           "DUUDLURRLDRRDLUDRDLURUDLLRRDUURDUDLUDRRL")
        ]

# _* Code =============================================================
# _ * API -------------------------------------------------------------
def move1(pad, m):
    if ((abs(m) == 3) and (pad+m > 0) and (pad+m < 10)):
        return pad + m
    elif ((abs(m) == 1) and ((pad % 3 == (abs(m)+m)/2) or (pad % 3 == 2))):
        return pad + m
    else:
        return pad

def get_key1(pad, key):
    for c in key:
        if c == 'U':
            pad = move1(pad, -3)
        elif c == 'R':
            pad = move1(pad, 1)
        elif c == 'D':
            pad = move1(pad, 3)
        elif c == 'L':
            pad = move1(pad, -1)

    return pad

def adjust_hex(num):
    if (num == 10):
        return 'A'
    elif (num == 11):
        return 'B'
    elif (num == 12):
        return 'C'
    elif (num == 13):
        return 'D'
    else:
        return num

def get_key2(pad, key):
    for c in key:
        if (pad > 5 and pad < 9) and c == 'U':
            pad = pad - 4
        elif (pad > 9 and pad < 13) and c == 'U':
            pad = pad - 4
        elif pad == 3 and c == 'U':
            pad = 1
        elif pad == 13 and c == 'U':
            pad = 11
        elif (pad > 1 and pad < 4) and c == 'R':
            pad = pad + 1
        elif (pad > 4 and pad < 9) and c == 'R':
            pad = pad + 1
        elif (pad > 9 and pad < 12) and c == 'R':
            pad = pad + 1
        elif (pad > 1 and pad < 5) and c == 'D':
            pad = pad + 4
        elif (pad > 5 and pad < 9) and c == 'D':
            pad = pad + 4
        elif pad == 11 and c == 'D':
            pad = 13
        elif pad == 1 and c == 'D':
            pad = 3
        elif (pad > 2 and pad < 5) and c == 'L':
            pad = pad - 1
        elif (pad > 5 and pad < 10) and c == 'L':
            pad = pad - 1
        elif (pad > 10 and pad < 13) and c == 'L':
            pad = pad - 1
        else:
            pad = pad

    return pad

def part1():
    answer = ""
    lastkey = 5

    for puzzlekey in input:
        lastkey = get_key1(lastkey, puzzlekey)
        answer += str(lastkey)

    return answer

def part2():
    answer = ""
    lastkey = 5

    for puzzlekey in input:
        lastkey = get_key2(lastkey, puzzlekey)
        answer += str(adjust_hex(lastkey))

    return answer

# _ * Main ------------------------------------------------------------
if __name__ == '__main__':
    print "part1 code: " + part1()
    print "part2 code: " + part2()

#_* Editor ===========================================================
# Local Variables:
# allout-layout: t
# vim: sw=4 ts=4 et
#
# End:
