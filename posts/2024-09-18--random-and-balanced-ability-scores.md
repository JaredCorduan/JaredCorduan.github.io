---
title: Random and Balanced Ability Scores
tags: ttrpg, dice
---

Many tabletop role-playing games make heavy use of six ability scores.
Dungeons & Dragons uses these six:

* Strength (STR)
* Dexterity (DEX)
* Constitution (CON)
* Wisdom (WIS)
* Intelligence (INT)
* Charisma (CHA)

These scores range between 3 and 18,
and represent how capable an individual is in that area,
where a higher score is better.

There are many different ways of generating these scores.
This post describes a method that I invented and which has these properties:

* The sum of the scores always adds up to 63.
* STR, DEX, and CON (the "physical" scores) are independent of each other.
* INT, WIS, and CHA (the "mental" scores) are independent of each other.
* The physical scores and the mental scores are inversely correlated with eath other.

I don't consider the last property particularly desirable,
but I think it is a fine price to pay for the first property.

## Outline

* describe the method
* describe a small optional modification
* context and motivation
* prior art

# The Method

## Step 1: roll

Roll nine six-sided dice and arrange them into a square.

For example:

<font size="20">
<table align="center">
 <tr>
   <td>⚄</td>
   <td>⚁</td>
   <td>⚃</td>
 </tr>
 <tr>
   <td>⚃</td>
   <td>⚀</td>
   <td>⚂</td>
 </tr>
 <tr>
   <td>⚅</td>
   <td>⚄</td>
   <td>⚁</td>
 </tr>
</table> 
</font>

## Step 2: sum the rows

Use the sums of the rows to assign values to STR, DEX, and CON.

<font size="20">
<table align="center">
 <tr>
   <td>⚄</td>
   <td>⚁</td>
   <td>⚃</td>
   <td>→</td>
   <td>11</td>
   <td></td>
   <td>STR</td>
 </tr>
 <tr>
   <td>⚃</td>
   <td>⚀</td>
   <td>⚂</td>
   <td>→</td>
   <td align="right">8</td>
   <td></td>
   <td>DEX</td>
 </tr>
 <tr>
   <td>⚅</td>
   <td>⚄</td>
   <td>⚁</td>
   <td>→</td>
   <td>13</td>
   <td></td>
   <td>CON</td>
 </tr>
</table> 
</font>

## Step 3: sum the BACKS of the columns

Use the sums of the **back faces** of the columns to assign values to INT, WIS, and CHA.

Note that this can be done with a very natural hand motion:
grab a column from the square with your index finger and thumb, then rotate the column so that you can see the back faces.


<font size="20">
<table align="center">
 <tr>
   <td>⚄</td>
   <td>⚁</td>
   <td>⚃</td>
   <td></td>
   <td>⚁</td>
   <td></td>
   <td>⚄</td>
   <td></td>
   <td>⚂</td>
 </tr>
 <tr>
   <td>⚃</td>
   <td>⚀</td>
   <td>⚂</td>
   <td>&nbsp;&nbsp;&nbsp;&nbsp;⟳&nbsp;&nbsp;&nbsp;&nbsp;</td>
   <td>⚂</td>
   <td></td>
   <td>⚅</td>
   <td></td>
   <td>⚃</td>
 </tr>
 <tr>
   <td>⚅</td>
   <td>⚄</td>
   <td>⚁</td>
   <td></td>
   <td>⚀</td>
   <td></td>
   <td>⚁</td>
   <td></td>
   <td>⚄</td>
 </tr>
 <tr>
   <td></td>
   <td></td>
   <td></td>
   <td></td>
   <td>↓</td>
   <td></td>
   <td>↓</td>
   <td></td>
   <td>↓</td>
 </tr>
 <tr>
   <td></td>
   <td></td>
   <td></td>
   <td></td>
   <td>6</td>
   <td>&nbsp;&nbsp;&nbsp;</td>
   <td>13</td>
   <td>&nbsp;&nbsp;</td>
   <td>12</td>
 </tr>
</table> 
</font>

## That's it!

In our running example, our ability scores are:

<table border=1>
 <tr>
   <th>&nbsp;&nbsp;STR&nbsp;&nbsp;</th>
   <th>&nbsp;&nbsp;DEX&nbsp;&nbsp;</th>
   <th>&nbsp;&nbsp;CON&nbsp;&nbsp;</th>
   <th>&nbsp;&nbsp;INT&nbsp;&nbsp;</th>
   <th>&nbsp;&nbsp;WIS&nbsp;&nbsp;</th>
   <th>&nbsp;&nbsp;CHA&nbsp;&nbsp;</th>
 </tr>
 <tr align="center">
   <td>11</td>
   <td>8</td>
   <td>13</td>
   <td>6</td>
   <td>13</td>
   <td>12</td>
 </tr>
</table> 

Notice that the sum of the six scores is 63.

## In Code (Python)

```python
import random

def d6():
    return random.randint(1, 6)

def sum_row(dice, row):
    return sum([dice[row][i] for i in range(3)])

def sum_col(dice, col):
    return sum([dice[i][col] for i in range(3)])

dice = [(d6(), d6(), d6()) for _ in range(3)]

scores = {
    "str": sum_row(dice, 0),
    "dex": sum_row(dice, 1),
    "con": sum_row(dice, 2),
    "int": 21 - sum_col(dice, 0),
    "wis": 21 - sum_col(dice, 1),
    "cha": 21 - sum_col(dice, 2)
}
```

# A Modification (for more randomness)

We can add even more variety by not always
pitting STR, DEX, and CON against INT, WIS, and CHA.

There are ten distinct ways to divide a set of six things in half.
Note that picking STR, DEX, CON for the rows is equivalent to picking
those same abilities for the columns, so without loss of generality
we can assume that STR is always the first row.

Therefore you can roll a d10 on the following table to randomly
pick one of the ways to divide the abilities in half.
For fun, I've also included an elemental affinity for each roll.

<table border=1>
 <tr>
   <th>d10</th>
   <th>rows</th>
   <th>columns</th>
   <th>element</th>
 </tr>
 <tr align="center">
   <td>1</td>
   <td>&nbsp;STR, DEX, CON&nbsp;</td>
   <td>&nbsp;INT, WIS, CHA&nbsp;</td>
   <td>Fire</td>
 </tr>
 <tr align="center">
   <td>2</td>
   <td>STR, DEX, INT</td>
   <td>CON, WIS, CHA</td>
   <td>Water</td>
 </tr>
 <tr align="center">
   <td>3</td>
   <td>STR, DEX, WIS</td>
   <td>CON, INT, CHA</td>
   <td>Earth</td>
 </tr>
 <tr align="center">
   <td>4</td>
   <td>STR, DEX, CHA</td>
   <td>CON, INT, WIS</td>
   <td>Air</td>
 </tr>
 <tr align="center">
   <td>5</td>
   <td>STR, CON, INT</td>
   <td>DEX, WIS, CHA</td>
   <td>Lightning</td>
 </tr>
 <tr align="center">
   <td>6</td>
   <td>STR, CON, WIS</td>
   <td>DEX, INT, CHA</td>
   <td>Light</td>
 </tr>
 <tr align="center">
   <td>7</td>
   <td>STR, CON, CHA</td>
   <td>DEX, INT, WIS</td>
   <td>Shadow</td>
 </tr>
 <tr align="center">
   <td>8</td>
   <td>STR, INT, WIS</td>
   <td>DEX, CON, CHA</td>
   <td>Poison</td>
 </tr>
 <tr align="center">
   <td>9</td>
   <td>STR, INT, CHA</td>
   <td>DEX, CON, WIS</td>
   <td>Void</td>
 </tr>
 <tr align="center">
   <td>10</td>
   <td>STR, WIS, CHA</td>
   <td>DEX, CON, INT</td>
   <td>Chaos</td>
 </tr>
</table> 


# Context and Motivation

Two common, but very different ways of generating ability scores are:

1. **3d6 down the line** - for each ability score, roll three six-sided dice and record the sum.
2. **assignment** - assign each of the numbers 15, 14, 13, 12, 10, and 8 to one of the abilities.

Whichever method you use is likely influenced by the
[culture of play](https://retiredadventurer.blogspot.com/2021/04/six-cultures-of-play.html)
for a given game and group of friends.

If you are planning on investing a lot of time and energy into playing and advancing your character,
the assignment method saves you from the possibility that you get stuck with a weak character.
If you have an archetype in mind for the character you want to play before you roll,
it is especially important that that archetype's key abilities get the higher scores.
Let's call this "Tolkien style", with the idea that the players want to go on a heroic and epic adventure.
This style of play prefers a more involved character creation process.

If, however, the goal is to seek out risky adventures and to be surprised by what happens,
the 3d6-down-the-line method is a natural fit.
With the right mindset, the potential for the characters to die at anytime can add to the excitement.
Let's call this "Vance style", with the idea that the players are happy to go down in a blaze of glory.
This style of play prefers a quick character creation process.

Of course, neither style exists as an absolute.
Contrasting these two styles of play is just one of many ways to compare different experiences.
Some games cater more to one of these styles than the other, but it is ultimately all what the
players make of it.
If I want a Tolkien style game, I'm likely to chose something like Pathfinder or Traveler.
If I want a Vance style game, I'm likely to chose something like Dungeon Crawl Classics.

I came up with the method of assigning ability scores in this post to see if I could
find a method that scratched both itches at the same time.
I wanted something that felt very random, but also always fair.

# Prior Art

Plenty of games have tried to solve the problem of balancing randomness with fairness.
Here are my two favorites.

## Dungeon Crawl Classics

Dungeon Crawl Classics is a game that encourages everyone to roll "3d6 down the line".
Rolling characters with low scores is not a big deal due the game's use of "funnels".

A funnel is a game session where all of the players create several characters,
usually between three and five. 
The session needs to be brutally hard, so that most of each player's characters are gone by the end.
The weak characters will likely not survive, and the ones that do survive are hard not to love.
In general, the funnel fosters the game's general attitude that losing characters is all a part of the fun.

## Into the Odd

Into the Odd also supports a fully random ability score generation.
Rolling characters with low scores is not a big deal due to the fact that
weak characters are compensated with good gear.
Gear makes a big difference in this game.
In fact, character progression is as much about accumulating items as it is gaining levels.

