---
title: Balanced and Odd
tags: ttrpg, dice
---

In a previous [post](/posts/2024-09-18--random-and-balanced-ability-scores.html)
I described to roll RPG ability scores that always add up to the same number.
It assumes that there are six ability scores.

Games such as *Into the Odd*, and those derived from it, such as *Mausritter* and *Cairn*,
have **three** ability scores: STR, DEX, and WIL.
Can we do something similar?
I found a method, but it is not nearly as elegant.

# The Method

## Step 1: roll

Roll four six-sided dice and arrange them into a line.

For example:

<font size="20">
<table align="center">
 <tr>
   <td>⚄</td>
   <td>⚃</td>
   <td>⚁</td>
   <td>⚂</td>
 </tr>
</table> 
</font>

## Step 2: sum the first three dice

<font size="20">
<table align="center">
 <tr>
   <td>⚄</td>
   <td>⚃</td>
   <td>⚁</td>
   <td>⚂</td>
 </tr>
 <tr>
   <td>↓</td>
   <td>↓</td>
   <td>↓</td>
   <td></td>
 </tr>
 <tr>
   <td>⚄</td>
   <td>⚃</td>
   <td>⚁</td>
   <td></td>
 </tr>
</table> 
</font>

\\[\\mathsf{STR} = 5+4+2 =11\\]

## Step 3: sum the BACKS of the last three dice

<font size="20">
<table align="center">
 <tr>
   <td>⚄</td>
   <td>⚃</td>
   <td>⚁</td>
   <td>⚂</td>
 </tr>
 <tr>
   <td></td>
   <td>⟳</td>
   <td>⟳</td>
   <td>⟳</td>
 </tr>
 <tr>
   <td></td>
   <td>⚂</td>
   <td>⚄</td>
   <td>⚃</td>
 </tr>
</table> 
</font>

\\[\\mathsf{DEX} = 3+5+4 =12\\]

## Step 4: add four to the remaining faces

<font size="20">
<table align="center">
 <tr>
   <td>⚄</td>
   <td>⚃</td>
   <td>⚁</td>
   <td>⚂</td>
 </tr>
 <tr>
   <td>⟳</td>
   <td></td>
   <td></td>
   <td>↓</td>
 </tr>
 <tr>
   <td>⚁</td>
   <td></td>
   <td></td>
   <td>⚂</td>
 </tr>
</table> 
</font>

\\[\\mathsf{WIL} = 2+3+4 =9\\]

The number 4 was chosen since the average value of a six-sided die is 3½.

## That's it!

In our running example, our ability scores are:

<table border=1>
 <tr>
   <th>&nbsp;&nbsp;STR&nbsp;&nbsp;</th>
   <th>&nbsp;&nbsp;DEX&nbsp;&nbsp;</th>
   <th>&nbsp;&nbsp;WIL&nbsp;&nbsp;</th>
 </tr>
 <tr align="center">
   <td>11</td>
   <td>12</td>
   <td>9</td>
 </tr>
</table> 

Notice that the sum of the three scores is 32.
It will always be 32!

# A Modification (for more randomness)

We can add even more variety by not always going in the order of: STR, DEX, WIL.
Notice that in the method described above, STR and DEX are more negatively correlated
than WIL is with either STR or DEX (STR and DEX share two dice).

There are exactly three ways to meaningfully reorder the score,
determined by which score is the one having 4 added to it
(like WIL above).

Therefore you can roll a d6 on the following table to randomly
pick an ordering of the scores:

<table border=1>
 <tr>
   <th>values</th>
   <th>order</th>
 </tr>
 <tr align="center">
   <td>1, 2</td>
   <td>&nbsp;STR, DEX, WIL&nbsp;</td>
 </tr>
 <tr align="center">
   <td>3, 4</td>
   <td>&nbsp;STR, WIL, DEX&nbsp;</td>
 </tr>
 <tr align="center">
   <td>5, 6</td>
   <td>&nbsp;WIL, DEX, STR&nbsp;</td>
 </tr>
</table> 
