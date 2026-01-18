---
title: Random and Balanced Ability Scores
tags: ttrpg, dice
---

I came up with a method of rolling ability scores in games like D&D
that strikes a balance between being **random** and being **balanced**.

The usual ability scores are STR, DEX, CON, INT, WIS, CHA, ranging from 3 to 18.

My new method has these properties:

* The sum of the scores always **adds up to 63**.
* STR, DEX, and CON (the "physical" scores) are **independent** of each other.
* INT, WIS, and CHA (the "mental" scores) are **independent** of each other.
* The physical scores and the mental scores are inversely correlated with each other.

I don't consider the last property particularly desirable,
but I think it is a fine price to pay for the first property.

Other games solve this differently—DCC uses funnels, Into the Odd gives weak characters better gear—but I wanted randomness and balance in the stats themselves.

# The Method

## Step 1: roll

Roll nine six-sided dice and arrange them into a square.

For example:

<font size="20">
<table align="center" style="line-height: 0.7;" cellspacing="0" cellpadding="0">
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
   <td align="left"><span style="vertical-align: middle; font-size: 50%;">11</span></td>
   <td></td>
   <td><span style="vertical-align: middle; font-size: 50%;">STR</span></td>
 </tr>
 <tr>
   <td>⚃</td>
   <td>⚀</td>
   <td>⚂</td>
   <td>→</td>
   <td align="left"><span style="vertical-align: middle; font-size: 50%;">8</span></td>
   <td></td>
   <td><span style="vertical-align: middle; font-size: 50%;">DEX</span></td>
 </tr>
 <tr>
   <td>⚅</td>
   <td>⚄</td>
   <td>⚁</td>
   <td>→</td>
   <td align="left"><span style="vertical-align: middle; font-size: 50%;">13</span></td>
   <td></td>
   <td><span style="vertical-align: middle; font-size: 50%;">CON</span></td>
 </tr>
</table>
</font>

## Step 3: sum the **backs** of the columns

Use the sums of the **back faces** of the columns to assign values to INT, WIS, and CHA.

On a standard d6, opposite faces always sum to 7.
To find the back faces, pick up each column of dice (pinched between index finger and thumb) and flip it toward you 180°.


<font size="20">
<table align="center" style="line-height: 0.7;" cellspacing="0" cellpadding="0">
 <tr>
   <td>⚄</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚁</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚃</td>
   <td>&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</td>
   <td>⚁</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚄</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚂</td>
 </tr>
 <tr>
   <td>⚃</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚀</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚂</td>
   <td>&nbsp;&nbsp;⟳&nbsp;&nbsp;</td>
   <td>⚂</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚅</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚃</td>
 </tr>
 <tr>
   <td>⚅</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚄</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚁</td>
   <td></td>
   <td>⚀</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚁</td>
   <td>&nbsp;&nbsp;</td>
   <td>⚄</td>
 </tr>
 <tr>
   <td style="height: 10px;"></td>
   <td></td>
   <td></td>
   <td></td>
   <td></td>
   <td></td>
   <td></td>
   <td></td>
   <td></td>
   <td></td>
   <td></td>
 </tr>
 <tr>
   <td></td>
   <td></td>
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
   <td></td>
   <td></td>
   <td align="center" valign="middle"><font size="5">6</font></td>
   <td></td>
   <td align="center" valign="middle"><font size="5">13</font></td>
   <td></td>
   <td align="center" valign="middle"><font size="5">12</font></td>
 </tr>
</table>
</font>

## Result

In this example, the ability scores are:

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

# A Modification (for more randomness)

We can add even more variety by not always
pitting STR, DEX, and CON against INT, WIS, and CHA.

There are ten distinct ways to divide a set of six things in half.
Since the method is symmetric (rows and columns could be swapped),
we can assume STR is always in the first row.

Therefore you can roll a **d10** on the following table to randomly
pick one of the ways to divide the abilities in half.
The rightmost column is optional flavor—a sign you can use or ignore.

<table border=1>
 <tr>
   <th>d10</th>
   <th>rows</th>
   <th>columns</th>
   <th>sign</th>
 </tr>
 <tr align="center">
   <td>1</td>
   <td>&nbsp;STR, DEX, CON&nbsp;</td>
   <td>&nbsp;INT, WIS, CHA&nbsp;</td>
   <td>Wolf</td>
 </tr>
 <tr align="center">
   <td>2</td>
   <td>STR, DEX, INT</td>
   <td>CON, WIS, CHA</td>
   <td>Fox</td>
 </tr>
 <tr align="center">
   <td>3</td>
   <td>STR, DEX, WIS</td>
   <td>CON, INT, CHA</td>
   <td>Hawk</td>
 </tr>
 <tr align="center">
   <td>4</td>
   <td>STR, DEX, CHA</td>
   <td>CON, INT, WIS</td>
   <td>Stag</td>
 </tr>
 <tr align="center">
   <td>5</td>
   <td>STR, CON, INT</td>
   <td>DEX, WIS, CHA</td>
   <td>Bear</td>
 </tr>
 <tr align="center">
   <td>6</td>
   <td>STR, CON, WIS</td>
   <td>DEX, INT, CHA</td>
   <td>Boar</td>
 </tr>
 <tr align="center">
   <td>7</td>
   <td>STR, CON, CHA</td>
   <td>DEX, INT, WIS</td>
   <td>Lion</td>
 </tr>
 <tr align="center">
   <td>8</td>
   <td>STR, INT, WIS</td>
   <td>DEX, CON, CHA</td>
   <td>Owl</td>
 </tr>
 <tr align="center">
   <td>9</td>
   <td>STR, INT, CHA</td>
   <td>DEX, CON, WIS</td>
   <td>Serpent</td>
 </tr>
 <tr align="center">
   <td>10</td>
   <td>STR, WIS, CHA</td>
   <td>DEX, CON, INT</td>
   <td>Raven</td>
 </tr>
</table>
