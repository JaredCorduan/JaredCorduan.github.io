---
title: Two six-room dungeons
tags: dnd, graphs
---

# Two six-room dungeons

This year there is an RPG game jam for making
[zungeons](https://playfulvoid.game.blog/2025/01/01/the-zungeon-manifesto-demystifying-dungeon-creation/),
which are a zine-inspired D&D dungeons.
[Marcia B's Bite-Sized Dungeons](https://traversefantasy.blogspot.com/2022/11/bite-sized-dungeons.html)
are recommended.

Marcia created a list of ten abstract dungeon layouts.
I found two other layouts that essentially "finish the collection".

This got me thinking about generalizations.


## Criterion

In the language of graph theory, Marcia is recommending that the abstract dungeon layouts be
connected, planar graphs with six vertices and six edges.

## Possible layouts

Marcia's post lists ten such layouts.
She mentions that hexagons also meet her criterion,
but that she isn't interested in plain loops.

It turns out that there are exactly 13 such layouts.
The two which do not appear on Marcia's blog are:

<p align="center">
![](/images/six-room-dungeons/graph.png)
</p>

As [maps](https://app.dungeonscrawl.com), these could look like:

<p align="center">
<img src="/images/six-room-dungeons/map.png" width="600" height="300"/>
</p>

So if we throw out hexagons,
we can randomly choose a layout with a [d12](https://g.co/kgs/hPc3tVb).

By "exactly 13 layouts", I mean up to graph-isomorphism.
This may not, however, be a good assumption in practice.
The following two graphs are graph-isomorphic,
but are arguably different enough as dungeons:

<p align="center">
![](/images/six-room-dungeons/graph-2.png)
</p>

## Resources for connected, planar graphs

The [house of graphs](https://houseofgraphs.org/meta-directory/planar)
contains a collection of connected, planar graphs in graph6 format.

If you download the collection of order 6 graphs, you can easily
find the ones with six edges using the
[showg](https://users.cecs.anu.edu.au/~bdm/data/formats.html) program.
The `-e` flag will display the number of edges:

```bash
$ showg planar_conn.6.g6 -e
```

Any graph whose second line is

```bash
6 6
```

will correspond to one of the abstract dungeon layouts.
So you can count them with:

```bash
$ showg planar_conn.6.g6 -e | grep "^6 6$" | wc -l
```

## Seven-room dungeons?

Connected, planar graphs make good sense as an abstract layout for a dungeon.
Six rooms/vertices was chosen as a sweet spot in game design.
With six vertices, five edges is enough connect the graph.
The sixth edge gives the dungeon a loop.

Every connected, planar graph that has the same number of vertices as edges
will always have exactly one loop.
More generally, Euler's formula says that:

\\[
v - e + f = 2 
\\]

where

* $v$ is the number of vertices
* $e$ is the number of edges
* $f$ is the number of faces (regions bounded by edges)

Note that one of the faces is always the outer,
infinitely large region.
Let's refer to the finite faces as "regions".

So if we want, say, a seven-room dungeon with two regions,
we would choose eight edges.

## How many layouts?

Using the data from 
the [house of graphs](https://houseofgraphs.org/meta-directory/planar):

<table border=1 align="center">
 <tr>
   <th>&nbsp;rooms&nbsp;</th>
   <th>&nbsp;regions&nbsp;</th>
   <th>&nbsp;total layouts&nbsp;</th>
 </tr>
 <tr align="center">
   <td>6</td>
   <td>1</td>
   <td>13</td>
 </tr>

 <tr align="center">
   <td>7</td>
   <td>1</td>
   <td>33</td>
 </tr>
 <tr align="center">
   <td>7</td>
   <td>2</td>
   <td>67</td>
 </tr>

 <tr align="center">
   <td>8</td>
   <td>1</td>
   <td>89</td>
 </tr>
 <tr align="center">
   <td>8</td>
   <td>2</td>
   <td>236</td>
 </tr>
 <tr align="center">
   <td>8</td>
   <td>3</td>
   <td>486</td>
 </tr>

 <tr align="center">
   <td>9</td>
   <td>1</td>
   <td>240</td>
 </tr>
 <tr align="center">
   <td>9</td>
   <td>2</td>
   <td>797</td>
 </tr>
 <tr align="center">
   <td>9</td>
   <td>3</td>
   <td>2,075</td>
 </tr>
 <tr align="center">
   <td>9</td>
   <td>4</td>
   <td>4,454</td>
 </tr>

 <tr align="center">
   <td>10</td>
   <td>1</td>
   <td>657</td>
 </tr>
 <tr align="center">
   <td>10</td>
   <td>2</td>
   <td>2,678</td>
 </tr>
 <tr align="center">
   <td>10</td>
   <td>3</td>
   <td>8,548</td>
 </tr>
 <tr align="center">
   <td>10</td>
   <td>4</td>
   <td>22,768</td>
 </tr>
 <tr align="center">
   <td>10</td>
   <td>5</td>
   <td>51,816</td>
 </tr>

 <tr align="center">
   <td>11</td>
   <td>1</td>
   <td>1,806</td>
 </tr>
 <tr align="center">
   <td>11</td>
   <td>2</td>
   <td>8,833</td>
 </tr>
 <tr align="center">
   <td>11</td>
   <td>3</td>
   <td>33,851</td>
 </tr>
 <tr align="center">
   <td>11</td>
   <td>4</td>
   <td>109,072</td>
 </tr>
 <tr align="center">
   <td>11</td>
   <td>5</td>
   <td>302,451</td>
 </tr>
 <tr align="center">
   <td>11</td>
   <td>6</td>
   <td>714,987</td>
 </tr>
</table> 
