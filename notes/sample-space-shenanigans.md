---
title: sample space shenanigans
tags: probability, shenanigans
---

# Sample Space Shenanigans

## The Two Child Problem

[two child](https://en.wikipedia.org/wiki/Boy_or_girl_paradox)

###

```python
def two_child_a(trials):                                                                            
    bb, bg, gb, gg = 0,0,0,0                                                                        
                                                                                                    
    for _ in range(trials):                                                                         
        kid1 = flip()                                                                               
        kid2 = flip()                                                                               
        if kid1 == Coin.HEADS:                                                                      
            if kid2 == Coin.HEADS:                                                                  
                bb += 1                                                                             
            else:                                                                                   
                bg += 1                                                                             
        else:                                                                                       
            if kid2 == Coin.HEADS:                                                                  
                gb += 1                                                                             
            else:                                                                                   
                gg += 1                                                                             
    print("one girl", bg+gb+gg)                                                                     
    print("two girls", gg) 
```

```python
def two_child_b(trials):                                                                            
    one, two = 0,0                                                                                  
                                                                                                    
    for _ in range(trials):                                                                         
        kid1 = flip()                                                                               
        kid2 = flip()                                                                               
                                                                                                    
        kida, kidb = (kid1, kid2) if flip() else (kid2, kid1)                                       
        if kida == Coin.HEADS:                                                                      
            if kidb == Coin.HEADS:                                                                  
                two += 1                                                                            
            else:                                                                                   
                one += 1                                                                            
    print("one girl", one)                                                                          
    print("two girls", two) 
```


## Sleeping Beauty problem

[sleeping beauty](https://en.wikipedia.org/wiki/Sleeping_Beauty_problem)

[veritasium](https://www.youtube.com/watch?v=XeSu9fBJ2sI)

###

```python
def sleeping(trials):
    heads_mon, tails_mon, tails_tues = 0, 0, 0

    for _ in range(trials):
        if flip() == Coin.HEADS:
            heads_mon += 1
        else:
            tails_mon += 1
            tails_tues += 1

    print("heads mon", heads_mon)
    print("tails mon", tails_mon)
    print("tails tues", tails_tues)
```

## Appendix

```python
import random
from enum import Enum

Coin = Enum('Coin', ['HEADS', 'TAILS'])

def flip():
    return Coin.HEADS if random.randint(0, 1) == 0 else Coin.TAILS
```
