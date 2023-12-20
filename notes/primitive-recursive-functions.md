---
title: Primitive Recursive Functions
tags: computability theory
---

# The Primitive Recursive Functions

The primitive recursive functions are a collection of functions that continue to intrigue me.
They have been studied extensively, and what follows is just my personal curation
and exposition.

I like to call this collection of functions, tongue-in-cheek, the first
total functional programming language.
They where first defined in 1919 and they played a pivotal role the formalization of the
notion of algorithm.
They are also interesting from the point of view of formal logic.

## Background and inception

The ancient Greeks drew a distinction between *actual infinity* and *potential infinity*.
The distinction may sound like hair-splitting at first, but it turns out to be fruitful.
Actual infinity involves the existence of infinite objects,
while potential infinity involves only an unending processes.

The ancient Greeks were comfortable with potential infinity, but not with actual infinity.
Aristotle is often attributed with inventing the slogan "infinitum actu non datur",
or "infinity is not actually given".
As an example, wielding the set of natural numbers as an object of mathematical inquiry requires
actual infinity, while defining the natural numbers with the usual inductive definition
requires only potential infinity.
Mathematicians got along just fine with potential infinity for a long time.

The reluctance to embrace actual infinity changed drastically in the 1800s
when in became clear that individual real numbers require actual infinity.
At the same time, it became clear how unintuitive infinite set can be.
Georg Cantor and his explorations of trigonometric series led the math world into a renewed
[debate](https://www.smbc-comics.com/comic/how-math-works) about actual infinity.

Amidst these changes and uncertainty grew an increased desire to place mathematics
on more solid foundations.
In particular, many mathematicians sought to make *reasoning* about infinite objects
reducible to finite methods, and to free the structure of mathematical proofs from 
inifinite structures.
Out of this investigation came the idea of the modern computer.

First-order logic was invented in the late 1800s, independently by Charles Sanders Peirce and
Gottlob Frege.
Frege was attempting to ground mathematics in first-order logic, 
but in 1901 Bertrand Russell published a major flaw to his system.
The insight is now known as Russell's paradox, even though it was known to Zermelo two years earlier.

Russel went on, together with Alfred Whitehead, to describe their own logical foundations of
mathematics, which was published as *Principia Mathematica*.

The backdrop is now set for Thoralf Skolem's primitive recursive functions.
The quantifiers in first-order logic, embodying the phrases "for all" and "there exists",
involves symbols whose variables can range over actual infinities.

In his paper "The foundations of elementary arithmetic established by means of the recursive
mode of thought, without the use of apparent variables ranging over infinite domains"[^skolem],
Skolem defines, by way of examples, the primitive recursive functions.
These functions, Skolem argues, provide an alternate foundation for the mathematics described
in the *Principia Mathematica*.
Crucially, they rely only on potential infinity.

The paper is very accessible and fun to read
(I read the English translation in *From Frege to Gödel* by Jean van Heijenoort).

### Grassman

## Definition

Similar to the natural numbers, the primitive recursive functions have an inductive definition.
The domain of every primitive recursive function is
\\(\\mathbb{N}^k\\) (\\(k\\)-tuples of natural numbers).
The range is always \\(\\mathbb{N}\\).

The **basic primitive** recursive functions are:

* the **constant** functions:
  \\[(x_1, \ldots, x_k)\\mapsto y\\]

  Notation: denote the constant functions by \\(C^k_y\\)

* the **successor** function:
  \\[x\\mapsto x+1\\]

  Notation: denote the successor function by \\(S\\)

* the **projection** functions:
  \\[(x_1, \\ldots, x_k)\\mapsto x_i\\text{, where } 1\\leq i\\leq k\\]

  Notation: denote the projection functions by \\(P^k_i\\)

Any primitive recursive function can be combined with composition and primitive recursion
to create a new primitive recursive function:

* **composition**:
  given primitive recursive functions
  * \\( g:\\mathbb{N}^m\\to\\mathbb{N} \\)
  * \\( h_1,\\ldots,h_m:\\mathbb{N}^k\\to\\mathbb{N} \\)

  then 
  \\[x_1\\ldots,x_k \\mapsto g(h_1(x_1, \\ldots, x_k), \\ldots, h_m(x_1, \\ldots, x_k))\\]
  is also primitive recursive.

  Notation: denote composition by \\(g \\circ (h_1, \\ldots, h_m)\\)

* **primitive recursion**:
  given primitive recursive functions
  * \\( g:\\mathbb{N}^k\\to\\mathbb{N} \\)
  * \\( h:\\mathbb{N}^{k+2}\\to\\mathbb{N} \\)

  then the following function \\(f:\\mathbb{N}^{k+1}\\to\\mathbb{N}\\) is also primitive recursive:
  \\[f(0, x_1, \\ldots, x_k) \\mapsto g(x_1, \\ldots, x_k)\\]
  \\[f(y+1, x_1, \\ldots, x_k) \\mapsto h(y, f(x_1, \\ldots, x_k), x_1, \\ldots, x_k)\\]

  Notation: denote primitive recursion by \\(R(g, h)\\)

### Examples

We can define addition as a primitive recursion function with:

\\[\\mathsf{add} \\equiv R(P^1_1, S\\circ P^3_2)
\\]

This is equivalent to what might be more normally written as:

\\[x+y = \\begin{cases}y & \\text{if }x=0 \\\\ (z+y)+1 & \\text{if }x=z+1\\end{cases}
\\]

If we would have preferred to define addition inductively on \\(y\\) instead of \\(x\\),
as is often more convenient,
we could use a flipping function which maps
\\( (x, y)\\mapsto f(y, x)\\):

\\[\\mathsf{flip}(f) \\equiv f \\circ (P^2_1, P^2_0)
\\]

We can now define multiplication as a primitive recursion function with:

\\[\\mathsf{mult} \\equiv R(C^1_0, \\mathsf{add} \\circ (P^3_0, P^3_1))
\\]

## As for-loops

We can represent the primitive recursive functions as an imperative programming language[^loops]
which contains:

* variable assignments:
  * `X=0`
  * `X=X+1`
  * `X=Y`
* bounded loops: `FOR X ... END`

For example, we can implement truncated subtraction as

```
LOOP Y
  A = 0
    LOOP X
      X = A
      A = A + 1
    END
END
```

where we run the program with `X=x`, `Y=y`, and the program terminates with `x∸y` in `X`.


## Church-Turing

Before the Church-Turing thesis was widely accepted,
there was no precise and commonly accepted definition of algorithm
(even though we have written records of algorithm from ancient Mesopotamian).
For a brief window of time, the primitive recursive functions looked like a
reasonable candidate of such a definition.

Ackermann and Sudan, however, were both able to produce examples of a functions
which meet our intuitive understanding of "algorithmic" but fail to be primitive recursive.

The addition of one more operator, namely the minimization operator \\(\\mu\\),
does produce a collection of functions satisfying our intuitive notion of algorithm.

### Fast growing functions

The examples from Ackermann and Sudan of algorithms which are not primitive recursive
both employ a use of "double recursion",
producing a function which grows faster than any primitive recursive function.

The Ackermann function is defined as:

\\[
\\begin{array}{lcl}
    A(0, n) & = & n + 1 \\\\
    A(m+1, 0) & = & A(m, 1) \\\\
    A(m+1, n+1) & = & A(m, A(m+1, n)) \\\\
\\end{array}
\\]

The "diagonal" of the Ackermann function, \\(d(m)=A(m, m)\\), grows incredibly fast.
One way to see this is by comparison to the explosive Knuth's up-arrow notation.
A single arrow represents exponentiation:
\\(
x\\uparrow y = x^y
\\).
Two arrows represents repeated exponentiation, so that \\(x\\uparrow\\uparrow y\\)
is an exponential tower of \\(y\\)-many \\(x\\)'s.
For example, \\(2\\uparrow\\uparrow 3 =\\) $2^{2^2}$ (this is sometimes called tetration).
Three arrows represents iteration of the double arrow, etc.

Fixing \\(m=4\\), the function \\(A(4, n)\\) grows like \\(2^n\\).

Thinking of the values of \\(A\\) as an infinite matrix, with \\(m\\) as the rows and $n$ as the columns,
the forth row of \\(A\\) grows like \\(2^n\\).
The fifth row grows like \\(2\uparrow\uparrow n\\),
and in general the \\((m-4)\\)th row grows like \\(2\uparrow^n m\\),
where $\uparrow^n$ is \\(n\\)-many \\(\uparrow\\)'s.
We write \\(\\uparrow^n\\) to reprent the operator with \\(n\\)-many arrows.
Then
\\[
A(m, n) = 2\\uparrow^{m-2}(n+3) - 3
\\]
for \\(m\\geq 3\\).

Each individual primitive recursive function is eventually bounded by a "row" of the Ackermann function.
In other words, if \\(f\\) is primitive recursive, then there exists numbers \\(m_0, n_0\\) such that
for all \\(n\\geq n_0\\), \\(f(n) \\leq A(m_0, n)\\).

Closely related to these rows is the Grzegorczyk hierarchy.
Thinking of each row as a function $A_m(n) = A(m, n)$,
every primitive recursive functions appears in some collection $\varepsilon_m$ of functions,
where $\varepsilon_m$ is the collection of functions which are "elementary" in $A_m$,
meaning it contains $A_m$, the same initial functions use to construct the primitive
recursive functions, and allowing for combining functions with composition and
bounded sums and products (See VIII.7 of Odifreddi's Classical Recursion Theory, volume II).

#### Graph of the Ackermann function

Hilariously, the *graph* of the Ackermann function is primitive recursive.

In other words, the function
\\[
\\mathsf{AG}(m, n, y) =
\\begin{cases}
  1 & \\text{if } A(m, n) = y \\\\
  0 & \\text{otherwise}
\\end{cases}
\\]
is primitive recursive.

The trick is to notice that the size of the data needed to validate \\(y\\)
as a solution is not that large *relative to* \\(y\\).

To see this, notice that \\(A\\) is strictly monotonic in each coordinate. 
Moreover, in order to validate if \\(A(m, n)=y\\), you can first compute
the table of all values \\(A(a, b)\\) where \\(0\\leq a,b\\leq 13\\),
marking any value which requires a recursive call not on the table with a dot.

For example, in order to verify \\(A(3, 1)=13\\), we can compute this table:

|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9  | 10 | 11 | 12 | 13 |
|---|:--|:--|:--|:--|:--|:--|:--|:--|:--|:-- |:-- |:-- |:-- |:-- |
| **0** | 1 | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | \\(\\cdot\\) |
| **1** | 2 | 3 | 4 | 5 | 6 | 7 | 8 | 9 | 10 | 11 | 12 | 13 | \\(\\cdot\\) | \\(\\cdot\\) |
| **2** | 3 | 5 | 7 | 9 | 11 | 13 | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) |
| **3** | 5 | 13 | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) | \\(\\cdot\\) |

In general, a function is primitive recursive *if and only if*:

* its graph is primitive recursive
* it is bounded by a primitive recursive function

### Totality

There is another fundamental problem with the collection of primitive recursive functions
preventing them from laying an exclusive claim to the notion of algorithm: *totality*.
Totality is a very nice property, but unfortunately any collection of total functions
which can be enumerated by an algorithm cannot be the collection of all algorithms.

A function that is defined on all inputs is called **total**.
This notion does not come up a lot in normal mathematics,
since "on all inputs" usually means the domain of a function and is therefore trivially true.
For computations, however, it can be difficulty to determine which potential inputs are in the
domain.
Take, for example, the function $\mathsf{TP}$ which maps $n$ to the $n$-th pair of twin primes.
No one alive today knows if this function's domain is $\mathbb{N}$ (all natural numbers)
or just $\{0, 1, \ldots, N\}$ for some big $N$.
It is easy to see that the primitive recursive functions are all total,
meaning they are defined on all $\mathbb{N}^k$
(and we'll have a lot more to say about this later, regarding Parson's theorem).

The same diagonal argument that is used to show that there are more real numbers than rational
numbers, the same diagonal argument used to prove that the halting set is not computable,
can also demonstrate that there is an algorithm which is not primitive recursive.

It is easy to see that we can list out all the primitive recursive functions:

\\[ P_0, P_1, P_2, \\ldots
\\]

Consider the following algorithm \\(d\\) defined as:

\\[d(n) = P_n(n)+1
\\]

By construction, \\(d\\) is different from every primitive recursive function.


### Adding minimization

If we add one new construction technique to the primitive recursive functions,
we can describe every algorithm.
We only need to add the minimization operator \\(\\mu\\):

* **minimization**:
  given a function 
  * \\( g:\\mathbb{N}^{k+1}\\to\\mathbb{N} \\)

  we construct a new function \\(\\mu(f)\\mathbb{N}^{k}\\to\\mathbb{N} \\)
  defined by:
  \\[ \\mu(f)(x_1, \\ldots, x_k) = y \\text{ if and only if }
      y \\text{ is the least number } x_0 \\text{ such that } f(x_0, x_1, \\ldots, x_k) = 0
  \\]

Notice that for any given \\(x_1, \\ldots, x_k\\), 
\\f(x_1, \\ldots, x_k\\) may fail to be defined, such as if \\(f\\) is never 0.
For this reason the diagonal argument does not apply to the general recursive functions.

When we add the minimization operator to the primitive recursive functions,
we call the collection of functions the general recursive functions,
which were first defined by Gödel in 1934 using an idea sent to him in a letter from Herbrand in 1931.

The general recursive functions define the same functions as both Turing machines and
the lambda calculus, and for this reason the Church-Turing thesis states that these models
capture our intuitive notion of algorithm.


## Syntax - Peano Arithmetic & Primitive Recursive Arithmetic

So for we have only discussed semantic part of this story,
but the actual history is interwoven with formal syntax.

The full story actually predates Skolem,
perhaps starting with Starts with Hermann Grassmann in 1861 with
"Lehrbouch der Arithmetik", where he defined arithmetic and multiplication
using the primitive recursive definition.
Skolem himself did not introduce a formal logic for his "recursive mode of thought",
this seems to have first been achieved by Ackermann in his 1924 dissertation[^ackPhD].
In the 1880's, Grassman's ideas of formalizing arithmetic saw progress from
Charles Peirce, Richard Dedekind, and Giuseppe Peano.
This culminated in what we now call the Peano axioms,
which state several of the statements that are apparent in the primitive recursive
definitions of arithmetic and multiplication,
and which roughly capture the fact that the integers are a semiring.
Peano arithmetic is the first order logical theory that assumes the Peano axioms.

The early 1920's also saw the rise of Hilbert's program,
which sought to find a solid foundation for mathematics resting on "finitistic" and formal grounds.
Ackermann was one of Hilbert's students, and primitive recursive arithmetic
was the kind of system Hilbert hoped could provide a foundation for math.
Peano arithmetic was another contender.
Unintuitive results about the actual infinite would be defensible when the arguments
were grounded in more constructive methods.

Gödel showed in 1931 that the full scope of Hilbert's program was *impossible*[^incompleteness].
He was exquisitely careful to perform most of the argument using primitive recursive functions
(including a delightful use of the Chinese remainder theorem).

Emil Post very nearly discovered the same incompleteness himself ten years earlier,
but he failed to publish them.[^post]

Note that the Church-Turing thesis would not even be proposed for five years and
the definition of computation was still debated.
It was crucial that the proofs steps Gödel used would be incontrovertible,
and needed to appeal to our intuitive notion of computation.
No funny business.
Gödel himself remained somewhat skeptical of his 1931 result until he saw
Turing's working on Turing machines and the ubiquitous equivalence of models of
computation.[^godelTM1] [^godelTM2]

In 1941, Haskell Curry produced a version with now logical connectives,
using only equality of terms[^curryPRA].

Here is a timeline to help visualize the story.
I've drawn two vertical lines, one to mark when the Church-Turing thesis was announced
and one to to mark when the ENIAC was created.

![](/images/primrec/timeline.png)

### Partial realization of Hilbert's program

\\(\\mathsf{WKL}_0\\) is \\(\\Pi^1\\) conservative over \\(\\mathsf{PRA}\\).

See Simpson. "Subsystems of Second Order Arithmetic", Remark IX.3.18 (page 381).

### Provably Total

The final connection I want to make with logic is the idea of provably total functions.

Definition of provably total.
[Peano Arithmetic](https://en.wikipedia.org/wiki/Peano_axioms#Peano_arithmetic_as_first-order_theory).

Parson's theorem states that the provably total functions of \\(\\mathsf{I}\\Sigma_1\\)
are exactly the primitive recursive functions.
The theorem is often stated as a conservation theorem, namely that 
if \\(\\phi\\) is a \\(\\Pi_2\\) formula which is provable in \\(\\mathsf{I}\\Sigma_1\\),
then \\(\\phi\\) also provable in \\(\\mathsf{PRA}\\).
This is equivalent since totality of a primitive recursive function is a \\(\\Pi_2\\) statement:
for all \\(x\\) there exists a \\(y\\) such that \\(f(x)=y\\).
Note that \\(f(x)=y\\) is not necessarily captured by a bounded formula,
but it is captured by a \\(\\Sigma_1\\) formula by
[Kleene's normal form theorem](https://en.wikipedia.org/wiki/Kleene%27s_T_predicate#Normal_form_theorem).
Indeed, tetration is primitive recursive but not definable by a bounded formula of PA.
(For a neat example of the difference between primitive recursion and bounded formulas,
you can consider the latter to be loop programs where the variable used to loop cannot change[^not_delta_0].)

### Primitive recursive functionals

* System T
* all provably total funcs of PA (but not "for all t, blah")

```haskell
foldr :: (a -> b -> b) -> b -> [a] -> b
```

```haskell
nat :: (b -> b) -> b -> Nat -> b
```

```haskell
data Nats = Zero | Succ Nats
```

```haskell
f :: Nats -> Sigma
f Zero = sigma
f (Succ n) -> g(n, f(n))
```

```haskell
f :: Nats -> Foo
f Zero = some_foo
f (Succ n) -> g(n, f())
```

\\[
\\mathsf{I}_\\sigma: (\\sigma\\to\\sigma) \\to \\sigma \\to \\mathbb{N} \\to \\sigma
\\]

\\[
\\mathsf{R}_\\sigma: (\\mathbb{N}\\to\\sigma\\to\\sigma) \\to \\sigma \\to \\mathbb{N} \\to \\sigma
\\]

\\[
\\begin{array}{lcl}
R_{\\sigma} ~{f} ~{m} ~{0} & \\Rightarrow & n \\\\
R_{\\sigma} ~{f} ~{m} ~{(n+1)} & \\Rightarrow & f(n, R_{\\sigma} ~{f} ~{m} ~{n)}) \\\\
\\end{array}
\\]


\\[
\\mathsf{R}_\\mathbb{N}: (\\mathbb{N}\\to\\sigma\\to\\sigma) \\to \\sigma \\to \\mathbb{N} \\to \\sigma
\\]

See [^dialectica]

## Further fun

### Reading

* Schwichtenberg, Wainer. "Proofs and Computations"
* Oddefreddi. "Classical Recursion Theory, Volume II"
* Kleene. "Introduction to Metamathematics"


### Links

* Representing the primitive recursive functions in Haskell[^haskell].
* Beeson's course notes[^beeson].
* Tait - https://home.uchicago.edu/~wwtx/PRA2.pdf

---

[^skolem]: Skolem. "The foundations of elementary arithmetic established by means of the recursive mode of thought without the use of apparent variables ranging over infinite domains"
[^loops]:[Meyer, Ritchie. "The complexity of loop programs"](https://doi.org/10.1145/800196.806014)
[^incompleteness]: Gödel. "On Formally Undecidable Propositions of Principia Mathematica and Related Systems"
[^godelTM1]: https://www.sciencedirect.com/science/article/pii/S0019995882912268
[^godelTM2]: http://www.people.cs.uchicago.edu/~soare/History/compute.pdf
[^not_delta_0]: [Thanks to Patrick Lutz for this great example](https://math.stackexchange.com/q/3700504)
[^dialectica]: A great exposition is given in [Avigad, Feferman. ["Gödel’s Functional ("Dialectica") Interpretation"](https://www.andrew.cmu.edu/user/avigad/Papers/dialect.pdf).
[^haskell]: [Thanks to Cirdec for this great example](https://stackoverflow.com/a/27217795)
[^ackPhD] https://cs.nyu.edu/pipermail/fom/2006-September/010823.html
[^beeson]: [Beeson's course notes](http://www.michaelbeeson.com/teaching/StanfordLogic/)
[^curryPRA]: https://doi.org/10.2307%2F2371522
[^post]: https://www.jstor.org/stable/3219226
