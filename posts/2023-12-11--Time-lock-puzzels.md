---
title: Time-lock puzzles
tags: time-lock-puzzles, number-theory
---

# Time-lock puzzles

Time-lock puzzles were conceived by Timothy May in 1993[^1]
and brought into reality by Rivest, Shamir, and Wagner in 1996[^2]
(hence called the RSW puzzle).
The idea is to encrypt something that cannot be decrypted until
after a set time in the future.

The RSW puzzle achieves these goals by forcing the decryption process to require a receptive task,
a task assumed to always take the same amount of time.
Crucially, the repetitive tasks cannot be performed in parallel.

The original RSW paper is very readable, but I am going to explain the construction
here with the key details explained more explicitly.

## The key idea

The key idea is to use the same clever trick used in the RSA cryptosystem.
If you know how RSA works, you might have fun stopping here and trying to invent
the RSW puzzle for yourself.

Decryption of the RSW puzzle involves computing the power of a fixed number $a$.
The person who produces the cypher text will have a "trapdoor" allowing them
to substitute a vastly smaller exponent.

We start with the product of two prime numbers $p$ and $q$:

\\[
n = pq
\\]

The factorization of $n$ is the secret trapdoor.

We now fix numbers $a$ and $t$
and compute $a$ to the exponent $2^t$, modulo $n$:

\\[
a^{(2\^t)}~(\\mathsf{mod} {~n})
\\]

There are two ways to compute this number,
the slow way and the fast way.
The slow way is what forces the secret to be revealed only in the future.
The fast way is the means by which the secret is place into the timelock.

If one does not know the factorization of $n$, then computing $a^{2^t}$
amounts to the brute force calculation of squaring $a$ $t$-many times.
The value of $t$ is chosen to be large enough to make this calculation
take the desired amount of time.
The lack of parallelization of the puzzle stems from no known way to parallelize this process.

If, however, you know that $n=pq$, then you can make use of
Euler's theorem:

\\[
a^{\\varphi(n)}\\equiv 1~(\\mathsf{mod} {~n})
\\]

where $\varphi(n)$ is
[Euler's totient function](https://en.wikipedia.org/wiki/Euler%27s_totient_function),
which counts the numbers up to $n$ which are relatively prime to $n$.
In our particular case, $\varphi(n)=(p-1)(q-1)$.

Letting $x$ be the remainder of $2^t$ after division by $(p-1)(q-1)$, by
[Euler's theorem](https://en.wikipedia.org/wiki/Euler%27s_theorem)
we have that

\\[
a^{(2\^t)} \\equiv a^x ~(\\mathsf{mod} {~n})
\\]

Since $x$ is vastly smaller than $2^t$,
$a^x$ (modulo $n$) is fast to compute.

## The construction

Instead of encrypting a long message $M$, we encrypt $M$ with a private key $K$
from some other cryptosystem, and place $K$ in a time-lock puzzle.
Let $C_M$ be the cipher text corresponding to $M$.

Let
\\[ C_K = K + a^{(2\^t)}~(\\mathsf{mod} {~n})\\]

Computing $C_K$ will make use of the fact that $n=pq$.

We make public $C_M$, $C_M$, $n$, $a$, and $t$, and make sure that $p$ and $q$ remain secret.
The person who wishes to unlock the time puzzle must compute $a^{2^t}$
in order to compute $K$ from $C_K$.
Using $K$, they then decrypt M from $C_M$.

## This is just the beginning

The RSW time-lock puzzle is only the beginning of the story.
For a recent survey, see the master's thesis of Ceylin Doğan[^3].

[^1]:I've only found references to this dead link: http://www.hks.net/cpunks/cpunks-
0/1460.html
[^2]:[Rivest, R.L., Shamir, A., Wagner, D.A.: Time-lock puzzles and timed-release crypto](https://people.csail.mit.edu/rivest/pubs/RSW96.pdf)
[^3]:[Dogan, C.: A Comprehensive Study of Time Lock Puzzles and Timed Signatures in Cryptography](https://open.metu.edu.tr/handle/11511/104452)
