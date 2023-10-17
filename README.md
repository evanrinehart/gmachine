# A simple graph reduction machine

The demo program loads a reducible expression as a graph into the heap.
Then it carries out a series of steps to reduce the expression to weak
head normal form.

The details come from Peyton Jones 1987 book Implementation of Functional
Programming Languages. See Part II chapter 11 and 12.

An early version of this code included support for dealing with indirection
nodes in the graph. But because no indirection nodes are created
(see section 12.4.2) that support was removed to make the code slightly leaner.

```
> cabal build
> cabal run
```
