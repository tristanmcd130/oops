# oops

A functional, dynamically-typed programming language implemented in OCaml, targeting a custom bytecode VM. I wrote a blog post about it [here](https://tristanmcd130.github.io/oops/2025/09/25/oops.html), talking a bit about its history.

# How to use

`dune exec oops` with no other arguments to enter a REPL. Add a filename at the end, and that file will be run.

# Language showcase

```
# single line comments only

3 # numbers
"aaaa" # strings
false # booleans
[1, 2, []] # (linked) lists
{1: false, 2: "a"} # maps

x = 4 # variables

if x == 4 then
  print(4)
elseif x < 4 then
  print("less than 4")
else
  print("greater than 4")
end

let x = 2 in
  print(x) # let-delcared variables shadow outer variables, this will print 2
end

# no while loops or iteration by design, to encourage recursion (don't worry, there are tail calls)

(fun(x, y) x + y)(6, 7) # lambdas, and they are closures!

struct Fib
  c
  n
end # declares a new type called Fib with fields c and n
f = Fib{c: 0, n: 1} # creates a new Fib, field names have to be given as keywords

impl Stream for Fib
  def head()
    self.c
  end
  def tail()
    Fib{c: self.n, n: self.c + self.n}
  end
  def is_empty()
    self.c >= 4000000
  end
end # when a trait's requirements are satisfied, they provide the implementing type with new behavior
# the stream type needs the 3 methods head, tail, and is_empty and provides methods like map, filter, and fold

f.filter(fun(x) x % 2 == 0 end).fold(fun(x, y) x + y end) # an easy solution to Project Euler problem 2 in O(n) time
```
