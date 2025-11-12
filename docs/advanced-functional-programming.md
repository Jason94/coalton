# Advanced Functional Programming in Coalton

Functional programming is built around _pure_ functions, which take some input and produce some output without performing any kind of side effect. This works well when you're just transforming values. But as soon as you need to do something (perform some kind of _effect_), it gets difficult to stick to the pure functions that functional programming relies on.

Coalton solves this problem using a data structure called a _Monad_. One way to describe a monad is that it allows you to construct a program using just pure functions, and at the very end of your code you can run that program, which is allowed to perform effects.

This guide documents some advanced techniques to do functional programming at a larger scale. Many examples of functional programming out there are pretty basic: just performing terminal I/O, just maintaing some internal state, etc. But in practice, we often need to perform multiple kinds of effects at once, or read/write to a file in _production_ but do something else in our test suite.

This guide does assume some basic familiarity with Monads and `do` notation, but will try to explain everything thoroughly beyond that.

## Defining Your Own Effects (Free Monads)

Coalton comes with several monads, particularly `State` and `Env`, that are useful in many applications. But it doesn't come with any monads that allow you to perform side-effects, such as terminal IO. Instead, Coalton gives you a tool called a Free Monad that lets you easily build your own.

For a thorough example of using free monads, see [the Freecalc example program](../examples/small-coalton-programs/src/freecalc.lisp). The Freecalc example is a good demonstration of a "pure" Monad - it's a useful way to leverage the Coalton type system and write pure programs without a lot of the boilerplate. But even when you run a Freecalc program, it's still pure.

Here is another example of a Free Monad, which _does_ perform side effects when run. We'll write a Free Monad called `Terminal` that lets us do basic terminal IO. First, we need to define the different effects that our Free Monad can perform and set up the basic type infrastructure:

```lisp
(cl:in-package :cl-user)
(defpackage :test-free
  (:use
   #:coalton
   #:coalton-prelude
   #:coalton-library/monad/free))

(in-package :test-free)

(coalton-toplevel

  (define-type (TerminalOperationF :next)
    (WriteLineF String :next)
    (ReadLineF (String -> :next)))

  (define-instance (Functor TerminalOperationF)
    (define (map f op)
      (match op
        ((WriteLineF s next) (WriteLineF s (f next)))
        ((ReadLineF next) (ReadLineF (map f next))))))

  (define-type-alias Terminal (Free TerminalOperationF)))
```

Next, define functions to create `write-line` and `read-line` operations. Notice how each one returns a `Terminal :a` - our Monad type, instead of just returning a result. This is what keeps our program _pure_. Normal, non-functional `read-line` would perform an effect and return the result (a String) when called. But our `read-line` returns `Terminal String`, which is just a little program that contains one operation: `ReadLineF`.
```lisp
(coalton-toplevel
  (declare write-line (String -> Terminal Unit))
  (define (write-line str)
    (liftF (WriteLineF str Unit)))

  (declare read-line (Terminal String))
  (define read-line
    (liftF (ReadLineF id))))
```

Because we're using Free Monad, we get all of the fancy monad stuff for free, so we can string our operations together inside a `do` block to make more complex programs, like this one:
```lisp
(coalton-toplevel
  (declare prompt-name (Terminal (Tuple String String)))
  (define prompt-name
    "Get the user's first and last name."
    (do
     (write-line "What is your first name?")
     (first-name <- read-line)
     (write-line "What is your last name?")
     (last-name <- read-line)
     (pure (Tuple first-name last-name)))))
```

Even though this is all pretty neat, we're still just building programs that are sequences of `WriteLineF` and `ReadLineF` operations. We still haven't actually defined what those operations _do_. The last step is to write a function that runs a `Terminal :a` program, performing all of the side-effects, and returning the result. We can do that using the `run-free` helper function, which expects a function that takes a `TerminalOperationF`, does something with it, and then returns back the next step in the program:
```lisp
(coalton-toplevel
  (declare run-terminal! (Terminal :a -> :a))
  (define run-terminal!
    (run-free
     (fn (opF)
       (match opF
         ((WriteLineF str next)
          (lisp :a (str)
            (cl:format cl:t "~a~%" str))
          next)
         ((ReadLineF f-next)
          (let input = (lisp String ()
                         (cl:read-line)))
          (f-next input)))))))
```
You can see that here, we are actually calling Common Lisp code that will print to/read from the terminal. When `run-terminal!` is called, the code will actually perform effects.

We can use this to run our `prompt-name` program and get back the result:
```lisp
(coalton (run-terminal! prompt-name))
```

## Using Multiple Effects (Monad Transformers)

With the `Terminal` monad, we can write programs that do terminal IO. We can achieve other effects with other monads, such as using `ST` to model mutable state:

```lisp
(declare my-terminal-program (Terminal :a))
...
    
(declare my-stateful-program (ST MyState :a))
...
```

But just these tools aren't enough to write a program that can do _both_ Terminal IO and stateful computations. To be able to use multiple different effects at once, you need to use another tool called a Monad Transformer. A Monad Transformer is a complicated way of saying "a monad that contains another monad, and knows how to do everything that both of them can do."

__Note__: The [Bank Example Program](../examples/small-coalton-programs/src/monads-bank.lisp) is a well documented example demonstrating monad transformers in action.

For example, `ST` provides effects like `get`, `put`, and `modify`. `Terminal` provides `write-line` and `read-line`. The _Monad Transformer_ version of `ST`, `StateT`, allows us to use both:

```lisp
(declare my-stateful-terminal-program (StateT MyState Terminal :a))
...
```

Here, `MyState` is the type of the state the program is storing, and `:a` is the return value of the whole program. Suppose we wanted to modify our prompt-name function from before to read the names in and store them in a list of names in the state instead of returning it:

```lisp
;; Make sure to :use #:coalton-library/monad/statet in the package!

(coalton-toplevel
  (define-type Name
    (Name String String))

  (declare prompt-and-store-name (StateT (List Name) Terminal Unit))
  (define prompt-and-store-name
    "Get the user's first and last name."
    (do
     (write-line "What is your first name?")
     (first-name <- read-line)
     (write-line "What is your last name?")
     (last-name <- read-line)
     (modify (Cons (Name first-name last-name))))))
;; COMPILER ERROR:
;;     68 |     (define prompt-and-store-name
;;        |  ___^
;;     69 | |     "Get the user's first and last name."
;;     ...
;;     74 | |      (last-name <- read-line)
;;     75 | |      (modify (Cons (Name first-name last-name))))))
;;        | |__________________________________________________^ Expected type '(((STATET (LIST NAME)) (FREE TERMINALOPERATIONF)) UNIT)' but got type '((FREE TERMINALOPERATIONF) UNIT)'
```

This is what we would _like_ to write, but it will throw a compile error. Remember, the type signature of `write-line` is: `(declare write-line (String -> Terminal Unit))`. It returns a `Terminal` monad. However, the `do` block in `prompt-and-store-name` is expecting a `StateT (ListName) Terminal` (which is itself a monad, albeit a much more complicated one). The error message looks complicated, but it's just telling us that `write-line` is returning one type, but it was expected to return a different type by the caller.

To fix this, we need to make use of the `lift` function that the `MonadTransformer` typeclass gives us. `lift` turns an operation on the "inner" monad (`Terminal` in this case) into an operation on the whole monad transformer. Here, it transforms a `Terminal` -> `StateT (List Name) Terminal`, which is what we need. So this will compile and work as expected:
```lisp
(coalton-toplevel
  (declare prompt-and-store-name-fixed (StateT (List Name) Terminal Unit))
  (define prompt-and-store-name-fixed
    "Get the user's first and last name."
    (do
     (lift (write-line "What is your first name?"))
     (first-name <- (lift read-line))
     (lift (write-line "What is your last name?"))
     (last-name <- (lift read-line))
     (modify (Cons (Name first-name last-name))))))
```
