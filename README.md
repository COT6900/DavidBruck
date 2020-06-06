# DavidBruck COT6900 Summer 2020
Code is made publically available with no warranty under [the 2-Clause BSD License](https://opensource.org/licenses/BSD-2-Clause):
The full LICENSE is included as file [LICENSE](LICENSE)

[Week 1](Week1)

[Week 2](Week2)

... additional weeks will be added weekly till course completion ...

### Tentative assignments:

##### Week 1:

Module topic: type system (List, Tuple, Function, Class)

###### Programming in Haskell, chapters 1, 2, 3, 10

Assignment excercises:  
1.3  Define a function product that produces the product of a list of numbers, and show using your definition that product `[2, 3, 4] = 24`  
1.4  How should the definition of the function qsort be modified so that itproduces a reverse sorted version of a list?  
2.4  Show how the library function last that selects the last element of a nonempty list could be defined in terms of the library functions introduced in this chapter. Can you think of another possible definition?  
2.5  Show how the library function init that removes the last element from a non-empty list could similarly be defined in two different ways.  
3.2  What are the types of the following functions?

*     `second xs = head (tail xs)`
*     `swap (x, y) = (y, x)`
*     `pair x y = (x, y)`
*     `double x = x * 2`
*     `palindrome xs = reverse xs == xs`
*     `twice f x = f (f x)`

3.4  Why is it not feasible in general for function types to be instances of the `Eq` class? When is it feasible? Hint: two functions of the same type are equal if they always return equal results for equal arguments.  
10.1 Using recursion and the function add, define a multiplication function `mult :: Nat → Nat → Nat` for natural numbers.  
10.4 Define a function `balance :: [Int] → Tree` that converts a non-empty list of integers into a balanced tree.

> For 10.4, I actually implemented it as `[a] -> Tree a` (where `a` is `sort`-able) since there was actually no need to specify any types since Haskell has great polymorphic type support!
>
> -David Bruck

##### Week 2:

Module topic: higher order functions and list comprehension

###### Programming in Haskell, chapters 4, 5, 6, 7, 8

Assignment excercises:  
4.2  Consider a function `safetail :: [a ] → [a]` that behaves as the library function tail, except that `safetail` maps the empty list to itself, whereas tail produces an error in this case. Define `safetail` using:  
* (a) a conditional expression;
* (b) guarded equations;
* (c) pattern matching.

5.3  A triple (x, y, z) of positive integers is pythagorean if `x2 + y2 = z2`.  
     Using a list comprehension, define a function `pyths :: Int → [(Int, Int, Int)]` that returns the list of all pythagorean triples whose components are at most a given limit. For example:

>    `> pyths 10`
>    [(3, 4, 5), (4, 3, 5), (6, 8, 10), (8, 6, 10)]

5.5  Show how the single comprehension `[(x, y) | x ← [1, 2, 3], y ← [4, 5, 6]]` with two generators can be re-expressed using two comprehensions with single generators.  
6.4  Define a recursive function `merge :: Ord a ⇒ [a] → [a] → [a]` that merges two sorted lists to give a single sorted list. For example:

>    `> merge [2, 5, 6] [1, 3, 4]`
>    [1, 2, 3, 4, 5, 6]

   Note: your definition should not use other functions on sorted lists such as insert or isort, but should be defined using explicit recursion.  
6.5  Using `merge`, define a recursive function `msort :: Ord a ⇒ [a] → [a]` that implements merge sort, in which the empty list and singleton lists are already sorted, and any other list is sorted by merging together the two lists that result from sorting the two halves of the list separately.  
7.7  A higher-order function unfold that encapsulates a simple pattern of recursion for producing a list can be defined as follows:

>    ```
>    unfold p h t x | p x = [ ]
>                   | otherwise = h x : unfold p h t (t x)
>    ```

   That is, the function `unfold p h t` produces the empty list if the predicate `p` is `true` of the argument, and otherwise produces a non-empty list by applying the function `h` to give the head, and the function `t` to generate another argument that is recursively processed in the same way to produce the tail of the list. For example, the function `int2bin` can be rewritten more compactly using `unfold` as follows:

>    `int2bin = unfold (== 0) (‘mod‘2) (‘div‘2)`

   Redefine the functions chop8, map f and iterate f using unfold.  
8.6  Extend the parser for arithmetic expressions to support subtraction and division, based upon the following extensions to the grammar:

>    ```
>    expr ::= term (+ expr | − expr | )
>    term ::= factor (* term | / term | )
>    ```

   **Note: the book-referenced source code from https://www.cs.nott.ac.uk/~pszgmh/pih.html included calculator.hs which already implemented ability to parse operators (-) / (/) so instead I will solve a different problem. I will be adding support for operator (√) for integer square root, and I will be adding the ability to use alternate syntaxes. For example, you can use "sqrt" for square root and for problem 7 we can also use (^) for "to the power of". -David**  
8.7  Further extend the grammar and parser for arithmetic expressions to support exponentiation, which is assumed to associate to the right and have higher priority than multiplication and division, but lower priority than parentheses and numbers. For example, `2 ↑ 3 ∗ 4` means `(2 ↑ 3) ∗ 4`.

##### Week 3:

Module topic: state management and interactive programs

###### Programming in Haskell, chapters 9, 10 (again)

Assignment excercises:  
9.3  On some systems the game of life may flicker, due to the entire screen being cleared each generation. Modify the game to avoid such flicker by only redisplaying positions whose status changes.  
9.4  Produce an editor that allows the user to interactively create and modify the content of the board in the game of life.  
9.6  Nim is a game that is played on a board comprising five numbered rows of stars, which is initially set up as follows:

>    1 : * * * * *  
>    2 : * * \**  
>    3 : * * *  
>    4 : \**  
>    5 : *

    Two players take it in turn to remove one or more stars from the end of a single row. The winner is the player who removes the last star or stars from the board. Implement the game of nim in Haskell.  
10.6 Using the function isTaut together with the parsing and interaction libraries from the previous two chapters, define an interactive tautology checker that allows propositions to be entered from the keyboard in a user-friendly syntax.

###### Learn You a Haskell, chapters 11, 12, 13

Assignment exercises:  
11.Applicative functors  
   Like normal functors, applicative functors come with a few laws. The most important one is the one that we already mentioned, namely that pure `f <*> x = fmap f x` holds. As an exercise, you can prove this law for some of the applicative functors that we've met in this chapter.The other functor laws are:

>    `pure id <*> v = v`  
>    `pure (.) <*> u <*> v <*> w = u <*> (v <*> w)`  
>    `pure f <*> pure x = pure (f x)`  
>    `u <*> pure y = pure ($ y) <*> u`

12.A knight's quest  
   As an exercise, you can change this function so that when you can reach one position from the other, it tells you which moves to take. Later on, we'll see how to modify this function so that we also pass it the number of moves to take instead of that number being hardcoded like it is now.  
13.Error error on the wall  
   As an exercise, you can rewrite that with the error monad so that when the tightrope walker slips and falls, we remember how many birds were on each side of the pole when he fell.

##### Week 4:

Module topic: algebraic data types and reasoning about programs

###### Programming in Haskell, chapters 11, 13

Assignment exercises:  
11.4 Using choices, exprs, and eval, verify that there are 33,665,406 possible expressions over the numbers `1, 3, 7, 10, 25, 50`, and that only 4,672,540 of these expressions evaluate successfully.  
11.5 Similarly, verify that the number of expressions that evaluate successfully increases to 10,839,369 if the numeric domain is generalised to arbitrary integers.  
13.2 Show that `add n (Succ m) = Succ (add n m)`, by induction on `n`.  
13.7 Using the definitions

>    ```
>    map f [ ] = [ ]
>    map f (x : xs) = f x : map f xs
>    (f ◦ g) x = f (g x)
>    show that map f (map g xs) = map (f ◦ g) xs, by induction on xs.
>    ```

###### Learn You a Haskell, chapter 8

Assignment exercises:  
8.Kinds and some type-foo  
   When working on real Haskell, you usually won't have to mess with kinds and do kind inference by hand like we did now. Usually, you just have to partially apply your own type to `* -> *` or `*` when making it an instance of one of the standard typeclasses. How and why does that actually work?

##### Week 5:

Module topic: advanced language constructs

https://wiki.haskell.org/A_practical_Template_Haskell_Tutorial  
Assignment exercises:  
   Write a Code Generator with Template Haskell to make object relational mapping (ORM) functions for CRUD operations on a database from a schema definition.

https://hackage.haskell.org/package/http-server  
Assignment exercises:  
   IMDb is the world's most popular and authoritative source for movie, TV and celebrity content, designed to help fans explore the world of movies and shows and decide what to watch. Write two Haskell applications.  
1. One which parses the IMDb dataset from https://datasets.imdbws.com/ into a database using the ORM-generated CRUD functions.
2. A second which hosts a web application that reads the same database with the ORM-generated CRUD functions to display a paged list of movies and TV shows with the ability to search by title. Each entry should display:
   - Type of title (Short / Movie)
   - Year
   - Runtime duration
   - Genres
