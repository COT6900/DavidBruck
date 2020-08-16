### David Bruck COT6900 - Week 1

[Assignment problems at the bottom](#programming-in-haskell-chapter-1), but it was not trivial to setup a Haskell development environment, so I am including my own installation steps first.

### Haskell + Visual Studio Code : Windows Environment Setup

##### Install latest stable MinGW with MSYS to default location: C:\MinGW

##### Install Git for Windows for "All Users" to default location C:\Program Files\Git

##### Install Haskell Tool Stack
* https://docs.haskellstack.org/en/stable/install_and_upgrade/
  Manual download
  * Download the latest release; if version is not 2.3.1, change "stack-2.3.1-windows-x86_64" (without the quotes) everywhere else it appears in these instructions
    * Windows 64-bit
  * Extract to C:\stack-2.3.1-windows-x86_64

##### Install Glasgow Haskell Compiler and configure System path

* Run MSYS as Administrator:
  `C:\MinGW\msys\1.0\msys.bat`
* Ensure git.exe and stack.exe are available on System path by appending environment variables \$PATH for MSYS with C:\Program Files\Git and C:\stack-2.3.1-windows-x86_64 with command:  
  `export "PATH=$PATH:/c/Program Files/Git/cmd:/c/stack-2.3.1-windows-x86_64"`
* Test locations of git.exe and stack.exe with commands:  
  `where git.exe`  
  `where stack.exe`
* Install Glasgow Haskell Compiler (ghc) version 8.10.1 (neither 8.8.1 nor 8.8.2 nor 8.8.3 seem to allow you to build dependencies of haskell-language-server like floskell due to segfaults exclusive to Windows). Run command:  
  `stack setup 8.10.1`
* Ensure ghc.exe is available on System path by appending environment variable \$PATH for MSYS with %LOCALAPPDATA%\Programs\stack\x86_64-windows\ghc-8.10.1\bin with command:  
  `export PATH=$PATH:$(echo "$LOCALAPPDATA\\Programs\\stack\\x86_64-windows\\ghc-8.10.1\\bin" | sed -r -e 's/\\/\//g' -e 's/^([^:]+):/\/\1/' -e 's/\/./\L&/')`
* Configure Stack to use the GHC now on the System path with commands:  
  `stack config set system-ghc true`  
  `stack config set system-ghc --global true`  
  `stack config set resolver ghc-8.10.1`
* Permanently ensure git.exe, stack.exe, and ghc.exe are on System path in MSYS, run command:  
  `echo $'export "PATH=$PATH:/c/Program Files/Git:/c/stack-2.3.1-windows-x86_64:$(echo "$LOCALAPPDATA\\\\Programs\\\\stack\\\\x86_64-windows\\\\ghc-8.10.1\\\\bin" | sed -r -e \'s/\\\\/\\//g\' -e \'s/^([^:]+):/\\/\\1/\' -e \'s/\\/./\\L&/\')"' > ~/.profile`

##### Retest git.exe, stack.exe, and ghc.exe are on System path in MSYS

* Close MSYS if already running since we want to test the profile changes to automatically configure System path again on startup
* Run MSYS as Administrator:  
  `C:\MinGW\msys\1.0\msys.bat`
* Test locations of git.exe, stack.exe, and ghc.exe with commands:  
  `where git.exe`  
  `where stack.exe`  
  `where ghc.exe`
* Test Stack is using ghc-8.10.1 with command:  
  `stack exec -- ghc --version`
  * It should not start downloading anything and should return almost instantly
    Output should display "The Glorious Glasgow Haskell Compilation System, version 8.10.1"

##### Install HLint (a Haskell linter)

* Run MSYS as Administrator (if not already running):  
  `C:\MinGW\msys\1.0\msys.bat`
* Install hlint (at this time it does not build with the ghc-8.10.1 compiler so use prior LTS release instead) with command:  
  `stack --resolver=lts-15.13 install hlint`

##### Build the Haskell Language Server

* Run MSYS as Administrator (if not already running):  
  `C:\MinGW\msys\1.0\msys.bat`

* Download and build Haskell IDE Engine (hie) source code from GitHub with commands:  
  `git clone https://github.com/haskell/haskell-ide-engine --recurse-submodules`  
  `cd haskell-ide-engine`  
  `git checkout tags/1.4`  
  `certutil.exe -urlcache -split -f "https://downloads.haskell.org/~cabal/cabal-install-3.2.0.0/cabal-install-3.2.0.0-x86_64-unknown-mingw32.zip" cabal-install-3.2.0.0-x86_64-unknown-mingw32.zip`  
  `$WINDIR\\System32\\tar.exe -xf cabal-install-3.2.0.0-x86_64-unknown-mingw32.zip`  
  `stack --skip-ghc-check exec --no-ghc-package-path -- cabal update`  
  `stack exec --no-ghc-package-path -- cabal v2-run ./install.hs --project-file=install\\shake.project hie`
  
* Copy Cabal to its own bin directory since hie.exe will look for it when it runs by running command:  
  `cp cabal.exe $APPDATA\\cabal\\bin`

##### Install Haskell Debug Adapter

* Run MSYS as Administrator (if not already running):  
  `C:\MinGW\msys\1.0\msys.bat`
* Install haskell-dap, ghci-dap, and haskell-debug-adapter with command:  
  `stack install haskell-dap ghci-dap haskell-debug-adapter`

##### Install Visual Studio Code (VS Code)

* https://code.visualstudio.com/download
  VS Code installation options (choose either/or):

  1. Portable: click .zip -> "64 bit" to download the portable version Visual Studio Code. Extra steps for portable install only:

     * Extract the zip file somewhere

     * Create an empty folder inside the extracted code folder named exactly "data" (without the quotes)

  2. Or install wizard: click User Installer -> "64 bit" to download the installer and run it

##### Setup VS Code for Haskell

Install and setup VS Code extensions Haskell Syntax Highlighting, haskell-linter, Haskell Language Server, and Haskell GHCi debug viewer Phoityne:

* Run VS Code (code.exe), open Extensions sidebar tab (Ctrl+Shift+X if using default keybindings)
* For each of the following, search by the name (just the part inside the quotes), select the corresponding result, and click the green Install button:
  * "Haskell Syntax Highlighting" by Justus Adam
  * "haskell-linter" by Cody Hoover
  * "Haskell Language Server" by Alan Zimmerman
  * "Haskell GHCi Debug Adapter Phoityne" by phoityne
* (Optional) Configure Log File for Haskell Language Server
  * Open the Command Palette (Ctrl+Shift+P if using default keybindings)
  * In the texbox which is now focused, leave the ">" character alone and append the following, then press enter:  
    `Preferences: Open Settings (JSON)`
  * A settings.json file should appear in the file editor view, add JSON field for "languageServerHaskell.logFile" (copy it INCLUDING the quotes) inside the root JSON object (fill in <<log_file_dest.log>> desired log output location):
    * "languageServerHaskell.logFile": "<<log_file_dest.log>>"

<div style="page-break-after: always; break-after: page;"></div>



##### More VS Code setup

* Close any open instances of VS Code (code.exe), and then open a new one.
* Create a new file with top File menu -> New File (or Ctrl+N if using default keybindings)
* Paste the following line into the new file:  
  `showDemo a b c = a $ b $ (c * c)`
* Save the file with top File menu -> Save (or Ctrl+S if using default keybindings). It can have any name, but it needs to have the ".hs" extension, like Untitled-1.hs. Finally, click Save to complete saving.
* You should see a warning message popup like: "Mismatching GHC versions: GHC session is No System GHC Found., HIE is 8.8.3 You may want to use hie-wrapper. Check the README for more information", follow additional instructions:
  * Note: you can right click the warning and click Copy Text and then paste your clipboard somewhere else to see the full message.
* Run Command Prompt as Administrator (the window title should read "Administrator: Command Prompt" once opened), and run the following command:  
  `setx /m Path "%Path%;C:\stack-2.3.1-windows-x86_64;%LOCALAPPDATA%\Programs\stack\x86_64-windows\ghc-8.8.3\bin;%APPDATA%\local\bin;%APPDATA%\cabal\bin"`
* Unfortunately, even closing VS Code may not allow it to pick up changes to the System path. You must restart Windows.
* Once Windows is restarted, open VS Code again.
* Open the last Haskell file saved, if it's not already opened (the one with contents: `showDemo a b c = a $ b $ (c * c)`)
* Open the Command Palette (Ctrl+Shift+P if using default keybindings)
* In the texbox which is now focused, leave the ">" character alone and append the following, then press enter:  
  `Haskell: Restart HIE`
* If all was done correctly, there should **not** be the same warning message popup this time. Also, there should be lightbulb symbol which wasn't there before at the end of the first line.

##### How to Create a New Haskell Project in VS Code

* Open VS Code (code.exe) if it's not already open
* Open the terminal if one is not already open with top Terminal menu -> New Terminal (Ctrl+Shift+~ if using default keybindings)
* Navigate to the parent directory which will contain a new Stack test project with `cd` commands in the terminal
* Initialize a Stack project with command (replace <<any_project_name>> as desired):  
  `stack new <<any_project_name>> new-template`
* Need to build the Stack project at least once to prevent errors like "cannot satisfy -package-id p3-0.1.0.0-5Ir81MgQfuqD4TEhZ8TIyu\n    (use -v for more information)" with the following commands:  
  `cd <<any_project_name>>`  
  `stack build`
* Open the new project folder in VS Code via the following instructions:
  * Click in the top File menu -> Open Folder
  * In the Open Folder dialog, navigate to the new project's folder and open it too
  * Click Select Folder
* Configure debugging via haskell-debug-adapter, by using the Command Palette (Ctrl+Shift+P if using default keybindings)
* In the texbox which is now focused, leave the ">" character alone and append the following, then press enter:  
  `Debug: Open launch.json`
* The textbox should now be cleared out with placeholder text "Select Environment", enter the following, then press enter:  
  `haskell-debug-adapter`
* The file launch.json should have been created and opened in the file editor view. Change the value for JSON field "startup" if it's pointing to a file under `/test/` folder. It should instead point to the file under `/app/` folder:  
  `"startup": "${workspaceFolder}/app/Main.hs",`
* Open the src file in top File menu -> Open File (Ctrl+O if using default keybindings)
* Navigate to /src/Lib.hs, select it, and click Open
* Lib.hs file should now be open in the file editor view, place the text cursor inside the last line `someFunc = putStrLn "someFunc"`
* Place breakpoint in top Run menu -> Toggle Breakpoint (F9 if using default keybindings)
* The last line should now have a red dot at the start of the line.
* Start debugging in top Run menu -> Start Debugging (F5 if using default keybindings)
* If everything goes well, the program should build and run, and VS Code should go into debugging mode

##### Generating Haddock Documentation (analogous to JavaDoc)

* Open VS Code (code.exe) if it's not already open
* Open a folder containing the Haskell project if not already open via top File menu -> Open Folder, navigate to the folder, open it, and click Select Folder
* Open the terminal if one is not already open with top Terminal menu -> New Terminal (Ctrl+Shift+~ if using default keybindings)
* Build Haddock Documentation and continue on errors with command:  
  `stack haddock --keep-going`
* It will output the location of the generated documentation as HTML paths
* Libraries with Haddock-commented exports in this way can be used to enhance Haskell Language Server intellisense as well (such as when hovering over classes/functions, etc.)
* If intellisense doesn't get updated immediately, troubleshoot with the following:
  * Try to restart HIE by opening the Command Palette (Ctrl+Shift+P if using default keybindings)
  * In the texbox which is now focused, leave the ">" character alone and append the following, then press enter:  
    `Haskell: Restart HIE`

##### Troubleshooting Haskell Language Server Errors

In VS Code, if you see an error like: "cannot satisfy -package-id p3-0.1.0.0-5Ir81MgQfuqD4TEhZ8TIyu\n    (use -v for more information)", one or more of these troubleshooting steps may help:

 1. Find Setup.hs in the root folder of the project and rename it like Setup.hs.bak. Also, close the file if it's open in a file editor view.
  2. Refresh hls/restart hie
     * Build via Stack
       * Open the terminal if one is not already open with top Terminal menu -> New Terminal (Ctrl+Shift+~ if using default keybindings)
       * Build with command:  
         `stack build`
     * Reload VS Code Window
       * Open the Command Palette (Ctrl+Shift+P if using default keybindings)
       * In the texbox which is now focused, leave the ">" character alone and append the following, then press enter:  
         `Developer: Reload Window`
  3. If it still doesn't work, check the log <<log_file_dest.log>> setup from earlier instructions "(Optional) Configure Log File for Haskell Language Server". You can still follow those instructions now if you didn't before, and you may need to close and reopen VS Code if you do.



### Programming in Haskell, chapter 1

#### Problem 3

C1P3.hs :

```haskell
{-|
Module      : C1P3
Description : Programming in Haskell, chapter 1, problem 3
Maintainer  : David Bruck
-}
module C1P3
    ( C1P3.product
    ) where

-- 'product' is also imported by Prelude,
-- so all references need be explicitly prefixed by module

-- |Produces the product of multiplying elements of input list
product []      = 1 {-  Identity "base" state for multiplication is:
                        element * identity == element
                        Therefore, identity == 1 -}
product (x:xs)  = x * C1P3.product xs
```



Main.hs :

```haskell
module Main where

import qualified C1P3 ( product )

-- |Tests 'C1P3.product' with a hardcoded list [2, 3, 4] with expected result: 24
main :: IO ()
main = print (C1P3.product [2, 3, 4])
```



#### Problem 4

C1P4.hs :

```haskell
{-|
Module      : C1P4
Description : Programming in Haskell, chapter 1, problem 4
Maintainer  : David Bruck
-}
module C1P4
    ( reverseQSort
    ) where

{-|Returns input list sorted in descending order
   Modified from qsort as provided in book Programming in Haskell, chapter 1
   NOTE: not a true quicksort because it does not randomly choose the
         partition point; instead, it always uses the first element on
         each partition which means it would have running time of O(n^2)
         on already-sorted inputs
-}
reverseQSort []     = []
reverseQSort (x:xs) = reverseQSort larger ++ [x] ++ reverseQSort smaller
                      where smaller = [a | a <- xs, a <= x]
                            larger  = [b | b <- xs, b > x]

```



Main.hs :

```haskell
module Main where

import C1P4

{-|Tests 'C1P4.reverseQSort' with a hardcoded list of unsorted range (1..9)
   with expected output being list sorted in descending order (opposite qsort)
-}  
main :: IO ()
main = print (reverseQSort [1, 9, 2, 8, 3, 7, 4, 6, 5])
```



### Programming in Haskell, chapter 2

#### Problem 4

C2P4.hs :

```haskell
{-|
Module      : C2P4
Description : Programming in Haskell, chapter 2, problem 4
Maintainer  : David Bruck
-}
module C2P4
    ( C2P4.last
    ) where

-- 'last' is also imported by Prelude,
-- so all references need be explicitly prefixed by module

-- |Returns 'Just' a list's last element, or 'Nothing' for an empty list
last []     = Nothing
last [x]    = Just x
last (x:xs) = C2P4.last xs
```



Main.hs :

```haskell
module Main where

import qualified C2P4 ( last )

{-|
    Tests 'C2P4.last' twice:
        once with a hardcoded list,
        and once with an empty list (throws an error)
-}
main :: IO ()
main = do {
    print (valueOrError ["primero", "luego", "finalmente"]);

    -- cannot take last of empty list
    print (valueOrError ([] :: [String]))
}
    where valueOrError possible = case C2P4.last possible of
              Nothing       -> errorWithoutStackTrace "empty list"
              Just possible -> possible
```



Another way to re-implement last with other existing library functions:

`head (reverse [1, 2, 3])` which outputs `3`



#### Problem 5

C2P5.hs :

```haskell
{-|
Module      : C2P5
Description : Programming in Haskell, chapter 2, problem 5
Maintainer  : David Bruck
-}
module C2P5
    ( initOne
    , initTwo
    ) where

initOneImpl _  []       = []
initOneImpl xs [x]      = xs
initOneImpl xs (x:xxs)  = initOneImpl (xs ++ [x]) xxs

-- |Returns an array which contains all but the last element
initOne     = initOneImpl [] {- initOne also has an implicit parameter
                                since partial application was applied to
                                initOneImpl (currying) -}

-- |Returns an array which contains all but the last element
initTwo arr = reverse (tail (reverse arr))
```



Main.hs :

```haskell
module Main where

import C2P5

-- |Tests 'C2P5.initOne' and 'C2P5.initTwo' with different hardcoded lists
main :: IO ()
main = do
    print (initOne ["notLast1", "notLast2", "last"])
    print (initTwo ["notLastA", "notLastB", "last"])
```



### Programming in Haskell, chapter 3

#### Problem 2

**`second xs = head (tail xs)`**

`tail` acts on a list of a polymorphic variable type and returns an output of the same type, so `xs :: [a]`. `head` then acts on this same input/output type from `tail` and returns the type of one of the list's elements, the polymorphic variable type itself, or `a`. Putting them together: `second :: [a] -> a`

**`swap (x, y) = (y, x)`**

`swap` acts on a tuple of polymorphic variable types and returns an output of a tuple with polymorphic types in opposite order, so `swap :: (a, b) -> (b, a)`.

**`pair x y = (x, y)`**

Similar logic but takes two polymorphic variable types as input and outputs the type of their tuple. Since the function takes two parameters, you apply the first parameter's type to get a new output function which can take the second (since we can have partial application), so `pair :: a -> b -> (a, b)`

**`double x = x * 2`**

Even though we see the value of `2` in the definition, we only know the type of parameter `x` must be restricted such that it is accepted for operator `(*)` with `2`. It could still be multiple types of numeric types, for example, `Int` / `Integer` / `Float`, so we know `x :: a` (polymorphic type) with `a` being type-restricted to be a `Num` (numeric). All together, we have `double :: a -> a`

**`palindrome xs = reverse xs == xs`**

We know `reverse` acts on a list of a polymorphic variable type. The operator `==` can act on left-side and right-side inputs of the same polymorphic variable type too, but always returns explicit type `Bool`, so  `palindrome :: [a] -> Bool`

**`twice f x = f (f x)`**

We see that parameter `f` is applied first with the polymorphic variable type of `x :: a`. Also, since `f` is then applied again on the output from the first application, we know it's output type is compatible with its input type, so `f :: (a -> a) -> a -> a`



#### Problem 4

Even if two functions have the same type, the only way they could theoretically be equal is if you are comparing the exact same function to itself, or if somehow you can guarantee both functions produce the exact same output for every input. Haskell would have to read the following code, for example, and realize they are functionally the same:  
`doubleOne a = a + a`  
`doubleTwo a = a * 2`

In other words, it might be trivial to say that `doubleOne == doubleOne` should return `True`, but unreasonable for Haskell to realize `doubleOne == doubleTwo` should also return `True`.

Even when I give explicit types for `_ :: Int -> Int`, when I tried compiling the following Haskell code, I get error **No instance for (Eq (Int -> Int)) arising from a use of `=='**:

```haskell
doubleOne :: Int -> Int
doubleOne a = a + a

doubleTwo :: Int -> Int
doubleTwo a = a * 2

doubleOne == doubleOne
```



### Programming in Haskell, chapter 10

#### Problem 1

Functions we are copying from the book, C10P1BookFunctions.hs :

```haskell
module C10P1BookFunctions
    ( nat2int
    , int2nat
    , add
    , Nat (..)
    ) where

-- |Book reference data type 'Nat'
data Nat = Zero | Succ Nat

-- |Book reference function 'nat2int'
nat2int Zero        = 0
nat2int (Succ n)    = 1 + nat2int n

-- |Book reference function 'int2nat'
int2nat 0 = Zero
int2nat n = Succ $ int2nat (n - 1)

-- |Book reference function 'add'
add Zero n      = n
add (Succ m) n  = Succ $ add m n
```



C10P1.hs :

```haskell
{-|
Module      : C10P1
Description : Programming in Haskell, chapter 10, problem 1
Maintainer  : David Bruck
-}
module C10P1
    ( multiply
    ) where

import C10P1BookFunctions
    ( add
    , Nat (..)
    )

{-|
    Using only 'C10P1BookFunctions.add', 'multiply' creates an expression
    tree which can be evaluated to the the product of two natural numbers
-}
multiply x Zero     = Zero
multiply x (Succ y) = add x $ multiply x y
```



Main.hs :

```haskell
module Main where

import C10P1

import C10P1BookFunctions
    ( nat2int
    , int2nat
    , Nat (..)
    )

-- |Tests 'C10P1.multiply' with hardcoded parameters 5 and 6 with expected result: 30
main :: IO ()
main = print $ nat2int $ multiply (int2nat 5) (int2nat 6)
```



#### Problem 4

I simply never had to specify the type of elements of the `List` nor `Tree` so `balance` function "just works" with `Int`, `Double`, `String` or anything `sort`-able.

C10P4.hs :

```haskell
{-|
Module      : C10P4
Description : Programming in Haskell, chapter 10, problem 4
Maintainer  : David Bruck
-}
module C10P4
    ( Tree
    , balance
    ) where

import Data.List
    ( sort
    , genericSplitAt
    )

-- |Binary tree which allows everything except a node with only a right child
data Tree a
    = Empty
    | Leaf a
    | LeftOnlyNode a (Tree a)
    | FullNode a (Tree a) (Tree a)

-- |Takes a possibly unsorted list and turns it into a balanced 'Tree'
balance xs = balanceImpl (sort xs)

spacesPerTab = 2
{-
    Show on ordered Tree (FullNode 3 (LeftOnlyNode 2 (Leaf 1)) (Leaf 4)) prints:
    3
      2
        1
      4
    (prints current node/leaf value,
     then travels down left (if node),
     then travels down right (if full node);
     each level is intended 2 extra spaces)

    Show on empty Tree prints: empty
-}
instance (Show a) => Show (Tree a) where
    show Empty  = "empty" {- Only the entire tree can be Empty (special case),
                             Empty is not reused for leafs of other nodes
                             so we never have to check for it on tree-walking
                          -}

    -- Start at zero indendation and without a starting newline
    show a      = showImpl 0 a where
        -- Abstract state machine to perform tree-walking
        showImpl t a = case a of
            (Leaf val)                  -> showVal val
            (LeftOnlyNode val left)     -> showValAndLeft val left
            (FullNode val left right)   -> showValFull val left right
            where

            showValFull val left right  = showValAndLeft val left ++
                                          showImpl (t + 1) right
            showValAndLeft val left     = showVal val ++ showImpl (t + 1) left
            showVal val                 = (if t == 0 then "" else "\n") ++
                                          replicate (t * spacesPerTab) ' ' ++
                                          show val

-- |Internal implementation of 'balance' which requires a sorted list
balanceImpl :: [a] -> Tree a
balanceImpl []      = Empty
balanceImpl [a]     = Leaf a
balanceImpl [a, b]  = LeftOnlyNode b (Leaf a)
balanceImpl xs      = {-
                          1. Split the list in half favoring a longer right side
                          2. Make the parent out of the head of the right part,
                          3. Make the left child out of left and right child out
                             of right's tail
                      -}
                      FullNode
                          (head right)
                          (balanceImpl left)
                          (balanceImpl (tail right)) where

                          (left, right) = genericSplitAt medianIndex xs
                          medianIndex   = length xs `quot` 2
```



Main.hs :

```haskell
module Main where

import C10P4

{-|Tests 'C10P4.balance' with some unsorted (actually reverse-sorted) lists
   of varying types, and also tests an empty list (prints "empty")
-}
main :: IO ()
main = do {
    print (balance ([] :: [Int]));                          -- depth 0 (empty)
    print (balance ["Darn", "Cats", "Believe", "Anything"]);-- [String], depth 3
    print (balance [4.321, 3.21, 2.1, 1.0]);                -- [Double], depth 3
    print (balance [9, 8, 7, 6, 5, 4, 3, 2, 1])             -- [Int], depth 4
}
```



An alternate implementation (not shown here) would be to start with an empty `Tree`, and implement an `add` which returns a copy of the tree, re-balanced, with the appended value, and we can `reduce` an unsorted list to a fully balanced (and sorted) tree.
