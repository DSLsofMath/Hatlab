# Preliminaries

- OSX: Install [Gnuplot][1] using [Homebrew][2]

        brew install gnuplot --with-wxmac


# Setup

Install deps using `cabal` or `stack`:

    cabal install --only-dependencies

or

    stack build


# Try it

`cabal repl` or `stack ghci` then

    > plot [Fun (sin.(pi*)) "sin", Fun (cos.(pi*)) "cos"]

Default plot interval is [-1,1].

![example plot][3]

# Plotting relations

Hatlab supports plotting of relations on the real numbers. The Hatlab.Relations
module defines a small DSL for relations. The supported operations are:

`r :: (Double -> Double -> Bool) -> String -> Relation` 
<br\>
`r` defines a basic relation. Some operations on relations
are supported:
<br\>
`r .\/. q = r ∪ q `
<br\>
`r ./\. q = r ∩ q `
<br\>
`c r      = compliment of r`
<br\>
`r .\.  q = r minus q `
<br\>

[1]: http://www.gnuplot.info
[2]: http://brew.sh
[3]: example.png
