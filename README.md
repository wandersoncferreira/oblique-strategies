# Oblique Strategies

Created by Brian Eno to assist in creative situations by providing
suggestions for a course of action or thinking. [These
cards](https://www.enoshop.co.uk/product/oblique-strategies.html) have
been used by many artists around the world.

The idea from [Kevin Lawler](https://kevinlawler.com/prompts) is to
provide equivalent strategies to programmers to help them get unstuck
during their work.

This package will randomly choose a card to show you. We all know that
only some random event can really help you out in these moments.

## Installation

Clone and add the package to your `load-path`:

``` emacs-lisp
(add-to-list 'load-path "~/emacs.d/<your-pkg-dirs>/oblique-strategies")
(require 'oblique-strategies)
```

## Usage

``` emacs-lisp
M-x oblique-strategies
```

(may) result in:

``` emacs-lisp
-----------------
Feats are fragile
-----------------

The trickier a solution is, the smaller the pool of people
able to maintain it
```

You can also press `n` or `p` to get another strategy from the
`*Oblique Strategy*` buffer.

## Credits

Kevin Lawler for the nice [blog post](https://kevinlawler.com/prompts).



