# elm-markup-language-demo

This is my exploration into making a markup language, mostly for blog articles

Some vague properties that I'd like the language to eventually have:

- Uniform &mdash; syntax and semantics comprise a few simple rules
- Fractal &mdash; it's easy to create bigger, more complex layouts from smaller building blocks; and especially to reuse a single definition in multiple places
- Narrow in scope &mdash; this is not your tool of choice if you want to typeset a beautiful book on bleeding edge mathematics


## TODO:

- [ ] Mathematical functions (parentheses, fractions, indices etc.)
    - [X] proof of concept, works for simple stuff
    - [ ] fix layout bug with using a fraction as a subscript/superscript
    - [ ] generally make sub, sup & frac play better together

- [ ] Better code editor, probably something with "custom elements" and CodeMirror 6

- [ ] User-provided functions
    - [X] First-order functions
    - [X] Passing functions by argument
    - [ ] Capturing closures

- [ ] Possibly export the "reduced" article as an intermediate data structure

    This is needed for alternative frontends (e.g. buildling a static HTML site)

    - [ ] Making a 'table of contents'
    - [ ] Generally figure out how to 'wire' stuff inside an article

- [ ] Calling an external resource (like HTML)
- [ ] Some basic data structures
    - [ ] lists
    - [ ] maps

## Demo

The project is in its early infancy, but it seems to kinda work.

1. Copy the sample markup from `./examples/sample.markup` in the root of this repository
2. Go to https://decorator-factory.github.io/elm-markup-language-demo
3. Paste the sample markup into the code editor
4. Scroll down to see the rendered page

# Language documentation

## Syntax

A program, or rather an "article" consists of a single _expression_. An _expression_ is either:

- a text literal, like `"Hello, world!"`
- a name, like `emdash`. A name should match `/[%a-z][-]/`
- a function call, consisting of a function name followed by 0 or more arguments, like: `(para "a particularly "  (bold "bold statement") ".")`)

Additionally, you can use this shorthand for interleaving:
```
(para
    | I am about to make a particularly $(bold "bold") statement. A $(italic "very" (bold "bold")) one.
    | Last time this "statement" was made everyone in the room got teleported to $(italic "Rome").
    )
```
The above means the same as:
```
(para
    "I am about to make a particularly " (bold "bold") "statement. A " (italic "very" (bold "bold")) "one. "
    "Last time this \"statement\" was made everyone in the room got teleported to " (italic "Rome") ". "
    )
```

"Calling" a string with zero arguments produces that string exactly. That's a conscious decision to make
stuff like this possible:
```
(para
    | I'm gonna pop some tags
    | Only got $(dollar)20 in my pocket
)
```

## Data types

- `string` --- a chunk of Unicoe text.
- `block` --- a rectangular element. (Similar to "block elements" in HTML/CSS)
- `inline` --- an inline element, something that can "flow" as part of a paragraph. A string is considered an inline element as well. (Similar to "inline elements" in HTML/CSS)
- `function` --- something you can call.



## Built-in functions and terms

I will be using the term `vis` (short for "visual element") to mean "`inline` or `block`".

A definition like `(aside string vis)` means that `aside` is a function
accepting two arguments: a string and a visual element.

A definition like `(link string ...inline)` means that `link` is a function
accepting at least one argument, where the first argument is a string and the others are inline elements.

---

### Convenience constants:

#### `emdash`

The string `"—"`


#### `dollar`

The string `"$"`


---

### Document layout


#### `(article ...vis) -> block`

A top-level node for your article. Will arrange the items in a column.

> Currently this is the same as `col`

#### `(col ...vis) -> block`

Structures elements in a vertical column

#### `(row ...vis) -> block`

Structures elements in a horizontal row

#### `(bullet-list ...vis) -> block`

A bulleted list, something like:

- foo
- bar
- baz

#### `(para ...inline) -> block`

A paragraph. This is a "bridge" between the world of inline elements and block elements.

#### `(code ...inline) -> inline`

Inline monospace text, like `Foo Bar`.

#### `(code-block ...vis) -> block`

A block of code lines. Will arrange each element as its own line.

Be careful with interpolating anything into a `code-block` with `|` and `$`, because
a line like `| Foo $(emdash) the $(bold "best") baz in the biz` actually consists of 5 expressions,
which will render as 5 lines.
You may want to use the `cc` function in this case.

#### `(italic ...inline) -> inline`

#### `(bold ...inline) -> inline`

#### `(term ...inline) -> inline`
> Currently the same as `italic

#### `(aside ...vis) -> block`
A note displayed to the side. See `examples/sample.markup for an example.

#### `(anchor string block) -> block`
Define a place you can jump to using a `link`.
```
(anchor "chapter-1"
    (col
        (para ...)
        (para ...)
        (code-block ...)
        (para ...)))
```

#### `(link string ...inline) -> inline`
A clickable link that teleports you to an `anchor` with the same "tag".
```
(col
    (bold "Table of contents")
    (bulleted-list
        (link "chapter-1" "1. Foo")
        (link "chapter-2" "2. Bar")
        (link "chapter-3" "3. Baz")))
```

#### `(comment ...whatever) -> vis`
Everything inside a comment is ignored. It has to be _syntactically valid_, though.

```
(comment
    | TODO: It would be nice if we had a 'cool' function which would be
    |       like `col` but like... cooler
    (cool
        (bold "A very cool table of contents")
        (bulleted-list
            (link "chapter-1" "1. Foo")
            (link "chapter-2" "2. Bar")
            (link "chapter-3" "3. Baz"))))
```

#### `(image string) -> block`
```
(row
    "Quack"
    (image
        "https://upload.wikimedia.org/wikipedia/commons/5/51/Mandarin.duck.arp.jpg"))
```

#### `(cc ...inline) -> inline`
Concatenate inline elements into one element.
```
(col
    "Shopping list:"
    (bullet-list
        (cc "Apples")
        (cc (italic "Fresh") "bananas")
        (cc "Cherries")
        (cc "Dates (do " (bold "not") " buy the brand " (italic "Foo"))))
```

---


### Custom definitions

#### `(def name value)`

Define a name to refer to a particular value.

Example:
```
(article
    (def product-name
        "AmazingWare")
    (def product-with-version
         (cc product-name "v1.0"))

    (para
        | Thanks for using $product-with-version!
        | If you are stuck, please contact the $product-name Support Team.
))
```
Output:
```
Thanks for using AmazingWare v1.0! If you are stuck, please contact the AmazingWare Support Team.
```

#### `(fun (%arg1 %arg2 %arg3...) body)`

Create a _function_. A function is essentially a "sub-document"
or an "expression" with some "placeholders" missing. Those placeholders are called _arguments_.

Functions are useful when:

- You want to extract the commonl repeated bits from your document,
  to make the source code more concise and to not have to change the same thing
  across many places.

- You want to configure the behaviour of another function
  (see `twice` and `sarcasm` for examples)

Example:
```
(article
    (def section
        (fun (%title)
            (row
                (col "")
                (col (bold %title))
                (col ""))))

    (def twice
        (fun (%func %arg)
            (%func (%func %arg))))

    (def sarcasm
        (fun (%word)
            (italic
                (twice
                    (fun (%s) (cc "\"" %s "\""))
                    %word))))

    (section "1. Foo")
    (para "Lorem ipsum...")

    (section "2. Foo")
    (para "Dolor sit amet...")

    (para
        | What an $(sarcasm "interesting") article.
        | I'm sure everyone $(sarcasm "enjoyed") reading it.
        ))

```


> Note that [Closures](https://en.wikipedia.org/wiki/Closure_(computer_programming)) are not implemented yet.
> We're working on that.


#### `(defun (name %arg1 %arg2 %arg3...) body)`

A shorthand for `def` and `fun`. This is what you should use instead
of the code in the `fun` example:
```
(article
    (defun (section %title)
        (row
            (col "")
            (col (bold %title))
            (col "")))

    (defun (twice %func %arg)
        (%func (%func %arg)))

    (defun (sarcasm %word)
        (italic
            (twice
                (fun (%s) (cc "\"" %s "\""))
                %word)))

    (section "1. Foo")
    (para "Lorem ipsum...")

    (section "2. Foo")
    (para "Dolor sit amet...")

    (para
        | What an $(sarcasm "interesting") article.
        | I'm sure everyone $(sarcasm "enjoyed") reading it.
        ))
```


---


### Maths

#### `(frac inline inline) -> inline`

Vertical fraction, like `(frac "22" "7")` or `(frac "x + 1" "x")`

#### `(big-frac inline inline) -> inline`

A version of `frac` that doesn't scale down the numerator and the denominator.

#### `(sup inline inline)`

Superscript, like `(sup "x" "2")`

#### `(sub inline inline)`

Subscript, like `(cc (sub "item" "i") ".." (sub "item" "j"))`

#### `(gr ...inline) -> inline`
Wrap expressions in parentheses. For example, these are the same:
```
(cc (gr "4" "+" (sup "x" "2")) "-" "y" )
(cc (cc "(" "4" "+" (sup "x" "2") ")" ) "-" "y" )
```
> Why is this needed? Because:
> 1. the language will help you balance your
>    parentheses. No more searching for a forgotten `\right)`.
> 2. in the future, I might copy the LaTeX feature of making parentheses
>    slightly smaller every time you nest deeper.
>
> TODO: make it possible to do this in a user-defined function. This is
> a useful construct for all kinds of brackets.
