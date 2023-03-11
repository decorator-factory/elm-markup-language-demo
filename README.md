# elm-markup-language-demo

This is my exploration into making a markup language, mostly for blog articles

Some vague properties that I'd like the language to eventually have:

- Uniform &mdash; syntax and semantics comprise a few simple rules
- Fractal &mdash; it's easy to create bigger, more complex layouts from smaller building blocks; and especially to reuse a single definition in multiple places
- Narrow in scope &mdash; this is not your tool of choice if you want to typeset a beautiful book on bleeding edge mathematics


## TODO:

- [ ] Mathematical functions (parentheses, fractions, indices etc.)
- [ ] Better code editor, probably something with "custom elements" and CodeMirror 6
- [ ] User-provided functions (macros?)

## Demo

The project is in its early infancy, but it seems to kinda work.

1. Copy the sample markup from `./sample.markup` in the root of this repository
2. Go to https://decorator-factory.github.io/elm-markup-language-demo
3. Paste the sample markup into the code editor
4. Scroll down to see the rendered page
