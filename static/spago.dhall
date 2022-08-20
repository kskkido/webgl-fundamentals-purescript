{-
Welcome to a Spago project!
You can edit this file as you like.

Need help? See the following resources:
- Spago documentation: https://github.com/purescript/spago
- Dhall language tour: https://docs.dhall-lang.org/tutorials/Language-Tour.html

When creating a new Spago project, you can use
`spago init --no-comments` or `spago init -C`
to generate this file without the comments in this block.
-}
{ name = "my-project"
, dependencies =
  [ "aff"
  , "arraybuffer"
  , "arraybuffer-types"
  , "arrays"
  , "canvas"
  , "console"
  , "control"
  , "effect"
  , "either"
  , "exceptions"
  , "float32"
  , "foldable-traversable"
  , "functions"
  , "integers"
  , "lists"
  , "math"
  , "maybe"
  , "monad-control"
  , "numbers"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "random"
  , "refs"
  , "signal"
  , "st"
  , "transformers"
  , "tuples"
  , "web-dom"
  , "web-events"
  , "web-html"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
