# My notes

## Considered packages

* [elm-form](https://package.elm-lang.org/packages/dillonkearns/elm-form/3.0.1/) by `dillonkearns`
  * Pros
    * Every possible field is supported
    * Supports several forms on one page
    * "Monadic" way of validation (with `andMap` / `andThen`)
    * Rendering is in Elm Html
  * Cons
    * In Ellie example, forms doesn't show required fields from the start, I for sure can do it myself though, may be it is the way the example was written and not the limitation
    * Form functions get their parameters as arguments, for a lot of fields in the form there could be too much
      * ...but may be it is possible to use records
* [elm-form](https://package.elm-lang.org/packages/etaque/elm-form/4.0.0/) by `etaque`
  * Pros
    * I like the API as well, even more than API above
    * Also supports all the fields
    * Includes API for tests
  * Cons
    * Seems outdated and abandoned
    * Also doesn't show required fields at start
    * Not completely typed as uses strings as field names
    * Has some issues with TextArea, though it seems we don't need textareas here
* [elm-validate](https://package.elm-lang.org/packages/rtfeldman/elm-validate/4.0.2/) by _Richard Feldman_
  * Pros
    * The _right_ way, as from Feldman
  * Cons
    * No form rendering API, but may be in our case it is an advantage for using `tailwind`
* [elm-validate](https://package.elm-lang.org/packages/iodevs/elm-validate/latest/) by `iodevs`
* [elm-form-decoder](https://package.elm-lang.org/packages/arowM/elm-form-decoder/1.4.0/) by `arowM`
  * Pros
    * Seems has many users
    * Supports custom decoders
    * Subjectively nice API
  * Cons
    * Just a validator, as well, but may be in our case it is an advantage for using `tailwind`
* [composable-form](https://package.elm-lang.org/packages/hecrj/composable-form/latest/) by `hecrj`
  * Pros
    * Has its own rendering supporting both native forms and `elm-ui`
    * Supports several forms on a page and different types of components
    * Subjectively complicated API
  * Cons
    * Rendering seems not to be customizable, while nice-looking, for the task we need to use `tailwind`
* [This comment in Elm Discourse](https://discourse.elm-lang.org/t/what-is-the-elm-way-to-validate-form-fields/9689/4)
  * Pros
    * Simple, short and clean code
    * Seems to be reusable
  * Cons
    * Has some likes, but no one gave direct feedback
* [No packages + Forms are Programs, Feldman Way](https://discourse.elm-lang.org/t/what-is-the-elm-way-to-validate-form-fields/9689/9)
  * Pros
    * Feldman : literally Elm way
    * No library needed
  * Cons
    * Message per field: Complex forms turn out in a lot of code or huge states
* [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)

## Conclusion

I will try these steps:

* Define form(s) as records at first
  * May be some fields are not needed for the logic, we'll see it in the process: look in the API
* Be able to parse them from JSON using native API
* Try to use `elm-validate` from `rtfeldman` or `elm-form-decoder` from `arowM`
* If rendering forms turns out to be hard, use either `elm-form` from `etaque` (if it's not too outdated) or `elm-form` from `dillonkearns`
  * But try not to use any more
* Style forms / UX / Design

## Questions

* Could values for timeouts be not Integers, but something else?