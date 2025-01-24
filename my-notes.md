# My notes

## Considered packages

* [elm-form](https://package.elm-lang.org/packages/dillonkearns/elm-form/3.0.1/) by `dillonkearns`
  * Pros
    * Every possible field kind is supported
    * Supports several forms on one page
    * "Monadic" way of validation (with `andMap` / `andThen`)
  * Cons
    * In Ellie example, forms doesn't show required fields from the start, I for sure can do it myself though, may be it is the way the example was written and not the limitation
    * It seems that rendering can not be customized
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

* [X] Define form(s) data as records at first
  * May be some fields are not needed for the logic, we'll see it in the process: look in the API
* [X] Be able to parse them from JSON using native API
* [X] Try to use `elm-validate` from `rtfeldman` or `elm-form-decoder` from `arowM`
* [X] Implement forms logic w/o any styling:
  * [X] Tags
  * [X] Contact Details
  * [X] Settings / Retention Policy
* If rendering forms turns out to be hard, use either `elm-form` from `etaque` (if it's not too outdated) or `elm-form` from `dillonkearns`
  * But try not to use any more
* [ ] Fix issues in UX / Logic:
  * [X] Show validation errors where they belong
  * [ ] Allow completely deleting tags (or make it default action?), current _remove_ could be _archive_
  * [ ] It's improper to update model values while user types in the values, we should commit changes only after editing
    * E-mail and such in contacts view
    * Timeouts in retention policy
  * [ ] Only one input should be triggered active in Contacts form
  * [ ] Validate phone & zip code (current validators are stubs)
  * [ ] If value in select box (such as Add Timeout by type select) is not changed, no event is fired when user presses Enter
    * Usually it is fixed with additional item in select box, but it sounds cheaty
  * [ ] Pressing Enter in text fields should trigger submit
  * [ ] Erasing value in timeout input removes the input box
  * [ ] No way to cancel editing, especially when an error happened in the form
  * [ ] Read-only modes for all the forms:
    * [ ] Tags
    * [ ] Contact details
    * [ ] Retention Policy
  * [ ] Clear only errors that belong to current form when validation repeats succesfully
  * [ ] Wouldn't it is be nice if clicking mouse outside of text field would also submit?
  * [ ] Immediate Trash of Retention Timeouts should disable all of timeouts?
  * [ ] Example validator that rejects huge timeouts
* [ ] Style forms / UX / Design
* [ ] Format code
* [ ] Optionally, Add tests for decoding / encoding : [fuzzy](https://package.elm-lang.org/packages/elm-explorations/test/latest/Fuzz)
* [ ] Optionally, fake sending data from the model

## Questions

* Some IDs are strings in the API / data, but they are numerals, so I had to encode them back to strings for a proper JSON
* I show removed tags in the tag list anyway (and allow to restore them back with specifying value), and they are checked for unique name when creating new tag, only when trying it in action I realized that it could be an overcomplication
  * But it helps to quickly restore tag that was removed by accident, I also prefill the last known value for it (if it happend in current session)
* I don't get exactly what `immediate_trash` for `retention_policy` means, at first I decided that it means removing all the policy values but after a second thought I got that it is a flag to remove the document right away, but then it should override all other timeouts anyway;
* Could values for timeouts in `ContactDetails` be not Integers, but something else?
* I have added `none` preferred contact method (to `email`, `post`, `phone`) to handle cases when the string in the JSON doesn't match any sample, so that it wouldn't set something undesired by accident. But may be there exist a default value I could always set in case of mismatch?
