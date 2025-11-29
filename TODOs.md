# TODOs, questions and suggestions

## TODOs

- [x] Make the mobile less crowded
- [x] Make sure save button is connected to the form
- [x] BUG: Removing currently edited tag messes up the next one
- [x] Confirm deletion
- [ ] Mobile layout is a bit silly
- [-] Toast or aria-live to describe actions - partially working
- [ ] Extract accessibility attributes into a module
- [ ]

## Questions & Comments

- I was not sure how much I should extend the setup or the project packages, but I've found out that when the project was built the tailwind didn't work in build so I've done some changes to make it work.
- New version of tailwind is out for quite some time, do we want to upgrade?

## Suggestions

- add `.nvmrc`, `.node-version` files or `engines` field in packajge.json to make sure people use the same node version
- consider moving away from `npm` to `pnpm` or other modern alternative
- consider using `corepack` and `packageManager` field in package.json
- review outdated dependencies and audit
- `elm-review` could be a nice addition to the project
