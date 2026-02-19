* This is a haskell project managed with `stack`. Always use `stack` for build and test operations — never `cabal`.
* Use `stack build` to build the project.
* Tests use the tasty framework. Run with `./bin/test`.

## Project intent — do not optimise away constructions

The purpose of this project is to demonstrate straight edge and compass constructions. Many algorithms are implemented in a deliberately non-optimal way to reflect the geometric construction they represent. For example, finding the midpoint of a segment is done via a bisection construction rather than simple arithmetic. **Do not replace these with mathematically equivalent shortcuts.** The construction steps are the point, not the result.