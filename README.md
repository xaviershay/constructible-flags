# constructible-flags

Flag SVG collection are widely available, but how do we know they are right? Few provide providence information. Also, I'm curious the degree of _constructability_ of different types of flags, as inspired by [Dr Zye](https://www.youtube.com/watch?v=w5QSVhgrqVE)

This isn't particularly _useful_, but I find it _interesting_.

## Approach

Instructions for each flag are defined inside an effect, that allows inspection of the types of operations needed to draw that flag.

Sources and source types are included so that quality can be assessed.

## Dependencies

### `convert` (ImageMagick)

Golden tests convert SVG output to PNG using the `convert` command from [ImageMagick](https://imagemagick.org/). Install it before running tests:

```bash
# Debian/Ubuntu
sudo apt install imagemagick

# macOS
brew install imagemagick
```

## Regenerating golden tests

Each flag has a pixel-exact golden image in `test/golden/`. To regenerate after visual changes (e.g. updated Pantone colours):

```bash
# Regenerate all golden images
rm test/golden/*.png
stack test

# Or regenerate a single flag
rm test/golden/bgd.png
stack test
```

The first run creates new golden files and fails so you can review them. Run `stack test` again to confirm they pass, then commit the updated PNGs.

## Design Notes

Ideally, numbers would be represented with exact algebra. I tried this a few ways, but it quickly led to exponential blowouts and code full of edge cases. I've now reverted to using double approximations, though still tracking what field the number should exist in. I'm not sure how useful that is, but it also maintains a number abstraction such that we can have another go at exact algebra in the future.
