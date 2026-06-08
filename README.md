# constructible-flags

Flag SVG collection are widely available, but how do we know they are right? Few provide providence information. Also, I'm curious the degree of _constructability_ of different types of flags, as inspired by [Dr Zye](https://www.youtube.com/watch?v=w5QSVhgrqVE)

This isn't particularly _useful_, but I find it _interesting_.

Deployed to [https://flags.xaviershay.com](https://flags.xaviershay.com)

## Approach

Instructions for each flag are defined inside an effect, that allows inspection of the types of operations needed to draw that flag.

Sources and source types are included so that quality can be assessed.

## Dependencies

### `rsvg-convert` (librsvg)

Golden tests convert SVG output to PNG using the `rsvg-convert` command from [librsvg](https://gitlab.gnome.org/GNOME/librsvg). Install it before running tests:

```bash
# Debian/Ubuntu
sudo apt install librsvg2-bin

# macOS
brew install librsvg
```

### `ffmpeg` (optional, for `bin/animate`)

`bin/animate` shells out to `ffmpeg` to assemble PNG frames into the final
movie file. It is only required if you want to generate construction
animations.

```bash
# Debian/Ubuntu
sudo apt install ffmpeg

# macOS
brew install ffmpeg
```

## Generating construction animations

The `bin/animate` script renders an animated movie of the construction
steps for a single flag, ending on a held frame of the canonical SVG.
The output is written to `out/animation/<iso>.<ext>`.

Each build-up frame is captioned in the lower-right with the path of
enclosing construction-group labels for the active step (e.g.
`Horizontal stripes – Quad`).  Low-level primitive operations like
`Intersect line/circle` are intentionally not shown — the caption is a
high-level summary of which named building block is currently being built,
not the underlying compass-and-straightedge step.  Layers that are not
nested inside any named group show no caption.

```bash
bin/animate TTO                     # Trinidad and Tobago, defaults (GIF)
bin/animate JPN --format mp4        # Japan as MP4
bin/animate FRA --width 800 --fps 24
```

Supported formats: `gif` (default), `mp4`, `webm`, `apng`, `webp`.
GIF is the most universally embeddable; MP4 is much smaller for long
constructions but needs an HTML `<video>` tag rather than `<img>`. Run
`bin/animate --help` for all options.

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

## Development

    bin/test
    bin/publish
    
    stack ghci --test
