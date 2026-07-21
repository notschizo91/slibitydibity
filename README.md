# 3DSideHustle Keychain Designer (SVG edition)

Type a name, get a 3D-printable keychain. Live 3D preview, base + raised text
at independent heights, keyring hole you can drag into position, and binary
STL export — combined, or base/text/artwork as separate files for multi-color
prints.

**Letter Specific Adjustments**: some fonts scrunch particular letter pairs.
Click a letter chip (all typed letters show as buttons), then shift that
letter — and everything after it — left or right in mm. Adjustments cascade
through the optical layout, are marked with a dot on the chip, and reset when
the text changes.

**SVG artwork bank**: upload SVGs (multiple at once — hearts, paws, logos …)
into a bank of thumbnails that persists in the browser (localStorage). Click
a bank item to place it on the keychain — as many instances as you like, each
with its own size, color and draggable position. Every artwork is flattened
with the shared engine, auto-extruded to the text height, and the base blob
grows to hold it wherever it is parked. The separate export adds one
`{name}-artN.stl` per placed artwork for multi-color prints.

Built on the SVG-extrude engine from the companion
[filipsaldy](https://github.com/notschizo91/filipsaldy) tool (same theme, same
geometry core).

## Features

- **Font & Text** — bundled Baloo 2 (bold, round, prints well) works out of
  the box; upload your own `.ttf`/`.otf`/`.woff` fonts. Text up to 15 chars.
- **Save Folder** — pick a folder once (Chrome/Edge) and exports are written
  straight into it as `{name}.stl`, ideal for batch name-tag production.
  Other browsers fall back to normal downloads.
- **Appearance** — base + text preview colors, text size (mm), border width
  (how fat the base blob is around the letters), letter spacing, word gap,
  hole fill threshold (closes small see-through pockets that form in the BASE between offset letter outlines; letter/artwork counters always stay open as recesses, and the keyring hole is never filled).
- **Keyring Hole** — hole size + ring thickness sliders; drag the ring in the
  preview to reposition it (auto-parks at the left edge otherwise).
- **Heights** — base height and text height (above the base) in mm.
- **Export** — one combined STL, or `-base.stl` + `-text.stl` separately for
  filament-swap / two-body multicolor printing. Binary STL, watertight,
  outward-facing normals, Z-up, model resting on z=0.

## Run it

Prerequisites: Node 18+, .NET SDK 8.

```bash
npm install
dotnet tool restore   # installs the Fable compiler (pinned in .config/dotnet-tools.json)
npm run dev           # Fable watch + Vite dev server on http://localhost:5173
npm run build         # production build into dist/ (static, host anywhere)
npm run test:e2e      # headless end-to-end test with STL validation
```

The e2e test types a name, exports combined + separate STLs, and validates the
binaries: byte layout, base/text z-ranges, watertightness (every edge shared
by exactly two opposite-winding triangles), outward orientation, and that
separate volumes sum to the combined volume.

## How it works

- **Text → outlines**: `opentype.js` parses the font; glyphs are laid out
  along a baseline with kerning, letter spacing and word-gap applied
  (`src/text-helpers.js`), then each glyph's bezier commands are adaptively
  flattened and classified into shapes-with-holes in F#
  (`src/TextShapes.fs`, reusing the engine's `PathParser`/`Rings`).
- **Base blob**: glyph outer rings are unioned and offset outward by the
  border width with round joins via Clipper (`src/clipper-helpers.js`,
  `src/Clipper.fs`). The keyring annulus is unioned in and the hole
  subtracted — all booleans are 2D, no 3D CSG needed.
- **Extrusion**: the engine's pure `Geometry.extrude` (earcut caps + wall
  quads, outward winding) builds the base at `0..baseH` and the text at
  `baseH..baseH+textH`; the identical triangles feed the three.js preview and
  the STL writer (`src/Stl.fs`).
- Glyph layout + Clipper run debounced (~70 ms) so sliders stay live; height,
  ring and color changes skip the expensive steps entirely.

## Project layout

```
index.html               app shell (topbar, canvas, card sidebar)
styles.css               theme
src/App.fsproj           F# project (compile order matters)
src/Types.fs             domain types            ┐
src/PathParser.fs        curve flattening        │ engine, shared with
src/Rings.fs             hole classification     │ the filipsaldy
src/Geometry.fs          extrusion (caps+walls)  │ SVG-extrude tool
src/Stl.fs               binary STL writer       ┘
src/TextShapes.fs        opentype commands -> shapes
src/Clipper.fs           offset/boolean bindings
src/Viewer.fs            viewer bindings
src/text-helpers.js      opentype.js layout glue
src/clipper-helpers.js   clipper-lib glue
src/fs-helpers.js        File System Access (save folder)
src/viewer.js            three.js scene + keyring drag
src/Main.fs              state, pipeline, UI wiring
tests/e2e.mjs            headless e2e + STL validation
```
