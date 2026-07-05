// opentype.js glue: font parsing and glyph layout. Geometry (flattening,
// holes, extrusion) all happens on the F# side — this module only produces
// raw glyph path commands in millimetre coordinates (y-down).
import { parse } from 'opentype.js';
import defaultFontUrl from '@fontsource/baloo-2/files/baloo-2-latin-700-normal.woff?url';

export async function loadDefaultFont() {
  const res = await fetch(defaultFontUrl);
  return parse(await res.arrayBuffer());
}

export function parseFontBuffer(buffer) {
  return parse(buffer);
}

export function fontName(font) {
  try {
    return font.names.fullName.en || font.names.fontFamily.en || 'font';
  } catch {
    return 'font';
  }
}

/**
 * Extract glyph outlines for `text`, each at its own origin (x=0, baseline
 * y=0, mm units, y-down). Horizontal placement happens on the F# side with
 * *optical* spacing — measured from the real outlines — because font advance
 * widths and kerning tables are unreliable in decorative fonts and produce
 * uneven gaps.
 *
 * Returns { glyphs: [{ commands, wordBreak }], spaceAdv } where wordBreak
 * marks glyphs preceded by whitespace and spaceAdv is the space width in mm.
 */
export function layoutText(font, text, sizeMm) {
  const scale = sizeMm / font.unitsPerEm;
  const glyphs = [];
  let pendingBreak = false;
  for (const ch of text) {
    const glyph = font.charToGlyph(ch);
    if (/\s/.test(ch)) {
      pendingBreak = true;
      continue;
    }
    glyphs.push({ commands: glyph.getPath(0, 0, sizeMm).commands, wordBreak: pendingBreak });
    pendingBreak = false;
  }
  const space = font.charToGlyph(' ');
  const spaceAdv = (space && space.advanceWidth ? space.advanceWidth : font.unitsPerEm * 0.3) * scale;
  return { glyphs, spaceAdv };
}
