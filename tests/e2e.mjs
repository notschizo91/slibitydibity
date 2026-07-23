// End-to-end test: serve the built app, type a keychain name, tweak settings,
// export combined + separate STLs, and validate the binaries.
//
// Prereqs: `npm run build` (dist/ must exist), a Chromium binary
// (CHROMIUM_PATH env var, default /opt/pw-browsers/chromium).
import http from 'node:http';
import path from 'node:path';
import { createReadStream, existsSync, readFileSync, mkdtempSync } from 'node:fs';
import os from 'node:os';
import { chromium } from 'playwright-core';

const ROOT = path.resolve(path.dirname(new URL(import.meta.url).pathname), '..');
const DIST = path.join(ROOT, 'dist');
const PORT = 4193;

const MIME = {
  '.html': 'text/html', '.js': 'text/javascript', '.css': 'text/css', '.woff': 'font/woff',
};

let failures = 0;
const check = (cond, msg) => {
  console.log(`${cond ? 'PASS' : 'FAIL'}  ${msg}`);
  if (!cond) failures++;
};

const server = http.createServer((req, res) => {
  const url = req.url === '/' ? '/index.html' : req.url.split('?')[0];
  const file = path.join(DIST, decodeURIComponent(url));
  if (!file.startsWith(DIST) || !existsSync(file)) {
    res.writeHead(404).end('not found');
    return;
  }
  res.writeHead(200, { 'content-type': MIME[path.extname(file)] ?? 'application/octet-stream' });
  createReadStream(file).pipe(res);
});
await new Promise((r) => server.listen(PORT, r));

const browser = await chromium.launch({
  executablePath: process.env.CHROMIUM_PATH || '/opt/pw-browsers/chromium',
});
const context = await browser.newContext({ acceptDownloads: true, viewport: { width: 1400, height: 900 } });
const page = await context.newPage();
const pageErrors = [];
page.on('pageerror', (e) => pageErrors.push(String(e)));
page.on('console', (m) => {
  if (m.type() === 'error') pageErrors.push(m.text());
});

const tmp = mkdtempSync(path.join(os.tmpdir(), '3dsh-'));

const parseStl = (buf) => {
  const triCount = buf.readUInt32LE(80);
  let minZ = Infinity, maxZ = -Infinity, minX = Infinity, maxX = -Infinity, volume = 0;
  let minY = Infinity, maxY = -Infinity;
  const edges = new Map();
  const key = (x, y, z) => `${x.toFixed(4)},${y.toFixed(4)},${z.toFixed(4)}`;
  for (let t = 0; t < triCount; t++) {
    const o = 84 + 50 * t + 12;
    const v = [];
    for (let k = 0; k < 3; k++) {
      const x = buf.readFloatLE(o + 12 * k);
      const y = buf.readFloatLE(o + 12 * k + 4);
      const z = buf.readFloatLE(o + 12 * k + 8);
      v.push([x, y, z]);
      if (z < minZ) minZ = z;
      if (z > maxZ) maxZ = z;
      if (x < minX) minX = x;
      if (x > maxX) maxX = x;
      if (y < minY) minY = y;
      if (y > maxY) maxY = y;
    }
    const [a, b, c] = v;
    volume +=
      (a[0] * (b[1] * c[2] - c[1] * b[2]) -
        a[1] * (b[0] * c[2] - c[0] * b[2]) +
        a[2] * (b[0] * c[1] - c[0] * b[1])) / 6;
    for (let k = 0; k < 3; k++) {
      const e = key(...v[k]) + '|' + key(...v[(k + 1) % 3]);
      edges.set(e, (edges.get(e) ?? 0) + 1);
    }
  }
  let badEdges = 0;
  for (const [e, n] of edges) {
    const rev = e.split('|').reverse().join('|');
    if (n !== 1 || (edges.get(rev) ?? 0) !== 1) badEdges++;
  }
  return { triCount, minZ, maxZ, minX, maxX, minY, maxY, volume, badEdges, bytesOk: buf.length === 84 + 50 * triCount };
};

const download = async (buttonSel) => {
  const [dl] = await Promise.all([page.waitForEvent('download'), page.click(buttonSel)]);
  const p = path.join(tmp, dl.suggestedFilename());
  await dl.saveAs(p);
  return { name: dl.suggestedFilename(), buf: readFileSync(p) };
};

try {
  await page.goto(`http://127.0.0.1:${PORT}/`);
  // The bundled font loads async; the export button enables after first build.
  await page.waitForSelector('#export-btn:not([disabled])', { timeout: 15000 });
  check(true, 'default font loaded and initial model built');
  // Cards are collapsible; open them all so the test can reach every control.
  await page.evaluate(() => document.querySelectorAll('details.card').forEach((d) => (d.open = true)));
  check((await page.locator('details.card')).count && (await page.locator('details.card:not([open])').count()) === 0, 'all setting cards opened');

  // Type a name (two words exercises word gap + per-glyph shapes).
  await page.fill('#text-input', 'KAI JO');
  await page.dispatchEvent('#text-input', 'input');
  await page.waitForTimeout(300);
  check((await page.locator('#char-count').textContent()).startsWith('6/15'), 'char counter updates');

  // Heights: base 3mm + text 2mm (defaults) -> total 5mm.
  const combined = await download('#export-combined');
  check(combined.name === 'KAI JO.stl', `combined filename is the keychain text (${combined.name})`);
  const c = parseStl(combined.buf);
  check(c.bytesOk, `byte length matches triangle count (${c.triCount} tris)`);
  check(c.triCount > 1000, 'non-trivial triangle count');
  check(Math.abs(c.minZ) < 1e-4, `model sits on z=0 (minZ=${c.minZ})`);
  check(Math.abs(c.maxZ - 5) < 1e-3, `total height is base+text = 5mm (maxZ=${c.maxZ})`);
  check(c.volume > 0, `outward-facing normals (volume=${c.volume.toFixed(0)} mm³)`);
  check(c.badEdges === 0, `watertight (${c.badEdges} bad edges)`);

  // Separate export: two files, base-only is 3mm, text-only spans 3..5mm.
  // Queue downloads via a listener — two parallel waitForEvent calls would
  // both resolve to the first download.
  const queue = [];
  const onDl = (d) => queue.push(d);
  page.on('download', onDl);
  await page.click('#export-separate');
  const deadline = Date.now() + 10000;
  while (queue.length < 2 && Date.now() < deadline) await page.waitForTimeout(100);
  page.off('download', onDl);
  check(queue.length === 2, `separate export produced ${queue.length} downloads`);
  const named = {};
  for (const dl of queue) {
    const p = path.join(tmp, dl.suggestedFilename());
    await dl.saveAs(p);
    named[dl.suggestedFilename()] = readFileSync(p);
  }
  check(
    'KAI JO-base.stl' in named && 'KAI JO-text.stl' in named,
    `separate export names (${Object.keys(named).join(', ')})`
  );
  const base = parseStl(named['KAI JO-base.stl']);
  const text = parseStl(named['KAI JO-text.stl']);
  check(Math.abs(base.maxZ - 3) < 1e-3 && Math.abs(base.minZ) < 1e-4, `base spans 0..3mm (${base.minZ}..${base.maxZ})`);
  // Text is sunk 0.2mm into the base so the solids genuinely overlap
  // (coincident faces cause slicer artifacts).
  check(Math.abs(text.minZ - 2.8) < 1e-3 && Math.abs(text.maxZ - 5) < 1e-3, `text spans 2.8..5mm, 0.2mm sunk into the base (${text.minZ}..${text.maxZ})`);
  check(base.badEdges === 0 && text.badEdges === 0, 'both separate STLs watertight');
  check(
    Math.abs(base.volume + text.volume - c.volume) < 1,
    'separate volumes sum to the combined volume'
  );
  // The keyring sticks out left of the text blob: base must be wider than text.
  check(base.minX < text.minX - 2, `keyring extends left of the text (base minX ${base.minX.toFixed(1)} vs text ${text.minX.toFixed(1)})`);

  // The keyring must never cut into the name: park the ring dead-center on
  // the text — the hole is fully swallowed, so the base collapses to exactly
  // the blob (no ring sticking out, nothing carved away).
  await page.evaluate(() => window.__setRing({ x: 0, y: 0 }));
  await page.waitForTimeout(200);
  const swallowed = parseStl((await download('#export-combined')).buf);
  check(swallowed.badEdges === 0, 'ring-over-text export still watertight');
  check(
    Math.abs(swallowed.minX - (text.minX - 3)) < 1.0,
    `hole does not cut the blob: base edge = text edge + border (${swallowed.minX.toFixed(1)} vs ${(text.minX - 3).toFixed(1)})`
  );
  // Keyring toggle off = plain nameplate: base collapses to exactly the blob.
  await page.uncheck('#ring-enabled');
  await page.waitForTimeout(250);
  const nameplate = parseStl((await download('#export-combined')).buf);
  check(nameplate.badEdges === 0, 'nameplate (keyring off) export watertight');
  check(
    Math.abs(nameplate.minX - swallowed.minX) < 1e-3,
    `keyring toggle off gives a plain nameplate (minX ${nameplate.minX.toFixed(1)})`
  );
  await page.check('#ring-enabled');
  await page.waitForTimeout(250);

  // Put the ring back on the left edge for the remaining checks.
  await page.evaluate(() => window.__setRing({ x: -44, y: 0 }));
  await page.waitForTimeout(200);

  // Settings affect the model: raise base height, shrink border.
  await page.fill('#base-h', '6');
  await page.dispatchEvent('#base-h', 'input');
  await page.waitForTimeout(250);
  const taller = parseStl((await download('#export-combined')).buf);
  check(Math.abs(taller.maxZ - 8) < 1e-3, `base height slider works (maxZ=${taller.maxZ})`);

  // Hole fill threshold must NOT touch letter counters: maxing it changes
  // nothing on plain text (no pockets in this blob, and letter holes are
  // recesses that always stay open).
  await page.fill('#hole-fill', '20');
  await page.dispatchEvent('#hole-fill', 'input');
  await page.waitForTimeout(350);
  const filled = parseStl((await download('#export-combined')).buf);
  check(
    Math.abs(filled.volume - taller.volume) < 1,
    `hole fill threshold leaves letter holes alone (${taller.volume.toFixed(0)} vs ${filled.volume.toFixed(0)} mm³)`
  );
  await page.fill('#hole-fill', '2');
  await page.dispatchEvent('#hole-fill', 'input');
  await page.waitForTimeout(300);

  // Optical letter spacing: widening the gap slider must widen the model by
  // roughly (n-1) * delta regardless of the font's own metrics. Bury the ring
  // first so the parked keyring doesn't pin the model bounds.
  await page.evaluate(() => window.__setRing({ x: 0, y: 0 }));
  await page.waitForTimeout(250);
  const beforeSp = parseStl((await download('#export-combined')).buf);
  await page.fill('#letter-spacing', '3');
  await page.dispatchEvent('#letter-spacing', 'input');
  await page.waitForTimeout(350);
  const spaced = parseStl((await download('#export-combined')).buf);
  const grown = spaced.maxX - spaced.minX - (beforeSp.maxX - beforeSp.minX);
  check(
    grown > 10 && grown < 15,
    `letter spacing controls the real outline gap (width grew ${grown.toFixed(1)}mm for +3mm x 4 gaps)`
  );

  // --- SVG artwork bank: upload once, place from the bank, drag, export ---
  // State here: base-h 6, text-h 2, letter-spacing 3, ring buried at center.
  await page.setInputFiles('#art-file', path.join(ROOT, 'tests', 'fixtures', 'heart.svg'));
  await page.waitForTimeout(400);
  check((await page.locator('.bank-item').count()) === 1, 'uploaded SVG appears in the bank');

  await page.click('.bank-item');
  await page.waitForTimeout(300);
  check((await page.locator('#art-list li').count()) === 1, 'clicking a bank item places it on the keychain');
  const withArt = parseStl((await download('#export-combined')).buf);
  check(withArt.badEdges === 0, 'artwork combined export watertight');
  check(withArt.volume > spaced.volume + 100, `artwork adds volume (${spaced.volume.toFixed(0)} -> ${withArt.volume.toFixed(0)} mm³)`);
  check(Math.abs(withArt.maxZ - 8) < 1e-3, `artwork does not change total height (maxZ=${withArt.maxZ})`);

  // Separate export emits one artwork STL per placed instance.
  const collect = async (expected) => {
    const q = [];
    const on = (d) => q.push(d);
    page.on('download', on);
    await page.click('#export-separate');
    const t = Date.now() + 10000;
    while (q.length < expected && Date.now() < t) await page.waitForTimeout(100);
    page.off('download', on);
    const named = {};
    for (const dl of q) {
      const p = path.join(tmp, dl.suggestedFilename());
      await dl.saveAs(p);
      named[dl.suggestedFilename()] = readFileSync(p);
    }
    return named;
  };
  const sep1 = await collect(3);
  check('KAI JO-art.stl' in sep1, `separate export includes artwork STL (${Object.keys(sep1).join(', ')})`);
  const art = parseStl(sep1['KAI JO-art.stl']);
  check(
    Math.abs(art.minZ - 5.8) < 1e-3 && Math.abs(art.maxZ - 8) < 1e-3,
    `artwork spans base..base+text with 0.2mm sink (${art.minZ}..${art.maxZ})`
  );
  check(art.badEdges === 0, 'artwork STL watertight (heart hole preserved)');

  // Place a SECOND instance from the same bank item and drag it (the hook
  // moves the active = newest instance): the base must grow under it.
  await page.click('.bank-item');
  await page.waitForTimeout(300);
  check((await page.locator('#art-list li').count()) === 2, 'same bank item can be placed twice');
  await page.evaluate(() => window.__setArt({ x: 0, y: 60 }));
  await page.waitForTimeout(300);
  const movedArt = parseStl((await download('#export-combined')).buf);
  check(movedArt.badEdges === 0, 'two-artwork export watertight');
  check(
    movedArt.maxY > withArt.maxY + 20,
    `base grows under dragged artwork (maxY ${withArt.maxY.toFixed(1)} -> ${movedArt.maxY.toFixed(1)})`
  );
  const sep2 = await collect(4);
  check(
    'KAI JO-art1.stl' in sep2 && 'KAI JO-art2.stl' in sep2,
    `separate export names each instance (${Object.keys(sep2).join(', ')})`
  );

  // Removing a placed instance from the list leaves the bank untouched.
  await page.click('#art-list li:last-child .l-x');
  await page.waitForTimeout(200);
  check((await page.locator('#art-list li').count()) === 1, 'placed instance removed via list ×');
  check((await page.locator('.bank-item').count()) === 1, 'bank still holds the SVG');

  // --- "Don't extrude black parts": dark fills become base cutouts ---
  await page.setInputFiles('#art-file', path.join(ROOT, 'tests', 'fixtures', 'ball.svg'));
  await page.waitForTimeout(400);
  await page.click('.bank-item[data-i="1"]'); // the ball
  await page.waitForTimeout(300);
  const ballSolid = parseStl((await download('#export-combined')).buf);
  await page.check('#art-nodark');
  await page.waitForTimeout(300);
  const ballCut = parseStl((await download('#export-combined')).buf);
  check(
    ballCut.volume < ballSolid.volume - 20,
    `no-black toggle removes dark areas (${ballSolid.volume.toFixed(0)} -> ${ballCut.volume.toFixed(0)} mm³)`
  );
  check(ballCut.badEdges === 0, 'no-black export watertight');
  // Cleanup: remove the ball instance and its bank entry.
  await page.click('#art-list li:last-child .l-x');
  await page.hover('.bank-item[data-i="1"]');
  await page.click('.bank-item[data-i="1"] .bank-x');
  await page.waitForTimeout(200);

  // --- Hole Fill Threshold acts on the BASE, not the letters ---
  // A C-shaped artwork with a narrow mouth: the border offset closes the
  // mouth, enclosing a see-through pocket in the back plate. The threshold
  // must fill that pocket.
  await page.setInputFiles('#art-file', path.join(ROOT, 'tests', 'fixtures', 'horseshoe.svg'));
  await page.waitForTimeout(400);
  await page.click('.bank-item[data-i="1"]');
  await page.waitForTimeout(300);
  await page.fill('#hole-fill', '0');
  await page.dispatchEvent('#hole-fill', 'input');
  await page.waitForTimeout(300);
  const pocketOpen = parseStl((await download('#export-combined')).buf);
  await page.fill('#hole-fill', '20');
  await page.dispatchEvent('#hole-fill', 'input');
  await page.waitForTimeout(300);
  const pocketFilled = parseStl((await download('#export-combined')).buf);
  check(
    pocketFilled.volume > pocketOpen.volume + 5,
    `threshold fills enclosed pockets in the base (${pocketOpen.volume.toFixed(0)} -> ${pocketFilled.volume.toFixed(0)} mm³)`
  );
  check(pocketFilled.badEdges === 0, 'pocket-filled export watertight');
  // Cleanup: remove the horseshoe and restore the default threshold.
  await page.click('#art-list li:last-child .l-x');
  await page.fill('#hole-fill', '2');
  await page.dispatchEvent('#hole-fill', 'input');
  await page.waitForTimeout(300);

  // --- Letter Specific Adjustments ---
  // Clean slate for width math: remove the last artwork and bury the ring.
  await page.click('#art-list li:last-child .l-x');
  await page.evaluate(() => window.__setRing({ x: 0, y: 0 }));
  await page.waitForTimeout(300);
  check((await page.locator('.letter-btn').count()) === 5, 'letter buttons match visible letters (KAI JO -> 5)');

  const beforeAdj = parseStl((await download('#export-combined')).buf);
  await page.click('.letter-btn[data-i="2"]'); // the "I"
  await page.fill('#letter-adjust', '4');
  await page.dispatchEvent('#letter-adjust', 'input');
  await page.waitForTimeout(300);
  const adj = parseStl((await download('#export-combined')).buf);
  const adjGrow = adj.maxX - adj.minX - (beforeAdj.maxX - beforeAdj.minX);
  check(
    adjGrow > 3.4 && adjGrow < 4.6,
    `+4mm shift at a letter widens the model by ~4mm (${adjGrow.toFixed(1)})`
  );
  check(adj.badEdges === 0, 'letter-adjusted export watertight');
  check((await page.locator('.letter-btn.adjusted').count()) === 1, 'adjusted letter is marked');

  await page.click('#letter-adjust-clear');
  await page.waitForTimeout(300);
  const cleared = parseStl((await download('#export-combined')).buf);
  check(
    Math.abs(cleared.maxX - cleared.minX - (beforeAdj.maxX - beforeAdj.minX)) < 0.2,
    'clear-all restores the original width'
  );

  await page.screenshot({ path: path.join(ROOT, 'e2e-screenshot.png') });
  check(pageErrors.length === 0, `no console/page errors${pageErrors.length ? ': ' + pageErrors.join(' | ') : ''}`);
} finally {
  await browser.close();
  server.close();
}

console.log(failures === 0 ? '\nAll checks passed.' : `\n${failures} check(s) FAILED`);
process.exit(failures === 0 ? 0 : 1);
