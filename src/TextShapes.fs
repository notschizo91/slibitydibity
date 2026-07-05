module SvgExtrude.TextShapes

open Fable.Core
open Fable.Core.JsInterop
open SvgExtrude.Types
open SvgExtrude.PathParser

// Bindings over text-helpers.js (opentype.js). Fonts are opaque JS objects.

[<Import("loadDefaultFont", "./text-helpers.js")>]
let loadDefaultFont () : JS.Promise<obj> = jsNative

[<Import("parseFontBuffer", "./text-helpers.js")>]
let parseFontBuffer (buffer: obj) : obj = jsNative

[<Import("fontName", "./text-helpers.js")>]
let fontName (font: obj) : string = jsNative

/// Glyph outlines at their own origins plus word-break flags and the space
/// width: { glyphs: [{ commands; wordBreak }]; spaceAdv }.
[<Import("layoutText", "./text-helpers.js")>]
let layoutText (font: obj) (text: string) (sizeMm: float) : obj = jsNative

/// opentype.js path commands -> flattened subpaths (y-down mm coordinates).
let commandsToSubpaths (tol: float) (commands: obj array) : Subpath list =
    let subs = ResizeArray<Subpath>()
    let mutable pts = ResizeArray<Pt>()
    let mutable cur = { X = 0.0; Y = 0.0 }
    let flush closed =
        if pts.Count > 1 then subs.Add { Points = pts.ToArray(); Closed = closed }
        pts <- ResizeArray<Pt>()
    for cmd in commands do
        let t: string = cmd?``type``
        match t with
        | "M" ->
            flush false
            cur <- { X = cmd?x; Y = cmd?y }
            pts.Add cur
        | "L" ->
            if pts.Count = 0 then pts.Add cur
            cur <- { X = cmd?x; Y = cmd?y }
            pts.Add cur
        | "C" ->
            if pts.Count = 0 then pts.Add cur
            let c1 = { X = cmd?x1; Y = cmd?y1 }
            let c2 = { X = cmd?x2; Y = cmd?y2 }
            let p = { X = cmd?x; Y = cmd?y }
            flattenCubic tol 0 cur c1 c2 p pts
            cur <- p
        | "Q" ->
            if pts.Count = 0 then pts.Add cur
            let q = { X = cmd?x1; Y = cmd?y1 }
            let p = { X = cmd?x; Y = cmd?y }
            flattenQuad tol cur q p pts
            cur <- p
        | "Z" -> flush true
        | _ -> ()
    flush false
    List.ofSeq subs

/// One glyph's commands -> fillable shapes with holes (y-down mm).
/// Holes smaller than minHoleArea mm² are filled in (dropped).
let glyphShapes (tol: float) (minHoleArea: float) (commands: obj array) : Shape list =
    let shapes, _ = Rings.toShapes (commandsToSubpaths tol commands)
    shapes
    |> List.map (fun s ->
        { s with Holes = s.Holes |> List.filter (fun h -> abs (Rings.signedArea h) >= minHoleArea) })

/// One glyph ready for optical placement.
type GlyphIn = { GShapes: Shape list; WordBreak: bool }

/// Place glyphs left to right with *optical* spacing: each glyph is shifted
/// so the minimal horizontal clearance between its outline and everything
/// placed before it equals `gap` mm (plus `wordExtra` after a space). This
/// ignores font advance widths/kerning entirely, so spacing is even in any
/// font. Works on horizontal profiles rasterized into y-buckets.
let layoutOptical (gap: float) (wordExtra: float) (glyphs: GlyphIn list) : Shape list =
    let glyphs = glyphs |> List.filter (fun g -> not g.GShapes.IsEmpty)
    if glyphs.IsEmpty then []
    else
        let negInf = System.Double.NegativeInfinity
        // Global vertical range across all glyphs.
        let mutable minY = infinity
        let mutable maxY = negInf
        for g in glyphs do
            for s in g.GShapes do
                for p in s.Outer do
                    if p.Y < minY then minY <- p.Y
                    if p.Y > maxY then maxY <- p.Y
        let n = 128
        let bh = max 1e-9 ((maxY - minY) / float n)
        let bucketOf y = max 0 (min (n - 1) (int ((y - minY) / bh)))
        // Left/right silhouette of a glyph: extreme x per y-bucket, sampled
        // along every outline edge so long edges don't skip buckets.
        let profileOf (shapes: Shape list) =
            let left = Array.create n infinity
            let right = Array.create n negInf
            let hit bk (x: float) =
                if x < left.[bk] then left.[bk] <- x
                if x > right.[bk] then right.[bk] <- x
            for s in shapes do
                let ring = s.Outer
                for k in 0 .. ring.Length - 1 do
                    let a = ring.[k]
                    let b = ring.[(k + 1) % ring.Length]
                    hit (bucketOf a.Y) a.X
                    let b0 = bucketOf (min a.Y b.Y)
                    let b1 = bucketOf (max a.Y b.Y)
                    for bk in b0 .. b1 do
                        let yc = minY + (float bk + 0.5) * bh
                        let x =
                            if abs (b.Y - a.Y) < 1e-9 then (a.X + b.X) / 2.0
                            else
                                let t = max 0.0 (min 1.0 ((yc - a.Y) / (b.Y - a.Y)))
                                a.X + t * (b.X - a.X)
                        hit bk x
            left, right
        let placedRight = Array.create n negInf
        let mutable placedMaxX = negInf
        let mutable first = true
        let out = ResizeArray<Shape>()
        for g in glyphs do
            let left, right = profileOf g.GShapes
            let mutable gMinX = infinity
            let mutable gMaxX = negInf
            for s in g.GShapes do
                for p in s.Outer do
                    if p.X < gMinX then gMinX <- p.X
                    if p.X > gMaxX then gMaxX <- p.X
            let spacing = gap + (if g.WordBreak then wordExtra else 0.0)
            let offset =
                if first then -gMinX
                else
                    let mutable req = negInf
                    for bk in 0 .. n - 1 do
                        if placedRight.[bk] > negInf && left.[bk] < infinity then
                            let need = placedRight.[bk] + spacing - left.[bk]
                            if need > req then req <- need
                    // No vertical overlap with anything placed (e.g. quotes
                    // vs a low letter): fall back to bounding boxes.
                    if req = negInf then placedMaxX + spacing - gMinX else req
            first <- false
            for s in g.GShapes do
                out.Add (Geometry.mapShape (fun p -> { X = p.X + offset; Y = p.Y }) s)
            for bk in 0 .. n - 1 do
                if right.[bk] > negInf && right.[bk] + offset > placedRight.[bk] then
                    placedRight.[bk] <- right.[bk] + offset
            if gMaxX + offset > placedMaxX then placedMaxX <- gMaxX + offset
        List.ofSeq out
