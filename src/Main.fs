module SvgExtrude.Main

open System
open Fable.Core
open Fable.Core.JsInterop
open Browser.Dom
open Browser.Types
open SvgExtrude.Types
open SvgExtrude.TextShapes

[<Emit("parseFloat($0)")>]
let private parseFloatJs (s: string) : float = jsNative

[<Emit("$0.arrayBuffer().then($1)")>]
let private readFileBuffer (file: obj) (cb: obj -> unit) : unit = jsNative

[<Emit("$0.text().then($1)")>]
let private readFileText (file: obj) (cb: string -> unit) : unit = jsNative

[<Emit("$0.then($1)")>]
let private thenDo (p: obj) (cb: obj -> unit) : unit = jsNative

[<Import("canPickFolder", "./fs-helpers.js")>]
let private canPickFolder () : bool = jsNative

[<Import("pickFolder", "./fs-helpers.js")>]
let private pickFolder () : JS.Promise<obj> = jsNative

[<Import("saveToFolder", "./fs-helpers.js")>]
let private saveToFolder (handle: obj) (filename: string) (buffer: obj) : JS.Promise<bool> = jsNative

let private byId (id: string) : HTMLElement = document.getElementById id
let private inputById (id: string) : HTMLInputElement = byId id :?> HTMLInputElement

// ---------------------------------------------------------------------------
// State (sliders hold their own values; these mirror them for the pipeline)
// ---------------------------------------------------------------------------

let private fonts = ResizeArray<string * obj>()
let mutable private activeFont: obj option = None
let mutable private text = "NAME"
let mutable private sizeMm = 20.0
let mutable private letterSpacing = 0.0
let mutable private wordGap = 1.0
let mutable private border = 3.0
let mutable private holeFill = 2.0
let mutable private ringEnabled = true
let mutable private holeSize = 5.0 // keyring hole diameter
let mutable private ringThick = 3.0
let mutable private baseH = 3.0
let mutable private textH = 2.0
let mutable private baseColor = "#7c3aed"
let mutable private textColor = "#ffffff"
/// Per-letter shift (mm), keyed by visible-letter index: moves that letter
/// and everything after it. Cleared when the text changes.
let private letterAdjust = System.Collections.Generic.Dictionary<int, float>()
let mutable private activeLetter: int option = None
/// User-dragged keyring position; None = automatic (left of the blob).
let mutable private ringPos: Pt option = None
let mutable private ringCenterActual = { X = 0.0; Y = 0.0 }
let mutable private folderHandle: obj = null

// --- SVG artwork bank + placed instances -----------------------------------
// The bank is a library of uploaded SVGs (persisted in localStorage); each
// bank entry can be placed on the keychain any number of times. Instances
// carry their own size, color and position.

/// One uploaded SVG in the bank: raw markup for the thumbnail/persistence
/// plus shapes normalized to the origin (y-up, natural SVG scale).
type private BankItem =
    { Name: string
      Svg: string
      BShapes: Shape list
      NatW: float
      NatH: float }

/// One artwork placed on the keychain.
type private ArtInst =
    { AShapes: Shape list
      NatW: float
      NatH: float
      AName: string
      mutable Size: float
      mutable Color: string
      mutable Pos: Pt }

let private bank = ResizeArray<BankItem>()
let private arts = System.Collections.Generic.Dictionary<int, ArtInst>()
let private artOrder = ResizeArray<int>()
let private artZones = System.Collections.Generic.Dictionary<int, Pt * float>()
let mutable private artCounter = 0
let mutable private activeArt: int option = None
let private artDefaultSize = 15.0
let private artColors = [| "#ef4444"; "#22d3ee"; "#eab308"; "#10b981"; "#ec4899"; "#3b82f6" |]
let mutable private artColorCursor = 0

let mutable private viewer: obj = null

// Pipeline caches: glyph shapes + blob survive mesh-only changes (heights,
// ring drag) so those stay cheap.
let mutable private glyphShapesCache: Shape list = []
let mutable private blobCache: Ring array = [||]
let mutable private basePositions: float array = [||]
let mutable private textPositions: float array = [||]
let private artPositions = System.Collections.Generic.Dictionary<int, float array>()

/// Curve flattening tolerance for glyph outlines, in mm.
let private glyphTol = 0.02

// ---------------------------------------------------------------------------
// Geometry pipeline
// ---------------------------------------------------------------------------

let private circleRing (c: Pt) (r: float) : Ring =
    [| for k in 0 .. 63 ->
         let th = 2.0 * Math.PI * float k / 64.0
         { X = c.X + r * cos th; Y = c.Y + r * sin th } |]

let private ringsBounds (rings: Ring array) : (float * float * float * float) option =
    let mutable minX = infinity
    let mutable minY = infinity
    let mutable maxX = -infinity
    let mutable maxY = -infinity
    let mutable any = false
    for r in rings do
        for p in r do
            any <- true
            if p.X < minX then minX <- p.X
            if p.Y < minY then minY <- p.Y
            if p.X > maxX then maxX <- p.X
            if p.Y > maxY then maxY <- p.Y
    if any then Some(minX, minY, maxX, maxY) else None

let private updateReadouts () =
    let sizeEl = byId "size-readout"
    let triEl = byId "tri-readout"
    if basePositions.Length = 0 then
        sizeEl.textContent <- "—"
        triEl.textContent <- "—"
    else
        // Bounds over the actual base mesh (includes the keyring).
        let mutable minX = infinity
        let mutable minY = infinity
        let mutable maxX = -infinity
        let mutable maxY = -infinity
        let mutable i = 0
        while i < basePositions.Length do
            let x = basePositions.[i]
            let y = basePositions.[i + 1]
            if x < minX then minX <- x
            if y < minY then minY <- y
            if x > maxX then maxX <- x
            if y > maxY then maxY <- y
            i <- i + 3
        sizeEl.textContent <-
            sprintf "%.1f × %.1f × %.1f mm" (maxX - minX) (maxY - minY) (baseH + textH)
        let artTotal = artPositions.Values |> Seq.sumBy (fun a -> a.Length)
        triEl.textContent <- string ((basePositions.Length + textPositions.Length + artTotal) / 9)

let private updateExportState () =
    let has = basePositions.Length > 0
    (inputById "export-btn").disabled <- not has
    (inputById "export-combined").disabled <- not has
    (inputById "export-separate").disabled <- not has
    (byId "viewer-hint")?style?display <- if has then "none" else ""

/// One instance's shapes scaled to its size and moved to its position.
/// Hole-fill threshold applies in final mm.
let private placedInst (inst: ArtInst) : Shape list =
    if inst.AShapes.IsEmpty || inst.NatH <= 0.0 then []
    else
        let s = inst.Size / inst.NatH
        inst.AShapes
        |> List.map (Geometry.mapShape (fun p -> { X = p.X * s + inst.Pos.X; Y = p.Y * s + inst.Pos.Y }))
        |> List.map (fun sh ->
            { sh with Holes = sh.Holes |> List.filter (fun h -> abs (Rings.signedArea h) >= holeFill) })

/// Rebuild meshes from the cached text shapes + placed artworks (heights,
/// ring, artwork placement, colors). The base blob hugs text AND artwork.
let private rebuildMeshes () =
    let placed = [ for id in artOrder -> id, placedInst arts.[id] ]
    let content = glyphShapesCache @ (placed |> List.collect snd)
    if content.IsEmpty then
        blobCache <- [||]
        basePositions <- [||]
        textPositions <- [||]
        artPositions.Clear ()
        artZones.Clear ()
        if not (isNull viewer) then
            Viewer.removeMesh viewer "base"
            Viewer.removeMesh viewer "text"
    else
        blobCache <-
            content
            |> List.map (fun s -> s.Outer)
            |> Array.ofList
            |> fun outers -> Clipper.offsetUnion outers border
        let rOuter = holeSize / 2.0 + ringThick
        ringCenterActual <-
            match ringPos with
            | Some p -> p
            | None ->
                match ringsBounds blobCache with
                | Some (minX, minY, _, maxY) -> { X = minX - rOuter * 0.35; Y = (minY + maxY) / 2.0 }
                | None -> { X = 0.0; Y = 0.0 }
        // Build the keyring as a solid annulus FIRST, then union it into the
        // blob: the hole only exists where the ring sticks out, so it can
        // never cut into the name. With the keyring off, the base is just
        // the blob — a plain nameplate.
        let basePaths =
            if not ringEnabled then blobCache
            else
                let annulus =
                    Clipper.combine
                        [| circleRing ringCenterActual rOuter |]
                        [| circleRing ringCenterActual (holeSize / 2.0) |]
                        "difference"
                Clipper.combine blobCache annulus "union"
        let baseShapes = Clipper.toShapes basePaths

        let basePos = ResizeArray<float>()
        for s in baseShapes do
            let p, _ = Geometry.extrude s baseH
            basePos.AddRange p
        basePositions <- basePos.ToArray()

        // Sink the raised solids slightly into the base: exactly-coincident
        // faces (bottom == base top) make slicers produce gaps and chewed
        // edges around the outlines; a real overlap slices cleanly.
        let extrudeRaised (shapes: Shape list) : float array =
            if textH < 0.05 || shapes.IsEmpty then [||]
            else
                let sink = min 0.2 (baseH / 2.0)
                let acc = ResizeArray<float>()
                for s in shapes do
                    let p, _ = Geometry.extrude s (textH + sink)
                    acc.AddRange (Geometry.translateZ (baseH - sink) p)
                acc.ToArray()
        textPositions <- extrudeRaised glyphShapesCache
        artPositions.Clear ()
        artZones.Clear ()
        for (id, shapes) in placed do
            artPositions.[id] <- extrudeRaised shapes
            // Drag zone: bounding circle of the placed instance.
            match Geometry.bounds shapes with
            | Some (minX, minY, maxX, maxY) ->
                let w = maxX - minX
                let h = maxY - minY
                artZones.[id] <- ({ X = (minX + maxX) / 2.0; Y = (minY + maxY) / 2.0 }, sqrt (w * w + h * h) / 2.0)
            | None -> ()

        if not (isNull viewer) then
            Viewer.setMesh viewer "base" basePositions baseColor
            if textPositions.Length > 0 then Viewer.setMesh viewer "text" textPositions textColor
            else Viewer.removeMesh viewer "text"
            for (id, _) in placed do
                let pos = artPositions.[id]
                if pos.Length > 0 then Viewer.setMesh viewer (sprintf "art-%d" id) pos arts.[id].Color
                else Viewer.removeMesh viewer (sprintf "art-%d" id)
    updateReadouts ()
    updateExportState ()

let mutable private firstBuild = true

/// Full rebuild: lay out the text, flatten glyphs, offset the base blob.
let private rebuildText () =
    match activeFont with
    | None -> ()
    | Some font ->
        let trimmed = text.Trim ()
        if trimmed = "" then
            glyphShapesCache <- []
            rebuildMeshes ()
        else
            let layout = TextShapes.layoutText font trimmed sizeMm
            let glyphArr: obj array = layout?glyphs
            let spaceAdv: float = layout?spaceAdv
            let glyphIns: TextShapes.GlyphIn list =
                glyphArr
                |> Array.toList
                |> List.mapi (fun i g ->
                    { GShapes = TextShapes.glyphShapes glyphTol holeFill (g?commands)
                      WordBreak = g?wordBreak
                      Adjust =
                        match letterAdjust.TryGetValue i with
                        | true, v -> v
                        | _ -> 0.0 })
            // Optical spacing: the slider is the true outline-to-outline gap,
            // independent of the font's (often unreliable) metrics. Then union
            // everything: script fonts build glyphs from overlapping strokes
            // and connected letters overlap each other — overlapping shells in
            // one STL solid make slicers misbehave, a single clean solid not.
            let raw =
                TextShapes.layoutOptical letterSpacing (spaceAdv * wordGap) glyphIns
                |> Clipper.unionShapes
            // Center on the origin and flip from font space (y-down) to y-up.
            let shapes =
                match Geometry.bounds raw with
                | Some (minX, minY, maxX, maxY) ->
                    let cx = (minX + maxX) / 2.0
                    let cy = (minY + maxY) / 2.0
                    raw |> List.map (Geometry.mapShape (fun p -> { X = p.X - cx; Y = cy - p.Y }))
                | None -> []
            glyphShapesCache <- shapes
            rebuildMeshes ()
            if firstBuild && basePositions.Length > 0 && not (isNull viewer) then
                firstBuild <- false
                Viewer.fitView viewer

// Debounced schedulers: text-affecting changes re-run the whole pipeline,
// mesh-only changes skip layout + clipper offset.
let mutable private textQueued = false
let mutable private meshQueued = false

let private scheduleText () =
    if not textQueued then
        textQueued <- true
        window.setTimeout ((fun () ->
            textQueued <- false
            rebuildText ()), 70)
        |> ignore

let private scheduleMeshes () =
    if not meshQueued then
        meshQueued <- true
        window.setTimeout ((fun () ->
            meshQueued <- false
            rebuildMeshes ()), 40)
        |> ignore

// ---------------------------------------------------------------------------
// Letter Specific Adjustments
// ---------------------------------------------------------------------------

/// Visible (non-whitespace) letters of the current text, in glyph order —
/// the same indexing the layout uses.
let private visibleLetters () =
    text.Trim().ToCharArray()
    |> Array.filter (fun c -> not (Char.IsWhiteSpace c))

let private syncLetterSlider () =
    let wrap = byId "letter-adjust-wrap"
    match activeLetter with
    | Some i ->
        wrap?style?display <- ""
        let letters = visibleLetters ()
        let name = if i < letters.Length then sprintf "[%c] #%d" letters.[i] (i + 1) else ""
        (byId "letter-adjust-name").textContent <- name
        let v =
            match letterAdjust.TryGetValue i with
            | true, x -> x
            | _ -> 0.0
        (inputById "letter-adjust").value <- string v
        (byId "letter-adjust-val").textContent <- sprintf "%+.1f mm" v
    | None -> wrap?style?display <- "none"

let private renderLetterButtons () =
    let box = byId "letter-btns"
    let letters = visibleLetters ()
    (byId "letters-empty")?style?display <- if letters.Length = 0 then "" else "none"
    box.innerHTML <-
        letters
        |> Array.mapi (fun i c ->
            let adjusted = letterAdjust.ContainsKey i && letterAdjust.[i] <> 0.0
            sprintf
                """<button type="button" class="letter-btn%s%s" data-i="%d">%c</button>"""
                (if activeLetter = Some i then " active" else "")
                (if adjusted then " adjusted" else "")
                i c)
        |> String.concat ""
    syncLetterSlider ()

// ---------------------------------------------------------------------------
// Artwork bank: upload, thumbnails, placement
// ---------------------------------------------------------------------------

[<Emit("encodeURIComponent($0)")>]
let private encodeURIComponentJs (s: string) : string = jsNative

[<Emit("(function(){ try { return localStorage.getItem($0); } catch { return null; } })()")>]
let private lsGet (key: string) : string = jsNative

[<Emit("(function(){ try { localStorage.setItem($0, $1); } catch {} })()")>]
let private lsSet (key: string) (v: string) : unit = jsNative

[<Emit("JSON.stringify($0)")>]
let private jsonStringify (o: obj) : string = jsNative

[<Emit("(function(){ try { return JSON.parse($0); } catch { return null; } })()")>]
let private jsonParse (s: string) : obj = jsNative

let private bankStorageKey = "3dsh-svg-bank"

/// Parse SVG markup into normalized shapes: flatten every drawable element
/// (transforms applied, holes classified), center at the origin, flip y-down
/// -> y-up, merge overlaps. Returns None when nothing fillable was found.
let private parseSvgShapes (svgText: string) : (Shape list * float * float) option =
    let holder = document.createElement "div"
    holder.setAttribute ("style", "position:fixed;left:-100000px;top:0;")
    holder.innerHTML <- svgText
    document.body.appendChild holder |> ignore
    let svg = holder.querySelector "svg"
    let result =
        if isNull svg then None
        else
            let scripts = svg.querySelectorAll "script"
            for k in 0 .. scripts.length - 1 do
                let s = scripts.[k] :?> Element
                s.parentElement.removeChild s |> ignore
            // Flattening tolerance relative to the artwork's own scale: after
            // scaling to keychain size the chord error stays under 0.05mm.
            let bb: obj = svg?getBBox ()
            let tol = max 1e-6 ((max (bb?width: float) (bb?height: float)) / 800.0)
            let shapes = ResizeArray<Shape>()
            let drawables = svg.querySelectorAll "path, rect, circle, ellipse, polygon, polyline"
            for k in 0 .. drawables.length - 1 do
                let el = drawables.[k] :?> Element
                if (el.closest "defs, clipPath, mask, symbol, pattern").IsNone then
                    let pe = SvgFlatten.parseElement svg tol (string k) "" el
                    for s in pe.Shapes do
                        shapes.Add s
            match Geometry.bounds shapes with
            | None -> None
            | Some (minX, minY, maxX, maxY) ->
                let cx = (minX + maxX) / 2.0
                let cy = (minY + maxY) / 2.0
                let normalized =
                    shapes
                    |> List.ofSeq
                    |> List.map (Geometry.mapShape (fun p -> { X = p.X - cx; Y = cy - p.Y }))
                    |> Clipper.unionShapes
                Some (normalized, maxX - minX, maxY - minY)
    document.body.removeChild holder |> ignore
    result

let private renderBank () =
    let box = byId "art-bank"
    (byId "bank-empty")?style?display <- if bank.Count = 0 then "" else "none"
    box.innerHTML <-
        bank
        |> Seq.mapi (fun i item ->
            sprintf
                """<div class="bank-item" data-i="%d" title="%s — click to add"><img src="data:image/svg+xml,%s" alt="%s" /><button type="button" class="bank-x" title="Remove from bank">×</button></div>"""
                i item.Name (encodeURIComponentJs item.Svg) item.Name)
        |> String.concat ""

let private saveBank () =
    let items = bank |> Seq.map (fun b -> createObj [ "name" ==> b.Name; "svg" ==> b.Svg ]) |> Array.ofSeq
    lsSet bankStorageKey (jsonStringify items)

let private renderArtList () =
    let ul = byId "art-list"
    (byId "art-placed-wrap")?style?display <- if artOrder.Count = 0 then "none" else ""
    ul.innerHTML <-
        artOrder
        |> Seq.map (fun id ->
            let inst = arts.[id]
            sprintf
                """<li data-id="%d" class="%s"><span class="swatch" style="background:%s"></span><span class="l-name">%s</span><span class="l-h">%.1f mm</span><button class="l-x" title="Remove from keychain">×</button></li>"""
                id (if activeArt = Some id then "active" else "") inst.Color inst.AName inst.Size)
        |> String.concat ""

/// Point the size slider + color input at the active instance.
let private syncArtEditor () =
    match activeArt with
    | Some id when arts.ContainsKey id ->
        (inputById "art-size").value <- string arts.[id].Size
        (byId "art-size-val").textContent <- sprintf "%.1f mm" arts.[id].Size
        (inputById "art-color").value <- arts.[id].Color
    | _ -> ()

let private setActiveArt (id: int option) =
    activeArt <- id
    renderArtList ()
    syncArtEditor ()

let private removeInstance (id: int) =
    arts.Remove id |> ignore
    artOrder.Remove id |> ignore
    artPositions.Remove id |> ignore
    artZones.Remove id |> ignore
    if not (isNull viewer) then Viewer.removeMesh viewer (sprintf "art-%d" id)
    if activeArt = Some id then
        activeArt <- if artOrder.Count > 0 then Some artOrder.[artOrder.Count - 1] else None
    rebuildMeshes ()
    renderArtList ()
    syncArtEditor ()

let private addFromBank (item: BankItem) =
    artCounter <- artCounter + 1
    let id = artCounter
    // Park right of everything currently on the keychain.
    let existing = glyphShapesCache @ (artOrder |> Seq.collect (fun i -> placedInst arts.[i]) |> List.ofSeq)
    let halfW = (item.NatW * (artDefaultSize / max 1e-9 item.NatH)) / 2.0
    let pos =
        match Geometry.bounds existing with
        | Some (_, _, maxX, _) -> { X = maxX + border + halfW + 1.0; Y = 0.0 }
        | None -> { X = 0.0; Y = 0.0 }
    let inst =
        { AShapes = item.BShapes
          NatW = item.NatW
          NatH = item.NatH
          AName = item.Name
          Size = artDefaultSize
          Color = artColors.[artColorCursor % artColors.Length]
          Pos = pos }
    artColorCursor <- artColorCursor + 1
    arts.[id] <- inst
    artOrder.Add id
    // Every instance is its own drag target; dead ids just return null.
    if not (isNull viewer) then
        Viewer.registerDrag
            viewer
            (fun () ->
                match artZones.TryGetValue id with
                | true, (c, r) when arts.ContainsKey id -> createObj [ "X" ==> c.X; "Y" ==> c.Y; "R" ==> r ]
                | _ -> null)
            (fun p ->
                match arts.TryGetValue id with
                | true, inst ->
                    inst.Pos <- { X = p?x; Y = p?y }
                    if activeArt <> Some id then setActiveArt (Some id)
                    scheduleMeshes ()
                | _ -> ())
    setActiveArt (Some id)
    rebuildMeshes ()
    if not (isNull viewer) then Viewer.fitView viewer

let private addToBank (name: string) (svgText: string) =
    match parseSvgShapes svgText with
    | None -> window.alert (sprintf "No fillable shapes found in %s." name)
    | Some (shapes, w, h) ->
        bank.Add { Name = name; Svg = svgText; BShapes = shapes; NatW = w; NatH = h }
        renderBank ()
        saveBank ()

let private loadBankFromStorage () =
    let raw = lsGet bankStorageKey
    if not (isNull raw) then
        let items: obj = jsonParse raw
        if not (isNull items) then
            let n: int = items?length
            for k in 0 .. n - 1 do
                let it: obj = items?(k)
                let name: string = it?name
                let svg: string = it?svg
                if not (isNull (box svg)) then
                    match parseSvgShapes svg with
                    | Some (shapes, w, h) ->
                        bank.Add { Name = name; Svg = svg; BShapes = shapes; NatW = w; NatH = h }
                    | None -> ()
            renderBank ()

// ---------------------------------------------------------------------------
// Export
// ---------------------------------------------------------------------------

let private safeName () =
    let sb = System.Text.StringBuilder ()
    for c in text.Trim () do
        sb.Append (if "\\/:*?\"<>|".Contains (string c) then '-' else c) |> ignore
    let s = sb.ToString ()
    if s = "" then "keychain" else s

let private saveStl (filename: string) (buf: obj) =
    let note = byId "export-note"
    if isNull folderHandle then
        Stl.download filename buf
        note.textContent <- sprintf "Saved %s (download)" filename
    else
        thenDo (saveToFolder folderHandle filename buf) (fun _ ->
            note.textContent <- sprintf "Saved %s to folder" filename)

let private artParts () =
    [ for id in artOrder do
        match artPositions.TryGetValue id with
        | true, p when p.Length > 0 -> p
        | _ -> () ]

let private exportCombined () =
    if basePositions.Length > 0 then
        saveStl (safeName () + ".stl") (Stl.build (basePositions :: textPositions :: artParts ()))

let private exportSeparate () =
    if basePositions.Length > 0 then
        saveStl (safeName () + "-base.stl") (Stl.build [ basePositions ])
        if textPositions.Length > 0 then
            saveStl (safeName () + "-text.stl") (Stl.build [ textPositions ])
        let parts = artParts ()
        parts
        |> List.iteri (fun i p ->
            let suffix = if parts.Length = 1 then "-art" else sprintf "-art%d" (i + 1)
            saveStl (safeName () + suffix + ".stl") (Stl.build [ p ]))

// ---------------------------------------------------------------------------
// UI wiring
// ---------------------------------------------------------------------------

let private renderFontSelect () =
    let sel = byId "font-select" :?> HTMLSelectElement
    sel.innerHTML <-
        fonts
        |> Seq.mapi (fun i (name, f) ->
            let selected = (activeFont = Some f)
            sprintf "<option value=\"%d\"%s>%s</option>" i (if selected then " selected" else "") name)
        |> String.concat ""

let private addFont (name: string) (font: obj) =
    fonts.Add (name, font)
    activeFont <- Some font
    renderFontSelect ()
    scheduleText ()

let private bindSlider (id: string) (fmt: float -> string) (apply: float -> unit) =
    let inp = inputById id
    let valEl = byId (id + "-val")
    let update () =
        let v = parseFloatJs inp.value
        if not (Double.IsNaN v) then
            valEl.textContent <- fmt v
            apply v
    inp.addEventListener ("input", fun _ -> update ())
    update ()

let private defaults = [
    "size", "20"; "border", "3"; "letter-spacing", "0"; "word-gap", "1"
    "hole-fill", "2"; "hole-size", "5"; "ring-thick", "3"; "base-h", "3"; "text-h", "2"
    "art-size", "20" ]

let private init () =
    viewer <- Viewer.createViewer (byId "viewer-box")

    // Keyring dragging: report the current ring so the viewer knows the grab
    // zone; drags override the automatic position.
    Viewer.registerDrag
        viewer
        (fun () ->
            if blobCache.Length = 0 || not ringEnabled then null
            else createObj [ "X" ==> ringCenterActual.X; "Y" ==> ringCenterActual.Y; "R" ==> (holeSize / 2.0 + ringThick) ])
        (fun p ->
            ringPos <- Some { X = p?x; Y = p?y }
            scheduleMeshes ())
    // (Artwork instances register their own drag targets when placed.)

    // Font upload + selection.
    let fontInput = inputById "font-files"
    (byId "font-btn").addEventListener ("click", fun _ -> fontInput.click ())
    fontInput.addEventListener (
        "change",
        fun _ ->
            let files: obj = fontInput?files
            let n: int = files?length
            for k in 0 .. n - 1 do
                let file: obj = files?item (k)
                let name: string = !!(file?name)
                readFileBuffer file (fun buf ->
                    try
                        let font = TextShapes.parseFontBuffer buf
                        addFont (TextShapes.fontName font) font
                    with _ ->
                        window.alert (sprintf "Could not read %s as a font (TTF/OTF/WOFF)." name))
            fontInput.value <- ""
    )
    (byId "font-select").addEventListener (
        "change",
        fun _ ->
            let i = int (parseFloatJs (inputById "font-select").value)
            if i >= 0 && i < fonts.Count then
                activeFont <- Some (snd fonts.[i])
                scheduleText ()
    )

    // Save folder (File System Access API; hidden when unsupported).
    if canPickFolder () then
        (byId "folder-btn").addEventListener (
            "click",
            fun _ ->
                thenDo (pickFolder ()) (fun handle ->
                    if not (isNull handle) then
                        folderHandle <- handle
                        (byId "folder-name").textContent <- sprintf "Saving into: %s" (!!(handle?name): string))
        )
    else
        (byId "folder-btn")?style?display <- "none"
        (byId "folder-label")?style?display <- "none"
        (byId "folder-name").textContent <- "Save folder needs Chrome/Edge — exports download normally"

    // Keychain text. Editing the text invalidates letter indices, so
    // per-letter adjustments reset with it.
    let textInput = inputById "text-input"
    let updateText () =
        text <- textInput.value
        (byId "char-count").textContent <- sprintf "%d/15 characters" text.Length
        letterAdjust.Clear ()
        activeLetter <- None
        renderLetterButtons ()
        scheduleText ()
    textInput.addEventListener ("input", fun _ -> updateText ())

    // Letter Specific Adjustments: click a letter, shift it and everything
    // after it left/right.
    (byId "letter-btns").addEventListener (
        "click",
        fun ev ->
            let target = ev.target :?> Element
            match target.closest ".letter-btn" with
            | Some btn ->
                activeLetter <- Some (int (parseFloatJs (btn.getAttribute "data-i")))
                renderLetterButtons ()
            | None -> ()
    )
    let letterSlider = inputById "letter-adjust"
    letterSlider.addEventListener (
        "input",
        fun _ ->
            match activeLetter with
            | Some i ->
                let v = parseFloatJs letterSlider.value
                if not (Double.IsNaN v) then
                    if v = 0.0 then letterAdjust.Remove i |> ignore else letterAdjust.[i] <- v
                    (byId "letter-adjust-val").textContent <- sprintf "%+.1f mm" v
                    renderLetterButtons ()
                    scheduleText ()
            | None -> ()
    )
    (byId "letter-adjust-clear").addEventListener (
        "click",
        fun _ ->
            letterAdjust.Clear ()
            renderLetterButtons ()
            scheduleText ()
    )

    // Colors (mesh-only refresh).
    (inputById "base-color").addEventListener (
        "input",
        fun _ ->
            baseColor <- (inputById "base-color").value
            if not (isNull viewer) then Viewer.setColor viewer "base" baseColor
    )
    (inputById "text-color").addEventListener (
        "input",
        fun _ ->
            textColor <- (inputById "text-color").value
            if not (isNull viewer) then Viewer.setColor viewer "text" textColor
    )

    // Artwork bank: upload (multiple), click a thumbnail to place, × removes.
    let artInput = inputById "art-file"
    (byId "art-btn").addEventListener ("click", fun _ -> artInput.click ())
    artInput.addEventListener (
        "change",
        fun _ ->
            let files: obj = artInput?files
            let n: int = files?length
            for k in 0 .. n - 1 do
                let file: obj = files?item (k)
                let name: string = !!(file?name)
                if name.ToLower().EndsWith ".svg" then
                    readFileText file (fun t -> addToBank name t)
                else
                    window.alert (sprintf "%s is not an .svg file." name)
            artInput.value <- ""
    )
    (byId "art-bank").addEventListener (
        "click",
        fun ev ->
            let target = ev.target :?> Element
            match target.closest ".bank-item" with
            | Some item ->
                let i = int (parseFloatJs (item.getAttribute "data-i"))
                if i >= 0 && i < bank.Count then
                    if target.classList.contains "bank-x" then
                        bank.RemoveAt i
                        renderBank ()
                        saveBank ()
                    else
                        addFromBank bank.[i]
            | None -> ()
    )
    // Placed-artwork list: click a row to edit it, × removes it.
    (byId "art-list").addEventListener (
        "click",
        fun ev ->
            let target = ev.target :?> Element
            match target.closest "[data-id]" with
            | Some row ->
                let id = int (parseFloatJs (row.getAttribute "data-id"))
                if target.classList.contains "l-x" then removeInstance id
                else setActiveArt (Some id)
            | None -> ()
    )
    (inputById "art-color").addEventListener (
        "input",
        fun _ ->
            match activeArt with
            | Some id when arts.ContainsKey id ->
                arts.[id].Color <- (inputById "art-color").value
                if not (isNull viewer) then Viewer.setColor viewer (sprintf "art-%d" id) arts.[id].Color
                renderArtList ()
            | _ -> ()
    )

    // Keyring on/off: off = plain nameplate.
    let ringCheck = inputById "ring-enabled"
    ringCheck.addEventListener (
        "change",
        fun _ ->
            ringEnabled <- ringCheck.``checked``
            (byId "ring-controls").classList.toggle ("off", not ringEnabled) |> ignore
            scheduleMeshes ()
    )

    // Sliders. Text-affecting ones re-run layout; the rest only re-mesh.
    let mm v = sprintf "%.1f mm" v
    bindSlider "size" mm (fun v -> sizeMm <- v; scheduleText ())
    bindSlider "border" mm (fun v -> border <- v; scheduleText ())
    bindSlider "letter-spacing" mm (fun v -> letterSpacing <- v; scheduleText ())
    bindSlider "word-gap" (sprintf "%.1f×") (fun v -> wordGap <- v; scheduleText ())
    bindSlider "hole-fill" (fun v -> sprintf "%.1f mm²" v) (fun v -> holeFill <- v; scheduleText ())
    bindSlider "hole-size" mm (fun v -> holeSize <- v; scheduleMeshes ())
    bindSlider "ring-thick" mm (fun v -> ringThick <- v; scheduleMeshes ())
    bindSlider "base-h" mm (fun v -> baseH <- v; scheduleMeshes ())
    bindSlider "text-h" mm (fun v -> textH <- v; scheduleMeshes ())
    bindSlider "art-size" mm (fun v ->
        match activeArt with
        | Some id when arts.ContainsKey id ->
            arts.[id].Size <- v
            renderArtList ()
            scheduleMeshes ()
        | _ -> ())

    // Reset: restore defaults (fonts, folder and text survive).
    (byId "reset-btn").addEventListener (
        "click",
        fun _ ->
            for (id, v) in defaults do
                (inputById id).value <- v
            ringPos <- None
            letterAdjust.Clear ()
            activeLetter <- None
            renderLetterButtons ()
            // Clear placed artworks (the bank itself survives).
            for id in artOrder |> Seq.toArray do
                arts.Remove id |> ignore
                artPositions.Remove id |> ignore
                artZones.Remove id |> ignore
                if not (isNull viewer) then Viewer.removeMesh viewer (sprintf "art-%d" id)
            artOrder.Clear ()
            activeArt <- None
            renderArtList ()
            ringEnabled <- true
            (inputById "ring-enabled").``checked`` <- true
            (byId "ring-controls").classList.remove "off"
            sizeMm <- 20.0
            border <- 3.0
            letterSpacing <- 0.0
            wordGap <- 1.0
            holeFill <- 2.0
            holeSize <- 5.0
            ringThick <- 3.0
            baseH <- 3.0
            textH <- 2.0
            for (id, _) in defaults do
                let valEl = byId (id + "-val")
                let v = parseFloatJs (inputById id).value
                valEl.textContent <-
                    match id with
                    | "word-gap" -> sprintf "%.1f×" v
                    | "hole-fill" -> sprintf "%.1f mm²" v
                    | _ -> sprintf "%.1f mm" v
            scheduleText ()
    )

    (byId "fit-btn").addEventListener ("click", fun _ -> if not (isNull viewer) then Viewer.fitView viewer)
    (byId "export-btn").addEventListener ("click", fun _ -> exportCombined ())
    (byId "export-combined").addEventListener ("click", fun _ -> exportCombined ())
    (byId "export-separate").addEventListener ("click", fun _ -> exportSeparate ())

    (byId "char-count").textContent <- sprintf "%d/15 characters" text.Length

    // Test hooks: deterministic keyring/artwork placement for the e2e suite.
    window?__setRing <- (fun (p: obj) ->
        ringPos <- Some { X = p?x; Y = p?y }
        rebuildMeshes ())
    window?__setArt <- (fun (p: obj) ->
        match activeArt with
        | Some id when arts.ContainsKey id ->
            arts.[id].Pos <- { X = p?x; Y = p?y }
            rebuildMeshes ()
        | _ -> ())

    loadBankFromStorage ()
    renderArtList ()
    renderLetterButtons ()

    // Bundled default font so the app works with zero setup.
    thenDo (TextShapes.loadDefaultFont ()) (fun font ->
        addFont "Baloo 2 (built-in)" font)

init ()
