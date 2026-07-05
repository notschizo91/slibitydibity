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
/// User-dragged keyring position; None = automatic (left of the blob).
let mutable private ringPos: Pt option = None
let mutable private ringCenterActual = { X = 0.0; Y = 0.0 }
let mutable private folderHandle: obj = null

// Imported SVG artwork, normalized: centered at its own origin, y-up, at the
// SVG's natural scale (scaled to artSize mm at placement time).
let mutable private artShapes: Shape list = []
let mutable private artNaturalW = 0.0
let mutable private artNaturalH = 0.0
let mutable private artSize = 20.0
let mutable private artColor = "#ef4444"
/// User-dragged artwork position; None = automatic (right of the text).
let mutable private artPos: Pt option = None
let mutable private artCenterActual = { X = 0.0; Y = 0.0 }
let mutable private artRadiusActual = 0.0

let mutable private viewer: obj = null

// Pipeline caches: glyph shapes + blob survive mesh-only changes (heights,
// ring drag) so those stay cheap.
let mutable private glyphShapesCache: Shape list = []
let mutable private blobCache: Ring array = [||]
let mutable private basePositions: float array = [||]
let mutable private textPositions: float array = [||]
let mutable private artPositions: float array = [||]

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
        triEl.textContent <- string ((basePositions.Length + textPositions.Length + artPositions.Length) / 9)

let private updateExportState () =
    let has = basePositions.Length > 0
    (inputById "export-btn").disabled <- not has
    (inputById "export-combined").disabled <- not has
    (inputById "export-separate").disabled <- not has
    (byId "viewer-hint")?style?display <- if has then "none" else ""

/// Artwork shapes scaled to artSize and moved into place (user-dragged or
/// auto-parked right of the text). Hole-fill threshold applies in final mm.
let private placedArt () : Shape list =
    if artShapes.IsEmpty || artNaturalH <= 0.0 then []
    else
        let s = artSize / artNaturalH
        let pos =
            match artPos with
            | Some p -> p
            | None ->
                match Geometry.bounds glyphShapesCache with
                | Some (_, _, maxX, _) -> { X = maxX + border + (artNaturalW * s) / 2.0 + 1.0; Y = 0.0 }
                | None -> { X = 0.0; Y = 0.0 }
        artShapes
        |> List.map (Geometry.mapShape (fun p -> { X = p.X * s + pos.X; Y = p.Y * s + pos.Y }))
        |> List.map (fun sh ->
            { sh with Holes = sh.Holes |> List.filter (fun h -> abs (Rings.signedArea h) >= holeFill) })

/// Rebuild meshes from the cached text shapes + artwork (heights, ring,
/// artwork placement, colors). The base blob hugs text AND artwork.
let private rebuildMeshes () =
    let art = placedArt ()
    let content = glyphShapesCache @ art
    if content.IsEmpty then
        blobCache <- [||]
        basePositions <- [||]
        textPositions <- [||]
        artPositions <- [||]
        artRadiusActual <- 0.0
        if not (isNull viewer) then
            Viewer.removeMesh viewer "base"
            Viewer.removeMesh viewer "text"
            Viewer.removeMesh viewer "art"
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
        artPositions <- extrudeRaised art

        // Artwork drag zone: bounding circle of the placed artwork.
        match Geometry.bounds art with
        | Some (minX, minY, maxX, maxY) ->
            artCenterActual <- { X = (minX + maxX) / 2.0; Y = (minY + maxY) / 2.0 }
            let w = maxX - minX
            let h = maxY - minY
            artRadiusActual <- sqrt (w * w + h * h) / 2.0
        | None -> artRadiusActual <- 0.0

        if not (isNull viewer) then
            Viewer.setMesh viewer "base" basePositions baseColor
            if textPositions.Length > 0 then Viewer.setMesh viewer "text" textPositions textColor
            else Viewer.removeMesh viewer "text"
            if artPositions.Length > 0 then Viewer.setMesh viewer "art" artPositions artColor
            else Viewer.removeMesh viewer "art"
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
                |> List.map (fun g ->
                    { GShapes = TextShapes.glyphShapes glyphTol holeFill (g?commands)
                      WordBreak = g?wordBreak })
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
// Artwork SVG import
// ---------------------------------------------------------------------------

/// Parse an uploaded SVG into artwork shapes: flatten every drawable element
/// (transforms applied, holes classified), center at the origin, flip y-down
/// -> y-up, and merge overlaps into clean solids.
let private loadArtSvg (name: string) (svgText: string) =
    let holder = document.createElement "div"
    holder.setAttribute ("style", "position:fixed;left:-100000px;top:0;")
    holder.innerHTML <- svgText
    document.body.appendChild holder |> ignore
    let cleanup () = document.body.removeChild holder |> ignore
    let svg = holder.querySelector "svg"
    if isNull svg then
        cleanup ()
        window.alert "That file doesn't contain a valid <svg> element."
    else
        let scripts = svg.querySelectorAll "script"
        for k in 0 .. scripts.length - 1 do
            let s = scripts.[k] :?> Element
            s.parentElement.removeChild s |> ignore
        // Flattening tolerance relative to the artwork's own scale: after
        // scaling to keychain size the chord error stays well under 0.05mm.
        let bb: obj = svg?getBBox ()
        let tol = max 1e-6 ((max (bb?width: float) (bb?height: float)) / 800.0)
        let shapes = ResizeArray<Shape>()
        let mutable openCount = 0
        let drawables = svg.querySelectorAll "path, rect, circle, ellipse, polygon, polyline"
        for k in 0 .. drawables.length - 1 do
            let el = drawables.[k] :?> Element
            if (el.closest "defs, clipPath, mask, symbol, pattern").IsNone then
                let pe = SvgFlatten.parseElement svg tol (string k) "" el
                openCount <- openCount + pe.OpenSubpaths
                for s in pe.Shapes do
                    shapes.Add s
        cleanup ()
        match Geometry.bounds shapes with
        | None -> window.alert "No fillable shapes found in that SVG."
        | Some (minX, minY, maxX, maxY) ->
            let cx = (minX + maxX) / 2.0
            let cy = (minY + maxY) / 2.0
            artShapes <-
                shapes
                |> List.ofSeq
                |> List.map (Geometry.mapShape (fun p -> { X = p.X - cx; Y = cy - p.Y }))
                |> Clipper.unionShapes
            artNaturalW <- maxX - minX
            artNaturalH <- maxY - minY
            artPos <- None
            (byId "art-name").textContent <-
                name + (if openCount > 0 then sprintf " · ⚠ %d open subpath(s) auto-closed" openCount else "")
            (byId "art-remove")?style?display <- ""
            rebuildMeshes ()
            if not (isNull viewer) then Viewer.fitView viewer

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

let private exportCombined () =
    if basePositions.Length > 0 then
        saveStl (safeName () + ".stl") (Stl.build [ basePositions; textPositions; artPositions ])

let private exportSeparate () =
    if basePositions.Length > 0 then
        saveStl (safeName () + "-base.stl") (Stl.build [ basePositions ])
        if textPositions.Length > 0 then
            saveStl (safeName () + "-text.stl") (Stl.build [ textPositions ])
        if artPositions.Length > 0 then
            saveStl (safeName () + "-art.stl") (Stl.build [ artPositions ])

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
    Viewer.registerDrag
        viewer
        (fun () ->
            if artRadiusActual <= 0.0 then null
            else createObj [ "X" ==> artCenterActual.X; "Y" ==> artCenterActual.Y; "R" ==> artRadiusActual ])
        (fun p ->
            artPos <- Some { X = p?x; Y = p?y }
            scheduleMeshes ())

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

    // Keychain text.
    let textInput = inputById "text-input"
    let updateText () =
        text <- textInput.value
        (byId "char-count").textContent <- sprintf "%d/15 characters" text.Length
        scheduleText ()
    textInput.addEventListener ("input", fun _ -> updateText ())

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

    // Artwork SVG upload / remove / color.
    let artInput = inputById "art-file"
    (byId "art-btn").addEventListener ("click", fun _ -> artInput.click ())
    artInput.addEventListener (
        "change",
        fun _ ->
            let files: obj = artInput?files
            let file: obj = files?item (0)
            if not (isNull file) then
                let name: string = !!(file?name)
                if name.ToLower().EndsWith ".svg" then
                    readFileText file (fun t -> loadArtSvg name t)
                else
                    window.alert "Please choose an .svg file."
            artInput.value <- ""
    )
    (byId "art-remove").addEventListener (
        "click",
        fun _ ->
            artShapes <- []
            artPos <- None
            artRadiusActual <- 0.0
            (byId "art-name").textContent <- "No artwork loaded"
            (byId "art-remove")?style?display <- "none"
            scheduleMeshes ()
    )
    (inputById "art-color").addEventListener (
        "input",
        fun _ ->
            artColor <- (inputById "art-color").value
            if not (isNull viewer) then Viewer.setColor viewer "art" artColor
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
    bindSlider "art-size" mm (fun v -> artSize <- v; scheduleMeshes ())

    // Reset: restore defaults (fonts, folder and text survive).
    (byId "reset-btn").addEventListener (
        "click",
        fun _ ->
            for (id, v) in defaults do
                (inputById id).value <- v
            ringPos <- None
            artPos <- None
            artSize <- 20.0
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
        artPos <- Some { X = p?x; Y = p?y }
        rebuildMeshes ())

    // Bundled default font so the app works with zero setup.
    thenDo (TextShapes.loadDefaultFont ()) (fun font ->
        addFont "Baloo 2 (built-in)" font)

init ()
