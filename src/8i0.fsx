// Part of the ImgUtil module.
module ImgUtil

// Difine types point and color.
type point = int * int
type color = ImgUtil.color

// Difine type figure.
type figure =
    | Circle of point * int * color
        // defined by center, radius, and color
    | Rectangle of point * point * color
        // defined by corners top -left, bottom -right, and color
    | Mix of figure * figure
        // combine figures with mixed color at overlap


///<summary>Finds color at specified location.</summary>
///<param name = "x, y">Input coordinate tuple.</param>
///<param name = "figure">Input figure.</param>
let rec colorAt (x, y) figure =
    match figure with
    | Circle ((cx, cy), r, col) ->
        if (x - cx) * (x - cx) + (y - cy) * (y - cy) <= r * r
            // Uses Pythagoras's equation to determine distance to center
        then Some col else None
    | Rectangle ((x0, y0), (x1, y1), col) ->
        if x0 <= x && x <= x1 && y0 <= y && y <= y1
            // Within corners
        then Some col else None
    | Mix (f1, f2) ->
        match (colorAt (x, y) f1, colorAt (x, y) f2) with
        | (None, c) -> c // No overlap
        | (c, None) -> c // No overlap
        | (Some c1, Some c2) ->
        let (a1, r1, g1, b1) = ImgUtil.fromColor c1
        let (a2, r2, g2, b2) = ImgUtil.fromColor c2
        in Some (ImgUtil.fromArgb ((a1 + a2) / 2, (r1 + r2) / 2, // Calculate
                    (g1 + g2) / 2, (b1 + b2) / 2)) // Average color


// Figure figTest
let figTest : figure =
    Mix (Circle ((50, 50), 45, (ImgUtil.red)),
                Rectangle ((40, 40), (90, 110), (ImgUtil.blue)))


///<summary>Paints input figure on canvas and creates a .png file.</summary>
///<param name = "name">Name to be given to the output image.</param>
///<param name = "fig">Input figure to paint on canvas.</param>
///<param name = "w">Width of the created image.</param>
///<param name = "h">Height of the created image.</param>
let makePicture (name:string) (fig:figure) (w:int) (h:int) =
    let (canvas:ImgUtil.canvas) =
        ImgUtil.init w h (fun(x, y) ->
            let (c:color) =
                match colorAt(x, y) fig with
                | None -> ImgUtil.fromRgb(128, 128, 128)
                | Some c -> c
            c)
    do ImgUtil.toPngFile name canvas


makePicture "figTest.png" figTest 100 150


///<summary>Tests an inputted figure to see if it's a valid figure.</summary>
///<param name = "fig">The input figure to test.</param>
///<returns>A bool value for if the input figure is correct.<returns>
let rec checkFigure (fig:figure) : bool =
    match fig with
    | Circle ((x, y), r, c) -> r >= 0
    | Rectangle ((x0, y0), (x1, y1), c) -> (x0 <= x1) && (y0 <= y1)
    | Mix (fig1, fig2) -> checkFigure(fig1) && checkFigure(fig2)


///<summary>Moves a given figure to a given location.</summary>
///<param name = "fig">The input figure to move.</param>
///<param name = "vecx">x value of the vector to move the figure along.</param>
///<param name = "vecy">y value of the vector to move the figure along.</param>
///<returns>A moved figure.<returns>
let rec move (fig:figure) (vecx:int, vecy:int) : figure =
    match fig with
        | Circle ((x, y), r, c) -> Circle ((x + vecx, y + vecy), r, c)
        | Rectangle ((x0, y0), (x1, y1), c) ->
                Rectangle ((x0 + vecx, y0 + vecy), (x1 + vecx, y1 + vecy), c)
        | Mix ((fig1), (fig2)) ->
                Mix ((move fig1 (vecx, vecy), (move fig2 (vecx, vecy))))

makePicture "moveTest.png" (move figTest (-20,20)) 100 150


///<summary>Specifies the min and max bounds of a input figure.</summary>
///<param name = "fig">The input figure to find bounds of.</param>
///<returns>A tuple of points for the max and min values of a figure.<returns>
let rec boundingBox (fig:figure) : point * point =
    match fig with
        | Circle ((x, y), r, c) -> ((x - r, y - r), (x + r, y + r))
        | Rectangle ((x0, y0), (x1, y1), c) -> ((x0, y0), (x1, y1))
        | Mix (fig1, fig2) -> // Solution for the body of this statement was,
                              // in part, found, fixed, and used from here;
                              // https://stackoverflow.com/questions/53484427/
                              // type-error-f-the-type-int-does-not-match?fbclid
                              // =IwAR0c4fAi6IcYR81UnuvVZFjvAgCejIOV8arOYaaTAui
                              // xFMawR9SAcQ2q7lc
            let (a1, b1) = boundingBox fig1
            let (a2, b2) = boundingBox fig2
            let bb1 = min (fst a1) (fst a2), min (snd a1) (snd a2)
            let bb2 = max (fst b1) (fst b2), max (snd b1) (snd b2)
            (bb1, bb2)

printfn "%A" (boundingBox figTest)