open System
open System.Drawing
open System.Text
open Argu
open OpenCvSharp

type CliArguments =
    | [<Mandatory>] Input of path: string

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "Input file"

let errorHandler =
    ProcessExiter(
        colorizer =
            function
            | ErrorCode.HelpText -> None
            | _ -> Some ConsoleColor.Red
    )

let parser = ArgumentParser.Create<CliArguments>(errorHandler = errorHandler)
let argv = Environment.GetCommandLineArgs() |> Array.skip 1
let result = parser.ParseCommandLine argv

let rgbText (r, g, b, text) = $"\x1b[38;2;{r};{g};{b}m{text}"

let colorText (color: Color, text) =
    rgbText (color.R, color.G, color.B, text)

let pixelToAnsi (pixel: Vec3b) =
    let b, g, r = (pixel.Item0, pixel.Item1, pixel.Item2)
    rgbText (r, g, b, "██")

let renderFrameToText (image: Mat) =
    if image.Empty() then
        [||]
    else
        let width = image.Width
        let height = image.Height

        let pixelCoords =
            [ for y in 0 .. height - 1 do
                  for x in 0 .. width - 1 -> (x, y) ]

        let mutable outputs =
            [| 0 .. (height - 1) |] |> Array.map (fun _ -> StringBuilder())

        pixelCoords
        |> List.iter (fun (x, y) ->
            let pixel = image.Get<Vec3b>(y, x)
            let text = pixelToAnsi pixel
            outputs[y].Append(text) |> ignore)

        outputs |> Array.map (_.ToString())

let getNewSize (originalSize: int * int) =
    let consoleWidth = Console.BufferWidth / 2
    let consoleHeight = Console.BufferHeight - 1

    let frameWidth, frameHeight = originalSize

    let widthRatio = float consoleWidth / float frameWidth
    let heightRatio = float consoleHeight / float frameHeight

    let ratio = Math.Min(widthRatio, heightRatio)

    let newWidth = int (float frameWidth * ratio)
    let newHeight = int (float frameHeight * ratio)

    newWidth, newHeight



let resizeFrameByConsoleSize (frame: Mat, resizedFrame: Mat) =
    let newWidth, newHeight = getNewSize (frame.Width, frame.Height)
    let newSize = Size(newWidth, newHeight)
    Cv2.Resize(frame, resizedFrame, newSize)

let mutable lastBufferSize = (0, 0)

let rec renderVideo (capture: VideoCapture) =
    let nextFrameTime = DateTime.Now.AddSeconds(1.0 / capture.Fps)
    let image = new Mat()
    let resizedImage = new Mat()

    if capture.Read(image) then
        resizeFrameByConsoleSize (image, resizedImage)
        let mutable output = StringBuilder()

        resizedImage
        |> renderFrameToText
        |> Array.iteri (fun i line ->
            (if i = Console.BufferHeight - 1 || line.Length = Console.BufferWidth then
                 output <- output.Append(line)
             else
                 output <- output.AppendLine(line)))

        if lastBufferSize <> (Console.BufferWidth, Console.BufferHeight) then
            Console.Clear()
            lastBufferSize <- (Console.BufferWidth, Console.BufferHeight)

        Console.SetCursorPosition(0, 0)
        Console.Write(output.ToString())
        Console.SetCursorPosition(0, Console.BufferHeight - 1)
        let totalTime = TimeSpan.FromSeconds(float capture.FrameCount / capture.Fps)
        let currentTime = TimeSpan.FromSeconds(float capture.PosFrames / capture.Fps)

        Console.Write(
            colorText (
                Color.White,
                $"""{currentTime.Minutes.ToString("D2")}:{currentTime.Seconds.ToString("D2")}/{totalTime.Minutes.ToString("D2")}:{totalTime.Seconds.ToString("D2")}"""
            )
        )

        image.Dispose()
        resizedImage.Dispose()
        let delay = nextFrameTime - DateTime.Now

        if delay > TimeSpan.Zero then
            System.Threading.Thread.Sleep(delay)

        renderVideo capture
    else
        0

let inputPath = result.GetResult Input
let capture = new VideoCapture(inputPath)

renderVideo capture |> ignore

exit 0
