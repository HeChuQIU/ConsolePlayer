open System
open System.Collections.Concurrent
open System.Diagnostics
open System.Drawing
open System.Text
open System.Text.RegularExpressions
open System.Threading.Tasks
open Argu
open NAudio.Wave
open OpenCvSharp

type CliArguments =
    | [<Mandatory>] Input of path: string
    | Buffer of size: int

    interface IArgParserTemplate with
        member s.Usage =
            match s with
            | Input _ -> "Input file"
            | Buffer _ -> "Buffer size"

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

let GetPlayTimeText (capture: VideoCapture, delay:TimeSpan) =
    let totalTime = TimeSpan.FromSeconds(float capture.FrameCount / capture.Fps)
    let currentTime = TimeSpan.FromSeconds(float capture.PosFrames / capture.Fps) - delay
    $"""{currentTime.Minutes.ToString("D2")}:{currentTime.Seconds.ToString("D2")}/{totalTime.Minutes.ToString("D2")}:{totalTime.Seconds.ToString("D2")}"""

let inputPath = result.GetResult Input
let bufferSize = result.GetResult Buffer

let main () =
    let capture = new VideoCapture(inputPath)
    use waveOut = new WaveOutEvent()
    use audioReader = new AudioFileReader(inputPath)

    if not (capture.IsOpened()) then
        Console.WriteLine("Failed to open video file")
        1
    else
        let mutable lastBufferSize = (Console.BufferWidth, Console.BufferHeight)
        let mutable lastFrameSize = (Console.BufferWidth, Console.BufferHeight)
        let frameBuffer =
            new BlockingCollection<string array>(ConcurrentQueue<string array>(), bufferSize)

        let image = new Mat()
        let resizedImage = new Mat()

        waveOut.Init(audioReader)
        waveOut.Volume <- 0.5f

        let mutable renderTask =
            Task.Run(fun () ->
                (while capture.Read(image) do
                    resizeFrameByConsoleSize (image, resizedImage)
                    let output = renderFrameToText resizedImage
                    frameBuffer.Add(output)))

        let printStatus (delay: TimeSpan) =
            Console.SetCursorPosition(0, Console.BufferHeight - 1)
            Console.Write(colorText (Color.White, GetPlayTimeText (capture, TimeSpan.FromSeconds(float frameBuffer.Count/float capture.Fps))))
            let delayMilliseconds = int delay.TotalMilliseconds
            let color = if delayMilliseconds > 0 then Color.Green else Color.Red

            Console.Write(
                colorText (color, $""" {delayMilliseconds.ToString(if delayMilliseconds < 0 then "D3" else "D4")}ms""")
            )

        let rec printFrame () =
            let timeForNextFrame = DateTime.Now.Add(TimeSpan.FromSeconds(1.0 / capture.Fps))

            if frameBuffer.Count = 0 && renderTask.Status = TaskStatus.RanToCompletion then
                0
            else
                let frame = frameBuffer.Take()
                if frame.Length > 0 then
                    let currentFrameSize = (Regex.Matches(frame[0], Regex.Escape("██")).Count, frame.Length)
                    let currentBufferSize = (Console.BufferWidth, Console.BufferHeight)

                    if (lastFrameSize,lastBufferSize)<>(currentFrameSize,currentBufferSize) then
                        lastFrameSize <- currentFrameSize
                        lastBufferSize <- currentBufferSize
                        Console.Clear()

                    Console.SetCursorPosition(0, 0)
                    frame |> Array.iter Console.WriteLine

                let delay = timeForNextFrame - DateTime.Now
                printStatus delay

                if delay > TimeSpan.Zero then
                    let sw = Stopwatch.StartNew()
                    let delayMilliseconds = delay.TotalMilliseconds
                    while sw.ElapsedMilliseconds < int delayMilliseconds do
                        ()
                else
                    audioReader.CurrentTime <- TimeSpan.FromSeconds(float (capture.PosFrames - frameBuffer.Count) / capture.Fps)

                printFrame ()

        waveOut.Play()
        printFrame () |> ignore
        image.Dispose()
        resizedImage.Dispose()
        0

exit (main ())
