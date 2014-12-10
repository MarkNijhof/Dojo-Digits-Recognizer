open System
open System.IO
let trainingPath = "/home/mark/src/Dojo-Digits-Recognizer/Dojo/trainingsample.csv"
let validationPath = "/home/mark/src/Dojo-Digits-Recognizer/Dojo/validationsample.csv"

type Image = { Label:int; Pixels:int[] }
type Match = { Distance:float; Sample:Image; Match:Image }

let distance (image1:int[]) (image2:int[]) =
  Array.map2 (fun (img1pixel:int) (img2pixel:int) -> (img1pixel-img2pixel) * (img1pixel-img2pixel) ) (image1:int[]) (image2:int[])
  |> Array.sum
  |> float
  |> sqrt

let convertFile path =
  File.ReadAllLines(path)
  |> (fun x -> x.[1 ..])
  |> Array.map (fun (x:string) -> x.Split(','))
  |> Array.map (Array.map(fun (x:string) -> Convert.ToInt32 x))
  |> Array.map (fun (x:int[]) -> { Label = x.[0]; Pixels = x.[1 ..] })


let clasifyImage (image:Image) (images:Image[]) =
  images
  |> Array.map (fun (x:Image) -> { Distance = (distance image.Pixels x.Pixels); Sample = image; Match = x } )
  |> Array.sortBy (fun (x:Match) -> x.Distance)
  |> Seq.truncate 10
  |> Seq.groupBy (fun (x:Match) -> x.Match.Label)
  |> Seq.maxBy (fun (k,v) -> v |> Seq.length)
  |> (fun (k,v) -> v)
  |> Seq.toArray
  |> Array.minBy (fun (x:Match) -> x.Distance)

let validate (training:Image[]) (samples:Image[]) =
  samples
  |> Array.map (fun (sample:Image) -> clasifyImage sample training)
  |> Array.filter (fun (item:Match) -> (item.Sample.Label <> item.Match.Label) )


let training = convertFile trainingPath
let validation = convertFile validationPath

//clasifyImage validation.[0] training

let faults = validate (training:Image[]) (validation:Image[])

(float 100) - (float (Array.length faults)) / ((float (Array.length validation)) / float 100)
