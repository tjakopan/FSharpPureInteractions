module PollingConsumer.Implementation

open System

let private time f x =
  let sw = System.Diagnostics.Stopwatch()
  sw.Start()
  let result = f x
  sw.Stop()
  (result, sw.Elapsed)

let private r = Random()

let poll = time (Simulation.pollForMessage r) >> fun (msg, d) -> msg, PollDuration d

let handle = time (Simulation.handle r) >> snd >> HandleDuration

let idle (IdleDuration d) =
  let s () =
    d.TotalMilliseconds |> int |> Async.Sleep |> Async.RunSynchronously

  ColorPrint.cprintfn ConsoleColor.Yellow "Sleeping"
  time s () |> snd |> IdleDuration
