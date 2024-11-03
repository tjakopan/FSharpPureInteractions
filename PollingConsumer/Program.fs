﻿open System
open PollingConsumer

let limit = TimeSpan.FromMinutes 1.

let printOnEntry (timeAtEntry: DateTimeOffset) =
  printfn "Started polling at %s." (timeAtEntry.ToString "T")
  printfn ""

let printOnExit timeAtEntry (durations: TimeSpan list) =
  let stats = Statistics.calculateAverageAndStandardDeviation durations
  let timeAtExit = DateTimeOffset.Now
  let elapsed = timeAtExit - timeAtEntry

  let durationColor =
    if elapsed <= limit then
      ConsoleColor.Green
    else
      ConsoleColor.Red

  printfn ""
  printfn "Stopped polling at %s." (timeAtExit.ToString "T")
  printf "Elapsed time: "
  ColorPrint.cprintf durationColor "%s" (elapsed.ToString "c")
  printfn "."
  printfn $"Handled %d{durations.Length} message(s)."

  stats
  |> Option.map (fun (avg, stdDev) -> avg.ToString "T", stdDev.ToString "T")
  |> Option.iter (fun (avg, stdDev) ->
    printfn $"Average duration: %s{avg}"
    printfn $"Standard deviation: %s{stdDev}")

let rec interpret =
  function
  | Pure x -> x
  | Free(CurrentTime next) -> DateTimeOffset.Now |> next |> interpret
  | Free(Poll next) -> Implementation.poll () |> next |> interpret
  | Free(Handle(msg, next)) -> Implementation.handle msg |> next |> interpret
  | Free(Idle(d, next)) -> Implementation.idle d |> next |> interpret

let rec run estimatedDuration idleDuration stopBefore s =
  let ns =
    PollingConsumer.transition estimatedDuration idleDuration stopBefore s
    |> interpret

  match ns with
  | PollingConsumer.StoppedState _ -> ns
  | _ -> run estimatedDuration idleDuration stopBefore ns

[<EntryPoint>]
let main _ =
  let timeAtEntry = DateTimeOffset.Now
  printOnEntry timeAtEntry
  let stopBefore = timeAtEntry + limit
  let estimatedDuration = TimeSpan.FromSeconds 2.
  let idleDuration = TimeSpan.FromSeconds 5. |> IdleDuration

  let durations =
    PollingConsumer.ReadyState []
    |> run estimatedDuration idleDuration stopBefore
    |> PollingConsumer.durations
    |> List.map PollingConsumer.toTotalCycleTimeSpan

  printOnExit timeAtEntry durations
  0
