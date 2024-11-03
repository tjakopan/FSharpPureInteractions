module PollingConsumer.Statistics

open System

let calculateAverage (durations: TimeSpan list) =
  if durations.IsEmpty then
    None
  else
    durations
    |> List.averageBy (fun x -> float x.Ticks)
    |> int64
    |> TimeSpan.FromTicks
    |> Some

let calculateAverageAndStandardDeviation durations =
  let stdDev (avg: TimeSpan) =
    durations
    |> List.averageBy (fun x -> ((x - avg).Ticks |> float) ** 2.)
    |> sqrt
    |> int64
    |> TimeSpan.FromTicks

  durations |> calculateAverage |> Option.map (fun avg -> (avg, stdDev avg))

let calculateExpectedDuration estimatedDuration durations =
  match calculateAverageAndStandardDeviation durations with
  | None -> estimatedDuration
  | Some(avg, stdDev) -> avg + stdDev + stdDev + stdDev

