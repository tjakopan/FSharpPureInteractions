open System

type PollDuration = PollDuration of TimeSpan
type IdleDuration = IdleDuration of TimeSpan
type HandleDuration = HandleDuration of TimeSpan

type CycleDuration =
  { PollDuration: PollDuration
    HandleDuration: HandleDuration }

type State<'msg> =
  | ReadyState of CycleDuration list
  | ReceivedMessageState of (CycleDuration list * PollDuration * 'msg)
  | NoMessageState of (CycleDuration list * PollDuration)
  | StoppedState of CycleDuration list

type PollingInstruction<'msg, 'next> =
  | CurrentTime of (DateTimeOffset -> 'next)
  | Poll of ('msg option * PollDuration -> 'next)
  | Handle of ('msg * (HandleDuration -> 'next))
  | Idle of (IdleDuration * (IdleDuration -> 'next))

let private mapI f =
  function
  | CurrentTime next -> CurrentTime(next >> f)
  | Poll next -> Poll(next >> f)
  | Handle(x, next) -> Handle(x, next >> f)
  | Idle(x, next) -> Idle(x, next >> f)

type PollingProgram<'msg, 'next> =
  | Free of PollingInstruction<'msg, PollingProgram<'msg, 'next>>
  | Pure of 'next

let rec bind f =
  function
  | Free instruction -> instruction |> mapI (bind f) |> Free
  | Pure x -> f x

let map f = bind (f >> Pure)

let currentTime = Free(CurrentTime Pure)
let poll = Free(Poll Pure)
let handle msg = Free(Handle(msg, Pure))
let idle duration = Free(Idle(duration, Pure))

type PollingBuilder() =
  member this.Bind(x, f) = bind f x
  member this.Return(x) = Pure x
  member this.ReturnFrom(x) = x
  member this.Zero() = Pure()

let polling = PollingBuilder()

let private shouldIdle (IdleDuration d) stopBefore =
  polling {
    let! now = currentTime
    return now + d < stopBefore
  }

let toTotalCycleTimeSpan x =
  let (PollDuration pd) = x.PollDuration
  let (HandleDuration hd) = x.HandleDuration
  pd + hd

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

let private shouldPoll estimatedDuration stopBefore statistics =
  polling {
    let expectedHandleDuration =
      statistics
      |> List.map toTotalCycleTimeSpan
      |> calculateExpectedDuration estimatedDuration

    let! now = currentTime
    return now + expectedHandleDuration < stopBefore
  }

let transitionFromStopped s = polling { return StoppedState s }

let transitionFromReceived (statistics, pd, msg) =
  polling {
    let! hd = handle msg

    return
      { PollDuration = pd
        HandleDuration = hd }
      :: statistics
      |> ReadyState
  }

let transitionFromNoMessage d stopBefore (statistics, _) =
  polling {
    let! b = shouldIdle d stopBefore

    if b then
      do! idle d |> map ignore
      return ReadyState statistics
    else
      return StoppedState statistics
  }

let transitionFromReady estimatedDuration stopBefore statistics =
  polling {
    let! b = shouldPoll estimatedDuration stopBefore statistics

    if b then
      let! pollResult = poll

      match pollResult with
      | Some msg, pd -> return ReceivedMessageState(statistics, pd, msg)
      | None, pd -> return NoMessageState(statistics, pd)
    else
      return StoppedState statistics
  }

let transition estimatedDuration idleDuration stopBefore = function
  | ReadyState s -> transitionFromReady estimatedDuration stopBefore s
  | ReceivedMessageState s -> transitionFromReceived s
  | NoMessageState s -> transitionFromNoMessage idleDuration stopBefore s
  | StoppedState s -> transitionFromStopped s

let rec interpret = function
  | Pure x -> x
  | Free(CurrentTime next) -> DateTimeOffset.Now |> next |> interpret
  | Free(Poll next) -> poll |> next |> interpret
  