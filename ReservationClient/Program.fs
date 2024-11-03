module ReservationClient.Program

open System
open Microsoft.FSharp.Control

let rec interpretCommandLine =
  function
  | CommandLineProgram.Pure x -> x
  | CommandLineProgram.Free(ReadLine next) -> Console.ReadLine() |> next |> interpretCommandLine
  | CommandLineProgram.Free(WriteLine(s, next)) ->
    Console.WriteLine(s)
    next |> interpretCommandLine

let rec interpretReservationsApi =
  function
  | ReservationsApiProgram.Pure x -> x
  | ReservationsApiProgram.Free(GetSlots(d, next)) ->
    ReservationHttpClient.getSlots d
    |> Async.RunSynchronously
    |> next
    |> interpretReservationsApi
  | ReservationsApiProgram.Free(PostReservation(r, next)) ->
    ReservationHttpClient.postReservation r |> Async.RunSynchronously
    next |> interpretReservationsApi

let rec interpret =
  function
  | CommandLineReservationsApiProgram.Pure x -> x
  | CommandLineReservationsApiProgram.Free p -> p |> interpretCommandLine |> interpretReservationsApi |> interpret

[<EntryPoint>]
let main _ =
  interpret Wizard.tryReserve
  0
