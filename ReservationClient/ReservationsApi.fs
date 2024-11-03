namespace ReservationClient

open System

type Slot =
  { Date: DateTimeOffset; SeatsLeft: int }

type Reservation =
  { Date: DateTimeOffset
    Name: string
    Email: string
    Quantity: int }

type ReservationsApiInstruction<'a> =
  | GetSlots of (DateTimeOffset * (Slot list -> 'a))
  | PostReservation of Reservation * 'a

type ReservationsApiProgram<'a> =
  | Free of ReservationsApiInstruction<ReservationsApiProgram<'a>>
  | Pure of 'a

module ReservationsApi =
  let private mapI f =
    function
    | GetSlots(dt, next) -> GetSlots(dt, next >> f)
    | PostReservation(r, next) -> PostReservation(r, f next)

  let rec bind f =
    function
    | Free instruction -> instruction |> mapI (bind f) |> Free
    | Pure x -> f x

  let map f = bind (f >> Pure)

  let getSlots date = Free(GetSlots(date, Pure))

  let postReservation r = Free(PostReservation(r, Pure()))

type ReservationsApiBuilder() =
  member this.Bind(x, f) = ReservationsApi.bind f x
  member this.Return x = Pure x
  member this.ReturnFrom x = x
  member this.Zero() = Pure()

[<AutoOpen>]
module ReservationsApiComputationExpression =
  let reservationsApi = ReservationsApiBuilder()
