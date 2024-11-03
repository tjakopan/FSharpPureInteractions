module ReservationClient.ReservationHttpClient

open System
open System.Net.Http
open FSharp.Data

type ReservationJson =
  JsonProvider<"""{
  "date": "some date",
  "name": "Mark Seemann",
  "email": "mark@pleoh.dk",
  "quantity": 4
}""">

type AvailabilityJson =
  JsonProvider<"""{
  "openings": [
    {
      "date": "some date",
      "seats": 10
    }
  ]
}""">

let private baseAddress = "http://localhost:5000"

let private toSlot (o: AvailabilityJson.Opening) =
  { Date = DateTimeOffset.Parse o.Date
    SeatsLeft = o.Seats }

let getSlots (d: DateTimeOffset) =
  async {
    use client = new HttpClient()

    use! response =
      $"%s{baseAddress}/availability/%i{d.Year}/%i{d.Month}/%i{d.Day}"
      |> client.GetAsync
      |> Async.AwaitTask

    let! content = response.Content.ReadAsStringAsync() |> Async.AwaitTask
    let a = AvailabilityJson.Parse content
    return a.Openings |> Array.map toSlot |> Array.toList
  }

let postReservation r =
  async {
    use client = new HttpClient()

    let json =
      ReservationJson
        .Root(r.Date.ToString "o", r.Name, r.Email, r.Quantity)
        .ToString()

    use content = new StringContent(json)
    content.Headers.ContentType.MediaType <- "application/json"
    let! response = client.PostAsync($"%s{baseAddress}/reservations", content) |> Async.AwaitTask
    printfn $"%A{response.StatusCode}"
  }
