open System.Net.WebSockets
open System.Threading
open System
open System.Text

let socket = new ClientWebSocket()
let cts = new CancellationTokenSource()
let uri = Uri("ws://localhost:8080/websocket")

let aa = socket.ConnectAsync(uri, cts.Token)

while not (aa.IsCompletedSuccessfully) do
    Thread.Sleep(500)

// let a = ArraySegment<byte>[|byte('1'); byte('2'); byte('3')|]
let a = Encoding.ASCII.GetBytes("123") |> ArraySegment<byte>
let aaa = socket.SendAsync (a, WebSocketMessageType.Text, true, cts.Token)
if aaa.IsCompletedSuccessfully then
    socket.CloseAsync (WebSocketCloseStatus.Empty, "", cts.Token) |> ignore