open Suave
open Suave.Http
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.Files
open Suave.RequestErrors
open Suave.Logging
open Suave.Utils

open System
open System.Net

open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket

open FSharp.Json
open System.Text.RegularExpressions
open FSharpx.Collections


type TweetMessage = {
    Author: string
    Tweet: string
}

type ApiMessage = {
    Author: string
    Payload: string
    Operation: string
}

type FindUserApiMessage = {
    UserName: string
    Tweets: string
    Followers: string
    Following: string
}

let mutable allUsers = Map.empty
let mutable allUserRefs = Map.empty
let mutable allTweets = Map.empty
let mutable subscribers = Map.empty
let mutable subscribing = Map.empty
let mutable tweetsToBeSent = Map.empty
let mutable userSubscribedTweets = Map.empty
let mutable myMentions = Map.empty
let mutable onlineUsers = Array.empty
let mutable tweetsReceived = 0

let isOnline user =
    onlineUsers |> Array.exists ((=) user)

let searchMentions newTweet=
    let mutable newUser = ""
    let mutable userTweetNumber = Array.create 0 ""
    let mutable userFound = 0
    for c in newTweet do
        if userFound = 0 && c = '@' then
            userFound <- 1
        elif userFound = 1 && (c = ' ' || c = '@' || c = '#') then
            newUser <- newUser.Trim()
            if newUser <> "" then
                userTweetNumber <- Array.concat [| userTweetNumber ; [|newUser|] |]
            userFound <- 0
            newUser <- ""
            if c = '@' then
                userFound <- 1
        elif userFound = 1 && c <> '@' then
            newUser <- newUser + string(c)
    newUser <- newUser.Trim()
    if newUser <> "" then
        userTweetNumber <- Array.concat [| userTweetNumber ; [|newUser|] |]
    userTweetNumber

let searchHashtags newTweet=
    let mutable newUser = ""
    let mutable userTweetNumber = Array.create 0 ""
    let mutable userFound = 0
    for c in newTweet do
        if userFound = 0 && c = '#' then
            userFound <- 1
        elif userFound = 1 && (c = ' ' || c = '@' || c = '#') then
            newUser <- newUser.Trim()
            if newUser <> "" then
                userTweetNumber <- Array.concat [| userTweetNumber ; [|newUser|] |]
            userFound <- 0
            newUser <- ""
            if c = '#' then
                userFound <- 1
        elif userFound = 1 && c <> '#' then
            newUser <- newUser + string(c)
    newUser <- newUser.Trim()
    if newUser <> "" then
        userTweetNumber <- Array.concat [| userTweetNumber ; [|newUser|] |]
    userTweetNumber

let searchTweets (receivedTweets:string[]) (searchString:string)=
    let mutable searchedTweets = Array.create 0 ""
    for newTweets in receivedTweets do
        let mutable wordIndex = newTweets.IndexOf(searchString)
        if wordIndex <> -1 then
            searchedTweets <- Array.concat [| searchedTweets ; [|newTweets|] |]
    searchedTweets
            
let matchSample r m =
    let r = Regex(r)
    let m1 = r.Match m
    let idFound = m1.Groups.[1] |> string |> int
    idFound


let ws (webSocket : WebSocket) (context: HttpContext) =
  socket {
    // if `loop` is set to false, the server will stop receiving messages
    let mutable loop = true

    while loop do
      // the server will wait for a message to be received without blocking the thread
      let! msg = webSocket.read()

      match msg with
      // the message has type (Opcode * byte [] * bool)
      //
      // Opcode type:
      //   type Opcode = Continuation | Text | Binary | Reserved | Close | Ping | Pong
      //
      // byte [] contains the actual message
      //
      // the last element is the FIN byte, explained later
      | (Text, data, true) ->
        // the message can be converted to a string
        let str = UTF8.toString data

        let message = Json.deserialize<ApiMessage> str
        printfn "Actor called with %A" message
        
        let operation = message.Operation
        let username = message.Author
        let payload = message.Payload

        printfn "Operation %s" operation
        match operation with

        | "SignUp" ->
            let mutable response = ""
            if not (allUserRefs.ContainsKey(username)) then
                let result = payload.Split ','
                allUsers <- allUsers.Add(username, result.[2])
                allTweets <- allTweets.Add(username, [|""|])
                subscribers <- subscribers.Add(username, [|""|])
                subscribing <- subscribing.Add(username, [|""|])
                tweetsToBeSent <- tweetsToBeSent.Add(username, [|{Author = ""; Tweet = ""}|])
                myMentions <- myMentions.Add(username, [|{Author = ""; Tweet = ""}|])
                userSubscribedTweets <- userSubscribedTweets.Add(username, [|""|])
                response <- "Signup done, now login"
            else
                response <- "Already registered, now login"
            printfn "str--%s" str
            let byteResponse =
                response
                |> System.Text.Encoding.ASCII.GetBytes
                |> ByteSegment
            do! webSocket.send Text byteResponse true



        | "Login" ->
            let mutable response = ""
            if (allUsers.ContainsKey(username)) then
                if payload = allUsers.[username] then
                    response <- "Logged in!"
                    allUserRefs <- allUserRefs.Add(username, webSocket)
                    onlineUsers <- Array.concat [| onlineUsers ; [|username|] |]
                    let byteResponse =
                      response
                      |> System.Text.Encoding.ASCII.GetBytes
                      |> ByteSegment

                    do! allUserRefs.[username].send Text byteResponse true
                    let tweetsToBeSentToUser = tweetsToBeSent.[username]
                    if tweetsToBeSentToUser.Length > 1 then
                        let data = {Author = username ; Payload = tweetsToBeSentToUser |> string; Operation = "TweetsWhileOffline"}
                        let response = Json.serialize data
                        let byteResponse =
                            response
                            |> System.Text.Encoding.ASCII.GetBytes
                            |> ByteSegment

                        do! allUserRefs.[username].send Text byteResponse true
                else
                    response <- "Incorrect Password, please try again!"
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment
                    do! webSocket.send Text byteResponse true
            else
                response <- "User not found, first register the user!"
                let byteResponse =
                    response
                    |> System.Text.Encoding.ASCII.GetBytes
                    |> ByteSegment
                do! webSocket.send Text byteResponse true



        | "Tweet" ->
            if (allUserRefs.ContainsKey(username)) then
                if isOnline(username) then
                    let mutable allTweetsByUser = allTweets.[username]
                    allTweetsByUser <- Array.concat [| allTweetsByUser ; [|payload|] |]
                    allTweets <- allTweets.Add(username, allTweetsByUser)
                    printfn "All tweets: %A" allTweets.[username]
                    let data = {Author = username ; Tweet = payload}
                    let response = Json.serialize data
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment

                    do! allUserRefs.[username].send Text byteResponse true

                    // store user mentions for tweet
                    let userMentions = searchMentions payload
                    for mentioned in userMentions do
                        let mutable myMentionedTweets = myMentions.[mentioned]
                        myMentionedTweets <- Array.concat [| myMentionedTweets ; [|data|] |]
                        myMentions <- myMentions.Add(mentioned, myMentionedTweets)

                    // who should this tweet be sent out to?
                    let mutable allSubscribers = subscribers.[username]
                
                    for mentioned in userMentions do
                        allSubscribers <- allSubscribers |> Array.filter ((<>) mentioned )
                        allSubscribers <- Array.concat [| allSubscribers ; [|mentioned|] |]

                    for subs in allSubscribers do
                        if subs <> "" then
                            // should we send it or not?                    
                            if not (isOnline username) then
                                let mutable usertweetsToBeSent = tweetsToBeSent.[subs]
                                usertweetsToBeSent <- Array.concat [| usertweetsToBeSent ; [|data|] |]
                                tweetsToBeSent <- tweetsToBeSent.Add(subs, usertweetsToBeSent)
                            else
                                do! allUserRefs.[subs].send Text byteResponse true
                else
                    let response = "User is offline, first login!"
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment
                    do! webSocket.send Text byteResponse true
            else
                let response = "User not found, first register the user!"
                let byteResponse =
                    response
                    |> System.Text.Encoding.ASCII.GetBytes
                    |> ByteSegment
                do! webSocket.send Text byteResponse true

        | "FindUser" ->
            if (allUserRefs.ContainsKey(username)) then
                if isOnline(username) then
                    let mutable response = ""
                    if (allUserRefs.ContainsKey(payload)) then
                        let mutable allTweetsByUser = allTweets.[payload]
                        let mutable myFollowers = subscribers.[payload]
                        let mutable myFollowings = subscribing.[payload]
                        let data = {UserName = payload ; Tweets = allTweetsByUser |> String.concat "////" |> string; Followers = myFollowers |> String.concat "////" ; Following =  myFollowings |> String.concat "////" }
                        response <- Json.serialize data
                    else
                        response <- "User not found"

                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment
                    do! allUserRefs.[username].send Text byteResponse true
                else
                    let response = "User is offline, first login!"
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment
                    do! webSocket.send Text byteResponse true
            else
                let response = "User not found, first register the user!"
                let byteResponse =
                    response
                    |> System.Text.Encoding.ASCII.GetBytes
                    |> ByteSegment
                do! webSocket.send Text byteResponse true

        | "Follow" ->
            if (allUserRefs.ContainsKey(username)) then
                if isOnline(username) then
                    let mutable userSubscribers = subscribers.[payload]
                    userSubscribers <- Array.concat [| userSubscribers ; [|username|] |] 
                    subscribers <- subscribers.Add(payload, userSubscribers)

                    let mutable userSubscribing = subscribing.[username]
                    userSubscribing <- Array.concat [| userSubscribing ; [|payload|] |] 
                    subscribing <- subscribing.Add(username, userSubscribers)

                    let response = sprintf "%s is subscribing to %s" username payload
                    printfn "%s is subscribing to %s" username payload
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment

                    do! allUserRefs.[username].send Text byteResponse true
                    do! allUserRefs.[payload].send Text byteResponse true
                else
                    let response = "User is offline, first login!"
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment
                    do! webSocket.send Text byteResponse true
            else
                let response = "User not found, first register the user!"
                let byteResponse =
                    response
                    |> System.Text.Encoding.ASCII.GetBytes
                    |> ByteSegment
                do! webSocket.send Text byteResponse true

        | "MyMentions" ->
            if (allUserRefs.ContainsKey(username)) then
                if isOnline(username) then
                    let mutable myMentionedTweets = myMentions.[username]
                    let myMentionedTweetsJson = Json.serialize myMentionedTweets
                    let data = {Author = username |> string; Payload = myMentionedTweetsJson |> string; Operation = "MyMentions"}
                    let response = Json.serialize data
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment

                    do! allUserRefs.[username].send Text byteResponse true
                else
                    let response = "User is offline, first login!"
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment
                    do! webSocket.send Text byteResponse true
            else
                let response = "User not found, first register the user!"
                let byteResponse =
                    response
                    |> System.Text.Encoding.ASCII.GetBytes
                    |> ByteSegment
                do! webSocket.send Text byteResponse true

        | "Query" ->
            if (allUserRefs.ContainsKey(username)) then
                if isOnline(username) then
                    let mutable allTweetsWithQueryString = [||]
                    for users in Map.keys(allUsers) do
                        let mutable allTweetsByUser = allTweets.[users]
                        let mutable thisUsersQueryTweets = searchTweets allTweetsByUser payload
                        for tweets in thisUsersQueryTweets do
                            let data = {Author = users |> string; Tweet = tweets |> string}
                            let data_json = Json.serialize data
                            allTweetsWithQueryString <- Array.concat [| allTweetsWithQueryString ; [|data_json|] |]
                        //allTweetsWithQueryString <- Array.concat [| allTweetsWithQueryString ; [|thisUsersQueryTweets|] |]
                    let data = {Author = username ; Payload = allTweetsWithQueryString |> String.concat "////" |> string; Operation = "Query"}
                    let response = Json.serialize data
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment

                    do! allUserRefs.[username].send Text byteResponse true
                else
                    let response = "User is offline, first login!"
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment
                    do! webSocket.send Text byteResponse true
            else
                let response = "User not found, first register the user!"
                let byteResponse =
                    response
                    |> System.Text.Encoding.ASCII.GetBytes
                    |> ByteSegment
                do! webSocket.send Text byteResponse true

        | "Logout" ->
            if (allUserRefs.ContainsKey(username)) then
                if isOnline(username) then
                    onlineUsers <- onlineUsers |> Array.filter ((<>) username )
                    let response = "Logged out!"
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment

                    do! allUserRefs.[username].send Text byteResponse true
                else
                    let response = "User is offline, first login!"
                    let byteResponse =
                        response
                        |> System.Text.Encoding.ASCII.GetBytes
                        |> ByteSegment
                    do! webSocket.send Text byteResponse true

        | _ -> ()

      | (Close, _, _) ->
        let emptyResponse = [||] |> ByteSegment
        do! webSocket.send Close emptyResponse true

        // after sending a Close message, stop the loop
        loop <- false

      | _ -> ()
    }

/// An example of explictly fetching websocket errors and handling them in your codebase.
let wsWithErrorHandling (webSocket : WebSocket) (context: HttpContext) = 
   
   let exampleDisposableResource = { new IDisposable with member __.Dispose() = printfn "Resource needed by websocket connection disposed" }
   let websocketWorkflow = ws webSocket context
   
   async {
    let! successOrError = websocketWorkflow
    match successOrError with
    // Success case
    | Choice1Of2() -> ()
    // Error case
    | Choice2Of2(error) ->
        // Example error handling logic here
        printfn "Error: [%A]" error
        exampleDisposableResource.Dispose()
        
    return successOrError
   }

let app : WebPart = 
  choose [
    path "/websocket" >=> handShake ws
    path "/websocketWithSubprotocol" >=> handShakeWithSubprotocol (chooseSubprotocol "test") ws
    path "/websocketWithError" >=> handShake wsWithErrorHandling
    GET >=> choose [ path "/" >=> file "index.html"; browseHome ]
    NOT_FOUND "Found no handlers." ]

//[<EntryPoint>]
let myCfg =
  { defaultConfig with
      bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 8082 ]
  }

let main argv =
  startWebServer { defaultConfig with logger = Targets.create Verbose [||] } app
  0

//
// The FIN byte:
//
// A single message can be sent separated by fragments. The FIN byte indicates the final fragment. Fragments
//
// As an example, this is valid code, and will send only one message to the client:
//
// do! webSocket.send Text firstPart false
// do! webSocket.send Continuation secondPart false
// do! webSocket.send Continuation thirdPart true
//
// More information on the WebSocket protocol can be found at: https://tools.ietf.org/html/rfc6455#page-34
//
main 0