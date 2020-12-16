
import {Socket} from "phoenix"

let socket = new Socket("/socket", {params: {token: window.userToken}})

socket.connect()

// Now that you are connected, you can join channels with a topic:
let channel = socket.channel("lobby:update", {});

$(document).ready(function() { channel.push('update_socket', { username: userID });
});

if(document.getElementById("btnFollow"))
{
  document.getElementById("btnFollow").onclick = function() {
    channel.push('subscribeTo', { username2: $('#username2').val(),
      selfId: window.location.hash.substring(1) });
  };
}

// if(document.getElementById("btnRetweet"))
// {
//   document.getElementById("btnRetweet").onclick = function() {
//     channel.push('reTweet', {
//       username: window.location.hash.substring(1),
//       tweet: $('input[name=radioTweet]:checked').attr("tweet"),
//       org: $('input[name=radioTweet]:checked').attr("user")
//     });
//   }};

if(document.getElementById("btnTweet"))
{
  $(document).ready(function() {
    channel.push('update_socket', { username: userID });
  });

  document.getElementById("btnTweet").onclick = function() {
    channel.push('tweet', { tweetText: $('#tweetContent').val() , username:
          window.location.hash.substring(1) });
  };
}

// if(document.getElementById("btnRetweet"))
// {
//   document.getElementById("btnRetweet").onclick = function() {
//     channel.push('reTweet', {
//       username: window.location.hash.substring(1),
//       tweet: $('input[name=radioTweet]:checked').attr("tweet"),
//       org: $('input[name=radioTweet]:checked').attr("user")
//     });
//   }};

if(document.getElementById("signup"))
{
  console.log(new_username);
  document.getElementById("signup").onclick = function() {
    channel.push('register_account',
        {
          username: $('#new_username').val(),
          password: $('#new_password').val()
        });
  };
}



if(document.getElementById("btnMyMentions"))
{
  document.getElementById("btnMyMentions").onclick = function() {
    channel.push('getMyMentions', {
      username: window.location.hash.substring(1)
    });
  };
}

if(document.getElementById("btnhashtag"))
{
  document.getElementById("btnhashtag").onclick = function() {
    channel.push('tweetsWithHashtag', { hashtag: $('#hashtag').val() });
  };
}

// channel.on('ReceiveTweet', payload => {
//   let tweet_list    = $('#tweet-list');
// var btn = document.createElement("INPUT");
// btn.setAttribute('type', 'radio');
// btn.setAttribute('name', 'radioTweet');
// btn.setAttribute('user', `${payload.tweeter}`);
// btn.setAttribute('tweet', `${payload.tweetText}`);
// tweet_list.append(btn);
// if(`${payload.isRetweet}` == "false")
// {
//   tweet_list.append(`<b>${payload.tweeter} tweeted:</b> ${payload.tweetText}<br>`);
// }
// if(`${payload.isRetweet}` == "true")
// {
//   tweet_list.append(`<b>${payload.tweeter} retweeted ${payload.org}'s post:</b> ${payload.tweetText}<br>`);
// }
// tweet_list.prop({scrollTop: tweet_list.prop("scrollHeight")});
// });

if(document.getElementById("btnRetweet"))
{
  document.getElementById("btnRetweet").onclick = function() {
    channel.push('reTweet', {
      username: window.location.hash.substring(1),
      tweet: $('input[name=radioTweet]:checked').attr("tweet"),
      org: $('input[name=radioTweet]:checked').attr("user")
    });
  }};



if(document.getElementById("signin"))
{
  document.getElementById("signin").onclick = function() {
    channel.push('login', {
      username: $('#username').val(),
      password: $('#password').val()
    });
  };
}

// if(document.getElementById("btnRetweet"))
// {
//   document.getElementById("btnRetweet").onclick = function() {
//     channel.push('reTweet', {
//       username: window.location.hash.substring(1),
//       tweet: $('input[name=radioTweet]:checked').attr("tweet"),
//       org: $('input[name=radioTweet]:checked').attr("user")
//     });
//   }};

if(document.getElementById("btnQueryTweets"))
{
  var userID =  window.location.hash.substring(1)
  document.getElementById("btnQueryTweets").onclick = function() {
    channel.push('queryTweets',
        {
          username: window.location.hash.substring(1)
        });
  }
};

channel.on('Login', payload => {

  var unlog    = document.getElementById("unlog");
unlog.innerHTML = '';
if(`${payload.login_status}` == "Login unsuccessful")
{
  unlog.innerHTML+= (`<b>Incorrect username or password.Please try again!<br>`);
}
else
{
  unlog.innerHTML = '';
  console.log("redirect------")
  window.location.href = 'http://localhost:4000/dashboard' + '#' + payload.user_name;
}
});

channel.on('ReceiveTweet', payload => {
  let tweet_list    = $('#tweet-list');
var btn = document.createElement("INPUT");
btn.setAttribute('type', 'radio');
btn.setAttribute('name', 'radioTweet');
btn.setAttribute('user', `${payload.tweeter}`);
btn.setAttribute('tweet', `${payload.tweetText}`);
tweet_list.append(btn);
if(`${payload.isRetweet}` == "false")
{
  tweet_list.append(`<b>${payload.tweeter} tweeted:</b> ${payload.tweetText}<br>`);
}
if(`${payload.isRetweet}` == "true")
{
  tweet_list.append(`<b>${payload.tweeter} retweeted ${payload.org}'s post:</b> ${payload.tweetText}<br>`);
}
tweet_list.prop({scrollTop: tweet_list.prop("scrollHeight")});
});

channel.on('ReceiveHashtags', payload => {
  var hasharea   = document.getElementById("hashtagArea");
var myTweets2 = payload.tweets;
var arrayLength2 = myTweets2.length;
hasharea.innerHTML = '';
for (var i = 0; i < arrayLength2; i++) {
  hasharea.innerHTML+=(`<b>${payload.tweets[i].tweeter} tweeted:</b> ${payload.tweets[i].tweet}`);
  hasharea.innerHTML+="<br>";
}
$(hasharea).prop({scrollTop: $(hasharea).prop("scrollHeight")});
});
// sss

// channel.on('ReceiveQueryResults', payload => {
//   var area   = document.getElementById("queryArea");
// var myTweets = payload.tweets;
// var arrayLength = myTweets.length;
// area.innerHTML = '';
// for (var i = 0; i < arrayLength; i++) {
//   area.innerHTML+=(`<b>${payload.tweeter} tweeted:</b> ${payload.tweets[i]}`);
//   area.innerHTML+="<br>";
// }
// $(area).prop({scrollTop: $(area).prop("scrollHeight")});
// });
channel.on('ReceiveQueryResults', payload => {
  var area   = document.getElementById("queryArea");
var myTweets = payload.tweets;
var arrayLength = myTweets.length;
area.innerHTML = '';
for (var i = 0; i < arrayLength; i++) {
  area.innerHTML+=(`<b>${payload.tweeter} tweeted:</b> ${payload.tweets[i]}`);
  area.innerHTML+="<br>";
}
$(area).prop({scrollTop: $(area).prop("scrollHeight")});
});


channel.on('AddToFollowsList', payload => {
  var area   = document.getElementById("followsArea");
console.log("------------------------------------")
var follows = payload.follows;
var arrayLength = follows.length;
area.innerHTML = '';
for (var i = 0; i < arrayLength; i++) {
  console.log(payload.follows[i])
  area.innerHTML+=(`${payload.follows[i]}`);
  area.innerHTML+="<br>";
}
$(area).prop({scrollTop: $(area).prop("scrollHeight")});
});

// channel.on('AddToFollowsList', payload => {
//   var area   = document.getElementById("followsArea");
// console.log("------------------------------------")
// var follows = payload.follows;
// var arrayLength = follows.length;
// area.innerHTML = '';
// for (var i = 0; i < arrayLength; i++) {
//   console.log(payload.follows[i])
//   area.innerHTML+=(`${payload.follows[i]}`);
//   area.innerHTML+="<br>";
// }
// $(area).prop({scrollTop: $(area).prop("scrollHeight")});
// });

channel.on('ReceiveMentions', payload => {
  var area   = document.getElementById("mentionsArea");
var myTweets = payload.tweets;
var arrayLength = myTweets.length;
area.innerHTML = '';
for (var i = 0; i < arrayLength; i++) {
  area.innerHTML+=(`<b>${payload.tweets[i].tweeter} tweeted:</b> ${payload.tweets[i].tweet}`);
  area.innerHTML+="<br>";
}
$(area).prop({scrollTop: $(area).prop("scrollHeight")});
});


channel.join()
    .receive("ok", resp => { console.log("Joined successfully.", resp) })
.receive("error", resp => { console.log("Unable to join.", resp) })

export default socket
