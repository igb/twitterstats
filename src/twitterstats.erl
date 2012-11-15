-module(twitterstats).
-export([get_stats/1, print_stats/1]).


print_stats(User)->
     {TotalTweetCount, OriginalTweetCount, RetweetCount}=get_stats(User),
     io:format("Out of your last ~p tweets ~p are original content and ~p are 'retweets'. That means you are ~p% original.~n", [TotalTweetCount, OriginalTweetCount, RetweetCount, trunc((OriginalTweetCount/TotalTweetCount) * 100)]).

get_stats(User)->
    [application:start(X) || X <- [inets, public_key,crypto, ssl]],
    Data=get_next_tweets(User, 0, []),
    {OriginalTweetCount, RetweetCount}=count(Data),
    {length(Data), OriginalTweetCount, RetweetCount}.
    

count(TotalData)->
    Retweets=[X || X <- TotalData, X > 0],
    {length(TotalData) - length(Retweets),length(Retweets)}.

 
get_tweets(User, MaxId)->
    RequestUrl=append_max_id(lists:flatten(["https://api.twitter.com/1/statuses/user_timeline.json?include_entities=true&include_rts=true&screen_name=",User]), MaxId),    
    {ok, {StatusLine, Headers, Body}}=httpc:request(get, {RequestUrl, []}, [],[]),

%    io:fwrite("Headers:~n~p~n", [Headers]),

    case StatusLine of
	{"HTTP/1.1",200,"OK"} ->
	     case Body of
		 "[]"->[];
		 _->re:split(Body, "},{\"created")
	     end;     
	 {err, StatusLine} ->
	     StatusLine
     end.

append_max_id(RequestUrl, MaxId) when MaxId > 0 ->
       lists:flatten([RequestUrl, "&max_id=", integer_to_list(MaxId)]);    
append_max_id(RequestUrl, _)->
    RequestUrl.

get_next_tweets(User,LastTweetId, Data)->
    Tweets=get_tweets(User, LastTweetId - 1),
    case Tweets of
	[]->
	    Data;
	_  -> DataAcc=lists:append(Data, [string:str(binary_to_list(X), "retweeted_status") || X <- Tweets]),
	      NewLastTweetId=get_last_tweet_id(Tweets),
	      get_next_tweets(User, NewLastTweetId, DataAcc)
    end. 
	    
    

get_last_tweet_id(Tweets)->
    [LastTweet|_]=lists:nthtail(length(Tweets)-1, Tweets),
    LastTweetStr= binary_to_list(LastTweet),
    get_tweet_id(LastTweetStr).


get_tweet_id(Tweet)->
    StartIdx=string:str(Tweet, "\"id\":"),
    TruncatedTweet=string:substr(Tweet, StartIdx + 5),
    EndIdx=string:chr(TruncatedTweet, $,),
    IdString=string:substr(TruncatedTweet, 1, EndIdx -1),
    {Id,[]}=string:to_integer(IdString),
    Id.

    
