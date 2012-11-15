-module(twitterstats).
-export([get_stats/1, print_stats/1]).


-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.


print_stats(User)->
     {TotalTweetCount, OriginalTweetCount, RetweetCount}=get_stats(User),
     io:format("Out of @~s's last ~p tweets ~p are original content and ~p are 'retweets'. That means @~s publishes ~p% original content.~n", [User, TotalTweetCount, OriginalTweetCount, RetweetCount, User, trunc((OriginalTweetCount/TotalTweetCount) * 100)]).

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

    io:fwrite("Headers:~n~p~n", [Headers]),

    case StatusLine of
	{"HTTP/1.1",200,"OK"} ->
	     case Body of
		 "[]"->[];
		 _->re:split(Body, "},{\"created")
	     end;     
	 {err, StatusLine} ->
	     StatusLine
     end.

process_tweet(Tweet)->
    TweetStr=binary_to_list(Tweet),
    IsRetweet=string:str(TweetStr, "retweeted_status").
    


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
    IdString=extract_chunk(Tweet, "\"id\":", ","),
    {Id,[]}=string:to_integer(IdString),
    Id.

extract_chunk(Tweet, ChunkStartToken, ChunkEndToken)->
    StartIdx=string:str(Tweet, ChunkStartToken),
    TruncatedTweet=string:substr(Tweet, StartIdx + length(ChunkStartToken)),
    EndIdx=string:str(TruncatedTweet, ChunkEndToken),
    string:substr(TruncatedTweet, 1, EndIdx -1).

extract_tweet_body(Tweet)->    
    extract_chunk(Tweet, "\"text\":\"","\",\"source\":\"").
    
-ifdef(TEST).

-define(TEST_TWEET, "{\"created_at\":\"Tue Nov 13 17:18:14 +0000 2012\",\"id\":268402405782196224,\"id_str\":\"268402405782196224\",\"text\":\"@benj_fry yeah, it's everywhere over here...crazy cognitive dissonance for a westerner.\",\"source\":\"\u003ca href=\\\"http:\/\/twitter.com\/download\/iphone\\\" rel=\\\"nofollow\\\"\u003eTwitter for iPhone\u003c\/a\u003e\",\"truncated\":false,\"in_reply_to_status_id\":268387265447874560,\"in_reply_to_status_id_str\":\"268387265447874560\",\"in_reply_to_user_id\":38293796,\"in_reply_to_user_id_str\":\"38293796\",\"in_reply_to_screen_name\":\"benj_fry\",\"user\":{\"id\":5649,\"id_str\":\"5649\",\"name\":\"Ian Brown\",\"screen_name\":\"igb\",\"location\":\"San Francisco, California\",\"url\":\"http:\/\/www.hccp.org\/\",\"description\":\"Engineering Manager at Salesforce.com\",\"protected\":false,\"followers_count\":106,\"friends_count\":105,\"listed_count\":6,\"created_at\":\"Sat Sep 09 03:38:31 +0000 2006\",\"favourites_count\":22,\"utc_offset\":-28800,\"time_zone\":\"Pacific Time (US & Canada)\",\"geo_enabled\":false,\"verified\":false,\"statuses_count\":1134,\"lang\":\"en\",\"contributors_enabled\":false,\"is_translator\":false,\"profile_background_color\":\"709397\",\"profile_background_image_url\":\"http:\/\/a0.twimg.com\/images\/themes\/theme6\/bg.gif\",\"profile_background_image_url_https\":\"https:\/\/si0.twimg.com\/images\/themes\/theme6\/bg.gif\",\"profile_background_tile\":false,\"profile_image_url\":\"http:\/\/a0.twimg.com\/profile_images\/56604308\/Photo_15_normal.jpg\",\"profile_image_url_https\":\"https:\/\/si0.twimg.com\/profile_images\/56604308\/Photo_15_normal.jpg\",\"profile_link_color\":\"FF3300\",\"profile_sidebar_border_color\":\"86A4A6\",\"profile_sidebar_fill_color\":\"A0C5C7\",\"profile_text_color\":\"333333\",\"profile_use_background_image\":true,\"default_profile\":false,\"default_profile_image\":false,\"following\":null,\"follow_request_sent\":null,\"notifications\":null},\"geo\":null,\"coordinates\":null,\"place\":null,\"contributors\":null,\"retweet_count\":0,\"entities\":{\"hashtags\":[],\"urls\":[],\"user_mentions\":[{\"screen_name\":\"benj_fry\",\"name\":\"Benjamin Fry\",\"id\":38293796,\"id_str\":\"38293796\",\"indices\":[0,9]}]},\"favorited\":false,\"retweeted\":false}\""). 

extract_tweet_id_test()->
    Expected=268402405782196224,
    Expected=get_tweet_id(?TEST_TWEET).
    
extract_chunk_test()->
    Expected="268402405782196224",
    Expected=extract_chunk(?TEST_TWEET, "\"id\":", ",").
    
extract_tweet_test()->
    Expected="@benj_fry yeah, it's everywhere over here...crazy cognitive dissonance for a westerner.",
    Expected=extract_tweet_body(?TEST_TWEET).


-endif.
