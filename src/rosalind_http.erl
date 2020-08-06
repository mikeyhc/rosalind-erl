-module(rosalind_http).

-export([get/2]).

get(Server, URI) ->
    {ok, ConnPid} = gun:open(Server, 443),
    {ok, _Protocol} = gun:await_up(ConnPid),
    Data = get_(ConnPid, URI),
    gun:close(ConnPid),
    Data.

get_(ConnPid, URI) ->
    StreamRef = gun:get(ConnPid, URI),
    case gun:await(ConnPid, StreamRef) of
        {response, _, 302, Headers} ->
            {_, Location} = lists:keyfind(<<"location">>, 1, Headers),
            get_(ConnPid, Location);
        {response, fin, _Status, _Headers} ->
            nodata;
        {response, nofin, _Status, _Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            Body
    end.
