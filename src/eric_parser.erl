-module(eric_parser).
-export([parse/1]).
-include("eric.hrl").

%% Based in https://github.com/gdamjan/erlang-irc-bot/blob/master/src/ircbot_lib.erl
parse(Line) when is_list(Line) ->
  parse(list_to_binary(Line));

parse(<<":", Line/binary>>) ->
  [Prefix | Rest] = re:split(Line, " ", [{parts, 2}]),
  [Nick | User] = re:split(Prefix, "[!@]", [{parts, 2}]),
  parse_cmd(Rest, [Nick, User]);

parse(Line) ->
  parse_cmd(Line, [<<>>, <<>>]).

parse_cmd(Line, Acc) ->
  [Front | Trailing] = re:split(Line, " :", [{parts, 2}]),
  Parts = if length(Trailing) == 0 -> 16; true -> 15 end,
  [Command | Params] = re:split(Front, " ", [{parts, Parts}]),
  {response, Acc ++ [cmd_to_atom(Command)] ++ Params ++ [strip_crlf(Trailing)]}.

strip_crlf(Text) ->
  re:replace(Text, "\r\n", "", [{return, binary}]).

cmd_to_atom(<<"200">>) -> rpl_tracelink;
cmd_to_atom(<<"201">>) -> rpl_traceconnecting;
cmd_to_atom(<<"202">>) -> rpl_traceandshake;
cmd_to_atom(<<"203">>) -> rpl_traceunknown;
cmd_to_atom(<<"204">>) -> rpl_traceoperator;
cmd_to_atom(<<"205">>) -> rpl_traceuser;
cmd_to_atom(<<"206">>) -> rpl_traceserver;
cmd_to_atom(<<"208">>) -> rpl_tracenewtype;
cmd_to_atom(<<"211">>) -> rpl_statslinkinfo;
cmd_to_atom(<<"212">>) -> rpl_statscommands;
cmd_to_atom(<<"213">>) -> rpl_statscline;
cmd_to_atom(<<"214">>) -> rpl_statsnline;
cmd_to_atom(<<"215">>) -> rpl_statsiline;
cmd_to_atom(<<"216">>) -> rpl_statskline;
cmd_to_atom(<<"218">>) -> rpl_statsyline;
cmd_to_atom(<<"219">>) -> rpl_endofstats;
cmd_to_atom(<<"221">>) -> rpl_umodeis;
cmd_to_atom(<<"235">>) -> rpl_servlistend;
cmd_to_atom(<<"241">>) -> rpl_statslline;
cmd_to_atom(<<"242">>) -> rpl_statsuptime;
cmd_to_atom(<<"243">>) -> rpl_statsoline;
cmd_to_atom(<<"244">>) -> rpl_statshline;
cmd_to_atom(<<"251">>) -> rpl_luserclient;
cmd_to_atom(<<"252">>) -> rpl_luserop;
cmd_to_atom(<<"253">>) -> rpl_luserunknow;
cmd_to_atom(<<"254">>) -> rpl_luserchannels;
cmd_to_atom(<<"255">>) -> rpl_luserme;
cmd_to_atom(<<"256">>) -> rpl_ladminme;
cmd_to_atom(<<"259">>) -> rpl_adminemail;
cmd_to_atom(<<"261">>) -> rpl_tracelog;
cmd_to_atom(<<"300">>) -> rpl_none;
cmd_to_atom(<<"301">>) -> rpl_away;
cmd_to_atom(<<"302">>) -> rpl_userhost;
cmd_to_atom(<<"303">>) -> rpl_ison;
cmd_to_atom(<<"305">>) -> rpl_unaway;
cmd_to_atom(<<"306">>) -> rpl_nowaway;
cmd_to_atom(<<"311">>) -> rpl_whoisuser;
cmd_to_atom(<<"312">>) -> rpl_whoisserver;
cmd_to_atom(<<"313">>) -> rpl_whoisoperator;
cmd_to_atom(<<"314">>) -> rpl_whowasuser;
cmd_to_atom(<<"315">>) -> rpl_endofwho;
cmd_to_atom(<<"317">>) -> rpl_whoisidle;
cmd_to_atom(<<"318">>) -> rpl_endofwhois;
cmd_to_atom(<<"319">>) -> rpl_whoischannels;
cmd_to_atom(<<"321">>) -> rpl_liststart;
cmd_to_atom(<<"322">>) -> rpl_list;
cmd_to_atom(<<"323">>) -> rpl_listend;
cmd_to_atom(<<"324">>) -> rpl_channelmodeis;
cmd_to_atom(<<"331">>) -> rpl_notopic;
cmd_to_atom(<<"332">>) -> rpl_topic;
cmd_to_atom(<<"341">>) -> rpl_inviting;
cmd_to_atom(<<"342">>) -> rpl_summoning;
cmd_to_atom(<<"351">>) -> rpl_version;
cmd_to_atom(<<"352">>) -> rpl_whoreply;
cmd_to_atom(<<"353">>) -> rpl_namreply;
cmd_to_atom(<<"364">>) -> rpl_links;
cmd_to_atom(<<"365">>) -> rpl_endoflinks;
cmd_to_atom(<<"366">>) -> rpl_endofnames;
cmd_to_atom(<<"367">>) -> rpl_banlist;
cmd_to_atom(<<"368">>) -> rpl_endofbandlist;
cmd_to_atom(<<"369">>) -> rpl_endofwhowas;
cmd_to_atom(<<"371">>) -> rpl_info;
cmd_to_atom(<<"372">>) -> rpl_motd;
cmd_to_atom(<<"374">>) -> rpl_endofinfo;
cmd_to_atom(<<"375">>) -> rpl_motdstart;
cmd_to_atom(<<"376">>) -> rpl_endofmotd;
cmd_to_atom(<<"381">>) -> rpl_youreoper;
cmd_to_atom(<<"382">>) -> rpl_rehashing;
cmd_to_atom(<<"391">>) -> rpl_time;
cmd_to_atom(<<"392">>) -> rpl_userstart;
cmd_to_atom(<<"393">>) -> rpl_users;
cmd_to_atom(<<"394">>) -> rpl_endofusers;
cmd_to_atom(<<"395">>) -> rpl_nousers;
cmd_to_atom(<<"401">>) -> err_nosuchnick;
cmd_to_atom(<<"402">>) -> err_nosuchserver;
cmd_to_atom(<<"403">>) -> err_nosuchchannel;
cmd_to_atom(<<"404">>) -> err_cannotsendtochan;
cmd_to_atom(<<"405">>) -> err_toomanychannels;
cmd_to_atom(<<"406">>) -> err_wasnosuchnick;
cmd_to_atom(<<"407">>) -> err_toomanytargets;
cmd_to_atom(<<"409">>) -> err_noorigin;
cmd_to_atom(<<"411">>) -> err_norecipient;
cmd_to_atom(<<"412">>) -> err_notexttosend;
cmd_to_atom(<<"413">>) -> err_notoplevel;
cmd_to_atom(<<"414">>) -> err_wildtoplevel;
cmd_to_atom(<<"421">>) -> err_unknowncommand;
cmd_to_atom(<<"422">>) -> err_nomotd;
cmd_to_atom(<<"423">>) -> err_noadmininfo;
cmd_to_atom(<<"424">>) -> err_fileerror;
cmd_to_atom(<<"431">>) -> err_nonicknamegiven;
cmd_to_atom(<<"432">>) -> err_erroneusnickname;
cmd_to_atom(<<"433">>) -> err_nicknameinuse;
cmd_to_atom(<<"436">>) -> err_nickcollision;
cmd_to_atom(<<"441">>) -> err_usernotinchannel;
cmd_to_atom(<<"442">>) -> err_notonchannel;
cmd_to_atom(<<"443">>) -> err_useronchannel;
cmd_to_atom(<<"444">>) -> err_nologin;
cmd_to_atom(<<"445">>) -> err_summondisabled;
cmd_to_atom(<<"446">>) -> err_usersdisabled;
cmd_to_atom(<<"451">>) -> err_notregistered;
cmd_to_atom(<<"461">>) -> err_needmoreparams;
cmd_to_atom(<<"462">>) -> err_alreadyregistred;
cmd_to_atom(<<"463">>) -> err_nopermforhost;
cmd_to_atom(<<"464">>) -> err_passwdmismatch;
cmd_to_atom(<<"465">>) -> err_yourebannedcreep;
cmd_to_atom(<<"467">>) -> err_keyset;
cmd_to_atom(<<"471">>) -> err_channelisfull;
cmd_to_atom(<<"472">>) -> err_unknownmode;
cmd_to_atom(<<"473">>) -> err_inviteonlychan;
cmd_to_atom(<<"474">>) -> err_bannedfromchan;
cmd_to_atom(<<"475">>) -> err_badchannelkey;
cmd_to_atom(<<"481">>) -> err_noprivileges;
cmd_to_atom(<<"482">>) -> err_chanoprivsneeded;
cmd_to_atom(<<"483">>) -> err_cantkillserver;
cmd_to_atom(<<"491">>) -> err_nooperhost;
cmd_to_atom(<<"492">>) -> err_noservicehost;
cmd_to_atom(<<"501">>) -> err_umodeunknownflag;
cmd_to_atom(<<"502">>) -> err_usersdontmatch;
cmd_to_atom(<<"NOTICE">>) -> notice;
cmd_to_atom(<<"JOIN">>) -> join;
cmd_to_atom(<<"PING">>) -> ping;
cmd_to_atom(<<"PONG">>) -> pong;
cmd_to_atom(<<"PRIVMSG">>) -> privmsg;
cmd_to_atom(<<"MODE">>) -> mode;
cmd_to_atom(<<"NICK">>) -> nick;
cmd_to_atom(Cmd) -> Cmd.
