-define(DEBUG, false).
-define(DBG_INFO(Str, Arr), case ?DEBUG of true -> io:format(Str,Arr); _ -> (null) end).
-define(DBG_INFO(Str), case ?DEBUG of true -> io:format(Str); _ -> (null) end).
