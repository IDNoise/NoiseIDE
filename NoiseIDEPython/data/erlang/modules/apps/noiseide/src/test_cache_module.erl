%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 09.09.2012

-module(test_cache_module).

-export([
    xxxxx/1 
    %add/3 
]).     
 
 
 
-spec xxxxx(zzz:zzz()) -> atom();
       (yyy:yyy()) -> atom();
       ([X]) -> atom() when X :: atom();
       ([term()]) -> atom().
xxxxx(X) -> X.    
    
x() ->        
    xxxxx(1).   

   
  
 
zz() -> z.



%, 
    %add(1, 1, []).

%-spec add(Sizer :: integer(), Window :: integer(), OptionalArgs) -> integer()when
%    OptionalArgs :: [ { proportion, integer() } | { flag, integer() } | { border, integer() } ].
%add(_Sizer, _Window, _OptionalArgs) -> ok. 