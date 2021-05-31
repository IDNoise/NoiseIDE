%% @author: Yaroslav 'IDNoise' Nikityshev
%% @date: 09.09.2012

-module(test_cache_module_1). 

-export([ 
    member/2
]).    

-spec member(Elem, List) -> boolean() when
      Elem :: T,
      List :: [T],
      T :: term().  
  
member(_, _) ->   
    erlang:nif_error(undef).     