{application, noiseide, [
    {description,"Description"},
    {vsn,"1.0"},
    {modules,[eide_cache,eide_compiler,eide_connect,mochijson2,noiseide_app,
          reloader]},  
    {registered,[]}, 
    {applications,[kernel,stdlib]},
    {mod,{noiseide_app,[]}} 
]}. 
   