{application, noiseide, [
    {description,"Description"},
    {vsn,"1.0"},
    {modules,[test_cache_module,reloader,noiseide_app,mochijson2,eide_connect,
          eide_compiler,eide_cache]},  
    {registered,[]}, 
    {applications,[kernel,stdlib]},
    {mod,{noiseide_app,[]}} 
]}. 
   