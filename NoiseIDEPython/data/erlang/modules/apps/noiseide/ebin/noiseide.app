{application, noiseide, [
    {description,"Description"},
    {vsn,"1.0"},
    {modules,[reloader,noiseide_app,noiseide_api,mochijson2,eide_connect,
          eide_compiler,eide_client_connect,eide_cache]},  
    {registered,[]}, 
    {applications,[kernel,stdlib]},
    {mod,{noiseide_app,[]}} 
]}. 
   