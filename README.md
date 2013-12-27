NoiseIDE
========
IDE for Erlang written from scratch using wxPython.
I'm a server programmer in ArtPlant. We are making games using Erlang as server and Unity\C# as client. All work is done on windows. 

##Windows MSI Installer
Latest windows installer you can download from my [dropbox folder](https://www.dropbox.com/s/u1esqq4h68qufcz/NoiseIDE.msi) or from [latest](https://github.com/IDNoise/NoiseIDE/releases/latest) github release

**NOTE**: Please, do NOT install v <= 0.733 to Program Files. I'm going to move all temp\cache files to AppData in next version.

##Linux\MacOs
Never tried to run it on these platforms. But wxPython is crossplatform and pywin32api calls placed in if\else blocks. 
Everyone interested in running it on these platforms are welcome.

##Features
* Single app\multiple app erlang projects

 * SIngle app. 1 erlang application. Project folder contains default src\include\ebin folders. 
 * Multiple app. Project contains 2 main folders: apps and deps. Apps folder contains erlang applications you are working on and deps folder contains all dependencies, like mochiweb, gproc, etc. Deps apps are compiled only by request. ( For example we have 20+ deps in our projects and compile it only on first project initialization and later on dependency update).

* Project tree explorer with customization:

 * File masks
 * Hide\Show specific folders
 * Show all files
 * Auto refresh (Manual refresh if you have problems with performance on old machines. We had memory leaks on old machines. Directory polling is simple dir tree walk with state saving. I don't use any platform specific libs for it) 
 * Create new dirs\files from templates(standard erlang templates and also user templates)
 * Ide\User commands support. (Special commands defined in configuration files that are executed in erlang (ide or project console) with placeholders for module\file\application
 * Cut\Copy\Delete\Rename. Auto module renaming in project by request.
 * Open folder in system explorer

* Erlang project configuration:

 * Selected erlang runtime
 * Erlang compiler options
 * Erlang consoles configuration: title, sname, stat command, cookie, shell params (f.e. -config etc/app.config)
 * Excluded dirs(ignore compilation, search, etc.)
 * Working dir for consoles

* Code editor:

 * Syntax highlighting
 * Code navigation to module\function\record\macros\include
 * Code completion for modules\functions\macros\vars\records\record fields\includes
 * Code completion help info and tooltips(in code editor): 
     * erlang docinfo\spec\comment\signature for function
     * record definition for record
     * macros definition for macros
 * Compilation on the fly
 * Brace match
 * Simple auto indent (increase\decrease indent based on previous line)
 * Highlight all occurrences of selected word
 * Line numbers
 * Folding
 * Fast marker navigation to error\warning\search entry (right panel)
 * Context menu: edit\copy\paste
 * Context menu: Find references based on word under cursor
 * Context menu: Compile with option (P\E\S erlang compilation flag to see resulting doc after all transformations. Good for parse_transform testing). Opens up addtional tab with result.
 * File change polling
 * Outline
 * Go to line
 * Incremental find in file
 * Toggle: auto close brackets
 * Toggle: place brackets around selected text
 * Toggle: show white spaces
 * Toggle: show eol
 * Multiline comment\uncomment
 * 2 color schemes (dark\white) for syntax highlighting. Now without user configuration
 * User defined snippets

* Error\Warings list with navigation to source

* XREF project check with results table and navigation to source

* Dializer support with results table and navigation to source. 
 * Set up home path
 * Set up base plt
 * Execute on project\application\module from project explorer

* Project consoles
 * Start\stop
 * Clear output
 * Execute command (code completion, syntax highlight)
 * Button with last 20 executed commands 
 * Ctrl-Up\Down to cycle between commands in text control
 * Navigation markers for error log output (crashes, throws)
 * Auto code reloading

* Save state for project(project tree state, opened files, masks, etc)

* Fast navigation to file by name

* Search and replace in files. Options: regexp, file masks, directory selection
 * Auto result update (on file change\add\delete)
 * Multiple result tabs

* Auto update (for windows)

* Client api for custom erlang tools:
 * Special connector module for use in your custom erlang tools. Module: noiseide_api. It's automatically loaded in each project console.
 * Current functions: goto_line, goto_file, goto_mfa. (we are using it in custom profiler for navigation to source file in editor)

* Editor tab manager:
 * Split tabs 
 * Float tab
 * Show in explorer tree
 * Rename
 * Close\All\Other

* Navigation history between files

* Last project list on start

* Dunno, may be forgot something :D

## Help and contacts.
I don't get payed for this project and work on it in my free time so if you'd like to thank me you can make a donation using [PayPal](https://www.paypal.com/cgi-bin/webscr?cmd=_donations&business=2TM8DYTDCBHWJ&lc=RU&item_name=Noise%20IDE%20%28ide%20for%20erlang%29&item_number=1&currency_code=USD&bn=PP%2dDonationsBF%3abtn_donateCC_LG%2egif%3aNonHosted). This way i can drink rum and spend more time working on it.

You can contact me by mail (in profile) or by skype: *idnoise* 


## Planned features
* Rebar project type
* Debugger
* Have something useful in mind? Contact me :D


## Used libs\apps
* Python 2.7
* wxPython 2.9.5
* PyYaml-3.*
* PyWin32-217
* cx_Freeze-4.3 for building exe\msi


###Licence
Noise IDE is licensed under GPL-2 ( Problem for you? Contact me :) )
