Artplant NoiseIDE ([http://artplant.no](http://artplant.no), [https://github.com/artplant](https://github.com/artplant))
========
IDE for Erlang written from scratch using wxPython.

In Artplant we are using Erlang on server and Unity\C# on client. All work is done on windows, servers hosted on linux.

Most of the features we use are already in place, project is not in active development. From time to time i add something new.
Codebase requires a lot of refactorings i can't find time to do. I think it requires total rewrite :)

##Windows MSI Installer
Latest windows installer you can download from my [dropbox folder](https://www.dropbox.com/s/u1esqq4h68qufcz/NoiseIDE.msi) or from [latest](https://github.com/IDNoise/NoiseIDE/releases/latest) github release (don't update it as often as dropbox)

##Linux\MacOs
Never tried to run it on these platforms. Required calls to pywin32api are placed in special platform blocks. 
Everyone interested in running it on these platforms are welcome.

## Tutorials
Simple tutorials for first setup, basic project and common controls elements are in [wiki](https://github.com/IDNoise/NoiseIDE/wiki).

##Features
* Single app\multiple app erlang projects

  * Single app. 1 erlang application. Project folder contains default src\include\ebin folders. 
  * Multiple app. Can contain 2 separate forlders: apps and deps.
     * Apps folder contains erlang applications you are working on and deps 
     * Deps folder contains all dependencies, like mochiweb, gproc, etc. 
     * They are compliled separately. ( For example we have 20+ deps in our projects and compile it only on first project initialization and later on dependency update).

  * Apps and deps path can be empty, all apps will be in project root.
 

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
 * Resolve record (import)
 * Unfold record fields

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

## Used libs\apps
* Python 2.7
* wxPython 2.9.5
* PyYaml-3.*
* PyWin32-217
* cx_Freeze-4.3 for building exe\msi

###Licence
Artplant Noise IDE is licensed under GPL-2

## About
Developed as hobby project in free time to learn Python and get proper IDE for daily use.
You can contact me by mail (in profile) or by skype: *idnoise* 
