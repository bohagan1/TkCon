#!/bin/sh
# The wish executable needs to be Tk4.1+ \
exec wish "$0" ${1+"$@"}

#
## tkcon.tcl
## Tk Console Widget, part of the VerTcl system
##
## Based (loosely) off Brent Welch's Tcl Shell Widget
##
## Thanks especially to the following for bug reports & code ideas:
## Steven Wahl <steven@indra.com>
## Jan Nijtmans <nijtmans@nici.kun.nl>
## Crimmins < @umich.edu somewhere >
##
## Copyright 1995,1996 Jeffrey Hobbs.  All rights reserved.
## Initiated: Thu Aug 17 15:36:47 PDT 1995
##
## jhobbs@cs.uoregon.edu, http://www.cs.uoregon.edu/~jhobbs/
##
## source standard_disclaimer.tcl

if [catch {package require Tk 4.1}] {
  error "TkCon requires at least the stable version of tcl7.5/tk4.1"
}
package ifneeded Tk $tk_version {load {} Tk}

## warn - little helper proc to pop up a tk_dialog warning message
# ARGS:	msg	- message you want to display to user
##
proc warn { msg } {
  bell
  tk_dialog ._warning Warning $msg warning 0 OK
}

## tkConInit - inits tkCon
# ARGS:	root	- widget pathname of the tkCon console root
#	title	- title for the console root and main (.) windows
# Calls:	tkConInitUI
# Outputs:	errors found in tkCon resource file
##
proc tkConInit {} {
  global tkCon tcl_interactive tcl_platform env auto_path argv0 argc argv

  set tcl_interactive 1

  if [info exists tkCon(name)] {
    set title $tkCon(name)
  } else {
    tkConMainInit
    set title Main
  }

  array set tkCon {
    color,blink		yellow
    color,proc		darkgreen
    color,prompt	brown
    color,stdin		black
    color,stdout	blue
    color,stderr	red

    blinktime		500
    font		fixed
    history		32
    library		{}
    lightbrace		1
    lightcmd		0
    loadTk		0
    maineval		{}
    nontcl		0
    prompt1		{([file tail [pwd]]) [history nextid] % }
    prompt2		{[history nextid] cont > }
    rcfile		.tkconrc
    scrollypos		left
    showmultiple	1
    showmenu		1
    slaveeval		{}
    subhistory		1

    exec slave app {} appname {} apptype slave cmd {} cmdbuf {} cmdsave {}
    event 1 svnt 1 cols 80 rows 24 deadapp 0
    errorInfo		{}
    slavealias		{ tkcon warn }
    slaveprocs		{ alias clear dir dump lremove puts tclindex \
			      auto_execpath unknown unalias which }
    version	0.52
    root	.
  }

  ## If there appear to be children of '.', then make sure we use
  ## a disassociated toplevel.
  if [string compare {} [winfo children .]] {
    set tkCon(root) .tkcon
  }

  if [string compare unix $tcl_platform(platform)] {
    array set tkCon {
      font	{Courier 12 {}}
      rcfile	tkcon.cfg
    }
  }

  if [info exists env(HOME)] {
    set tkCon(rcfile) [file join $env(HOME) $tkCon(rcfile)]
  }

  ## Handle command line arguments before sourcing resource file to
  ## find if resource file is being specified (let other args pass).
  for {set i 0} {$i < $argc} {incr i} {
    if [string match \-rcfile [lindex $argv $i]] {
      set tkCon(rcfile) [lindex $argv [incr i]]
    }
  }

  if [file exists $tkCon(rcfile)] {
    set code [catch [list uplevel \#0 source $tkCon(rcfile)] err]
  }

  if [info exists env(TK_CON_LIBRARY)] {
    eval lappend auto_path $env(TK_CON_LIBRARY)
  } else {
    eval lappend auto_path $tkCon(library)
  }

  set dir [file dir [info nameofexec]]
  foreach dir [list $dir [file join [file dir $dir] lib]] {
    if [file exists [file join $dir pkgIndex.tcl]] {
      if {[lsearch -exact $auto_path $dir] < 0} {
	lappend auto_path $dir
      }
    }
  }

  foreach dir $auto_path {
    if [file exists [file join $dir pkgIndex.tcl]] {
      source [file join $dir pkgIndex.tcl]
    }
  }

  ## Handle rest of command line arguments after sourcing resource file
  ## and slave is created, but before initializing UI or setting packages.
  set slaveargs {}
  set slavefiles {}
  for {set i 0} {$i < $argc} {incr i} {
    set arg [lindex $argv $i]
    if [regexp -- {-.+} $arg] {
      ## Handle arg based options
      switch -- $arg {
	-rcfile		{ incr i }
	-maineval - -e -
	-eval		{ append tkCon(maineval) [lindex $argv [incr i]]\n }
	-slave - -slavescript -
	-slaveeval	{ append tkCon(slaveeval) [lindex $argv [incr i]]\n }
	-package - -pkg -
	-load		{ set tkCon(load[lindex $argv [incr i]]) 1 }
	-nontcl		{ set tkCon(nontcl) 0 }
	-root		{ set tkCon(root) [lindex $argv [incr i]] }
	default		{ lappend slaveargs $arg }
      }
    } elseif {[file isfile $arg]} {
      lappend slavefiles $arg
    } else {
      lappend slaveargs $arg
    }
  }

  ## Create slave executable
  if [string comp {} $tkCon(exec)] {
    eval tkConInitSlave $tkCon(exec) $slaveargs
  }

  tkConAttach $tkCon(appname) $tkCon(apptype)
  tkConInitUI $title

  ## Set up package info for the slave
  tkConCheckPackages

  ## Evaluate maineval in slave
  if {[string comp {} $tkCon(maineval)] &&
      [catch {uplevel \#0 $tkCon(maineval)} merr]} {
    puts stderr "error in eval:\n$merr"
  }

  ## Source extra command line argument files into slave executable
  foreach fn $slavefiles {
    puts -nonewline "slave sourcing $fn ... "
    if {[catch {tkConEvalSlave source $fn} fnerr]} {
      puts stderr "error:\n$fnerr"
    } else {
      puts "OK"
    }
  }

  interp alias {} ls {} dir
  #interp alias $tkCon(exec) clean {} tkConStateRevert tkCon
  #tkConStateCheckpoint tkCon

  ## Evaluate slaveeval in slave
  if {[string comp {} $tkCon(slaveeval)] &&
      [catch {interp eval $tkCon(exec) $tkCon(slaveeval)} serr]} {
    puts stderr "error in slave script:\n$serr"
  }
  ## Output any error/output that may have been returned from rcfile
  if {[info exists code] && [string comp {} $err]} {
    if $code {
      puts stderr "error in $tkCon(rcfile):\n$err"
    } else {
      puts stdout "returned from $tkCon(rcfile):\n$err"
    }
  }
}

## tkConInitSlave - inits the slave by placing key procs and aliases in it
## It's arg[cv] are based on passed in options, while argv0 is the same as
## the master.  tcl_interactive is the same as the master as well.
# ARGS:	slave	- name of slave to init.  If it does not exist, it is created.
#	args	- args to pass to a slave as argv/argc
##
proc tkConInitSlave {slave args} {
  global tkCon argv0 tcl_interactive
  if [string match {} $slave] {
    error "Don't init the master interpreter, goofball"
  }
  if ![interp exists $slave] { interp create $slave }
  if {[string match {} [$slave eval info command tcl_puts]]} {
    interp eval $slave rename puts tcl_puts
  }
  foreach cmd $tkCon(slaveprocs) { interp eval $slave [dump proc $cmd] }
  foreach cmd $tkCon(slavealias) { interp alias $slave $cmd {} $cmd }
  interp alias $slave ls $slave dir
  interp eval $slave set tcl_interactive $tcl_interactive \; \
      set argv0 [list $argv0] \; set argc [llength $args] \; \
      set argv  [list $args] \; history keep $tkCon(history)
  
  foreach pkg [lremove [package names] Tcl] {
    foreach v [package versions $pkg] {
      interp eval $slave [list package ifneeded $pkg $v \
			      [package ifneeded $pkg $v]]
    }
  }
}

## tkConInitUI - inits UI portion (console) of tkCon
## Creates all elements of the console window and sets up the text tags
# ARGS:	root	- widget pathname of the tkCon console root
#	title	- title for the console root and main (.) windows
# Calls:	tkConInitMenus, tkConPrompt
##
proc tkConInitUI {title} {
  global tkCon

  set root $tkCon(root)
  if [string match . $root] { set w {} } else { set w [toplevel $root] }
  set tkCon(base) $w
  wm withdraw $root

  set tkCon(menubar) [frame $w.mbar -relief raised -bd 2]
  set tkCon(console) [text $w.text -font $tkCon(font) -wrap char \
      -yscrollcommand "$w.sy set" -setgrid 1 -foreground $tkCon(color,stdin)]
  bindtags $w.text "$w.text PreCon Console PostCon $root all"
  set tkCon(scrolly) [scrollbar $w.sy -takefocus 0 -bd 1 \
			  -command "$w.text yview"]

  tkConInitMenus $tkCon(menubar)

  if $tkCon(showmenu) { pack $tkCon(menubar) -fill x }
  pack $tkCon(scrolly) -side $tkCon(scrollypos) -fill y
  pack $tkCon(console) -fill both -expand 1

  tkConPrompt "$title console display active\n"

  foreach col {prompt stdout stderr stdin proc} {
    $w.text tag configure $col -foreground $tkCon(color,$col)
  }
  $w.text tag configure blink -background $tkCon(color,blink)

  bind $w.text <Configure> {
    scan [wm geometry [winfo toplevel %W]] "%%dx%%d" tkCon(cols) tkCon(rows)
  }

  wm title $root "tkCon $tkCon(version) $title"
  wm deiconify $root
  focus $w.text
}

## tkConEval - evaluates commands input into console window
## This is the first stage of the evaluating commands in the console.
## They need to be broken up into consituent commands (by tkConCmdSep) in
## case a multiple commands were pasted in, then each is eval'ed (by
## tkConEvalCmd) in turn.  Any uncompleted command will not be eval'ed.
# ARGS:	w	- console text widget
# Calls:	tkConCmdGet, tkConCmdSep, tkConEvalCmd
## 
proc tkConEval {w} {
  global tkCon
  tkConCmdSep [tkConCmdGet $w] cmds tkCon(cmd)
  $w mark set insert end-1c
  $w insert end \n
  if [llength $cmds] {
    foreach cmd $cmds {tkConEvalCmd $w $cmd}
    $w insert insert $tkCon(cmd) {}
  } elseif {[info complete $tkCon(cmd)] && ![regexp {[^\\]\\$} $tkCon(cmd)]} {
    tkConEvalCmd $w $tkCon(cmd)
  }
  $w see insert
}

## tkConEvalCmd - evaluates a single command, adding it to history
# ARGS:	w	- console text widget
# 	cmd	- the command to evaluate
# Calls:	tkConPrompt
# Outputs:	result of command to stdout (or stderr if error occured)
# Returns:	next event number
## 
proc tkConEvalCmd {w cmd} {
  global tkCon
  $w mark set output end
  if [string comp {} $cmd] {
    set err 0
    if $tkCon(subhistory) {
      set ev [tkConEvalSlave history nextid]
      incr ev -1
      if {[string match !! $cmd]} {
	set err [catch {tkConEvalSlave history event $ev} cmd]
      } elseif [regexp {^!(.+)$} $cmd dummy event] {
	set err [catch {tkConEvalSlave history event $event} cmd]
      } elseif [regexp {^\^([^^]*)\^([^^]*)\^?$} $cmd dummy old new] {
	if ![set err [catch {tkConEvalSlave history event $ev} cmd]] {
	  regsub -all -- $old $cmd $new cmd
	}
      }
    }
    if $err {
      $w insert output $cmd\n stderr
    } else {
      if [string match {} $tkCon(appname)] {
	if [catch {tkConEvalSlave eval $cmd} res] {
	  set tkCon(errorInfo) [tkConEvalSlave set errorInfo]
	  set err 1
	}
      } else {
	if {$tkCon(nontcl) && [string match interp $tkCon(apptype)]} {
	  if [catch "tkConEvalSend $cmd" res] {
	    set tkCon(errorInfo) {Non-Tcl errorInfo not available}
	    set err 1
	  }
	} else {
	  if [catch [list tkConEvalAttached $cmd] res] {
	    set tkCon(errorInfo) [tkConEvalAttached set errorInfo]
	    set err 1
	  }
	}
      }
      tkConEvalSlave history add $cmd
      if $err {
	$w insert output $res\n stderr
      } elseif [string comp {} $res] {
	$w insert output $res\n stdout
      }
    }
  }
  tkConPrompt
  set tkCon(svnt) [set tkCon(event) [tkConEvalSlave history nextid]]
}

## tkConEvalSlave - evaluates the args in the associated slave
# ARGS:	args	- the command and args to evaluate
##
proc tkConEvalSlave args {
  global tkCon
  interp eval $tkCon(exec) $args
}

## tkConEvalSend - sends the args to the attached interpreter
## Varies from 'send' by determining whether attachment is dead
## when an error is received
# ARGS:	args	- the args to send across
# Returns:	the result of the command
##
proc tkConEvalSend args {
  global tkCon
  if $tkCon(deadapp) {
    if {[lsearch -exact [winfo interps] $tkCon(app)]<0} {
      return
    } else {
      set tkCon(appname) [string range $tkCon(appname) 5 end]
      set tkCon(deadapp) 0
      tkConPrompt "\n\"$tkCon(app)\" alive\n" [tkConCmdGet $tkCon(console)]
    }
  }
  set code [catch {eval send [list $tkCon(app)] $args} result]
  if {$code && [lsearch -exact [winfo interps] $tkCon(app)]<0} {
    ## Interpreter disappeared
    if [tk_dialog $tkCon(base).dead "Dead Attachment" \
	    "\"$tkCon(app)\" appears to have died.\nReturn to primary slave interpreter?" questhead 0 OK No] {
      set tkCon(appname) "DEAD:$tkCon(appname)"
      set tkCon(deadapp) 1
    } else {
      set err "Attached Tk interpreter \"$tkCon(app)\" died."
      tkConAttach {}
      tkConEvalSlave set errorInfo $err
    }
    tkConPrompt \n [tkConCmdGet $tkCon(console)]
  }
  return -code $code $result
}

## tkConCmdGet - gets the current command from the console widget
# ARGS:	w	- console text widget
# Returns:	text which compromises current command line
## 
proc tkConCmdGet w {
  if [string match {} [set ix [$w tag nextrange prompt limit end]]] {
    $w tag add stdin limit end-1c
    return [$w get limit end-1c]
  }
}

## tkConCmdSep - separates multiple commands into a list and remainder
# ARGS:	cmd	- (possible) multiple command to separate
# 	list	- varname for the list of commands that were separated.
#	rmd	- varname of any remainder (like an incomplete final command).
#		If there is only one command, it's placed in this var.
# Returns:	constituent command info in varnames specified by list & rmd.
## 
proc tkConCmdSep {cmd ls rmd} {
  upvar $ls cmds $rmd tmp
  set tmp {}
  set cmds {}
  foreach cmd [split [set cmd] \n] {
    if [string comp {} $tmp] {
      append tmp \n$cmd
    } else {
      append tmp $cmd
    }
    if {[info complete $tmp] && ![regexp {[^\\]\\$} $tmp]} {
      lappend cmds $tmp
      set tmp {}
    }
  }
  if {[string comp {} [lindex $cmds end]] && [string match {} $tmp]} {
    set tmp [lindex $cmds end]
    set cmds [lreplace $cmds end end]
  }
}

## tkConPrompt - displays the prompt in the console widget
# ARGS:	w	- console text widget
# Outputs:	prompt (specified in tkCon(prompt1)) to console
## 
proc tkConPrompt {{pre {}} {post {}}} {
  global tkCon
  set w $tkCon(console)
  if [string comp {} $pre] { $w insert end $pre stdout }
  set i [$w index end-1c]
  if [string comp {} $tkCon(appname)] {
    $w insert end ">$tkCon(appname)< " prompt
  }
  $w insert end [tkConEvalSlave subst $tkCon(prompt1)] prompt
  $w mark set output $i
  $w mark set limit insert
  $w mark gravity limit left
  if [string comp {} $post] { $w insert end $post stdin }
  $w see end
}

## tkConAbout - gives about info for tkCon
## 
proc tkConAbout {} {
  global tkCon
  tk_dialog $tkCon(base).about "About TkCon v$tkCon(version)" \
      "Jeffrey Hobbs, Copyright 1995-96\njhobbs@cs.uoregon.edu\
	\nhttp://www.cs.uoregon.edu/~jhobbs/" questhead 0 OK
}

## tkConHelp - gives help info for tkCon
## 
proc tkConHelp {} {
  global tkCon
  tk_dialog $tkCon(base).help "Help on TkCon v$tkCon(version)" \
      "Jeffrey Hobbs, jhobbs@cs.uoregon.edu\nHelp available at:\
      http://www.cs.uoregon.edu/~jhobbs/work/tkcon/" questhead 0 OK
}

## tkConInitMenus - inits the menus for the console
# ARGS:	w	- console text widget
## 
proc tkConInitMenus w {
  global tkCon

  pack [menubutton $w.con  -text Console  -un 0 -menu $w.con.m] -side left
  pack [menubutton $w.edit -text Edit     -un 0 -menu $w.edit.m] -side left
  #pack [menubutton $w.insp -text Inspect  -un 0 -menu $w.insp.m] -side left
  pack [menubutton $w.pkgs -text Packages -un 0 -menu $w.pkgs.m] -side left
  pack [menubutton $w.pref -text Prefs    -un 0 -menu $w.pref.m] -side left
  pack [menubutton $w.help -text Help     -un 0 -menu $w.help.m] -side right

  menu $w.pop -tearoff 0
  $w.pop add cascade -label Console  -un 0 -menu $w.pop.con
  $w.pop add cascade -label Edit     -un 0 -menu $w.pop.edit
  #$w.pop add cascade -label Inspect  -un 0 -menu $w.pop.insp
  $w.pop add cascade -label Packages -un 0 -menu $w.pop.pkgs
  $w.pop add cascade -label Prefs    -un 0 -menu $w.pop.pref
  $w.pop add cascade -label Help     -un 0 -menu $w.pop.help
  bind [winfo toplevel $w] <Button-3> "tk_popup $w.pop %X %Y"

  ## Console Menu
  ##
  foreach m [list [menu $w.con.m] [menu $w.pop.con]] {
    $m add command -label "New Console" -un 0 -acc Ctrl-N -com tkConNew
    $m add command -label "Close Console " -un 0 -acc Ctrl-w -com tkConDestroy
    $m add separator
    $m add cascade -label "Attach Console " -un 0 -menu $m.apps
    $m add separator
    $m add command -label Quit -un 0 -acc Ctrl-q -command exit

    ## Attach Console Menu
    ##
    menu $m.apps -disabledforeground $tkCon(color,prompt) \
	-postcommand "tkConFillAppsMenu $m.apps"
  }

  ## Edit Menu
  ##
  set text $tkCon(console)
  foreach m [list [menu $w.edit.m] [menu $w.pop.edit]] {
    $m add command -label Cut   -un 1 -acc Ctrl-x -command "tkConCut $text"
    $m add command -label Copy  -un 1 -acc Ctrl-c -command "tkConCopy $text"
    $m add command -label Paste -un 0 -acc Ctrl-v -command "tkConPaste $text"
  }

  ## Inspect Menu
  ## Currently disabled
  foreach m {} {
    $m add command -label Procedures       -command "tkConInspect procs"
    $m add command -label "Global Vars"    -command "tkConInspect vars"
    $m add command -label Interpreters     -command "tkConInspect interps"
    $m add command -label Aliases          -command "tkConInspect aliases"
    $m add command -label Images           -command "tkConInspect images"
    $m add command -label "All Widgets"    -command "tkConInspect widgets"
    $m add command -label "Canvas Widgets" -command "tkConInspect canvases"
    $m add command -label "Menu Widgets"   -command "tkConInspect menus"
    $m add command -label "Text Widgets"   -command "tkConInspect texts"
  }

  ## Packages Menu
  ##
  menu $w.pkgs.m -disabledforeground $tkCon(color,prompt) \
      -postcommand "tkConCheckPackages $w.pkgs.m"
  menu $w.pop.pkgs -disabledforeground $tkCon(color,prompt) \
      -postcommand "tkConCheckPackages $w.pop.pkgs"

  ## Prefs Menu
  ##
  foreach m [list [menu $w.pref.m] [menu $w.pop.pref]] {
    $m add checkbutton -label "Brace Highlighting"    -var tkCon(lightbrace)
    $m add checkbutton -label "Command Highlighting"  -var tkCon(lightcmd)
    $m add checkbutton -label "History Substitution"  -var tkCon(subhistory)
    $m add checkbutton -label "Non-Tcl Attachments"   -var tkCon(nontcl)
    $m add checkbutton -label "Show Multiple Matches" -var tkCon(showmultiple)
    $m add checkbutton -label "Show Menubar"	      -var tkCon(showmenu) \
	-command "if \$tkCon(showmenu) { 
			pack $w -fill x -before $tkCon(scrolly)
		  } else { pack forget $w }"
    $m add cascade -label Scrollbar -un 0 -menu $m.scroll

    ## Scrollbar Menu
    ##
    set m [menu $m.scroll -tearoff 0]
    $m add radio -label Left -var tkCon(scrollypos) -value left -command {
      pack config $tkCon(scrolly) -side left
    }
    $m add radio -label Right -var tkCon(scrollypos) -value right -command {
      pack config $tkCon(scrolly) -side right
    }
  }

  ## Help Menu
  ##
  foreach m [list [menu $w.help.m] [menu $w.pop.help]] {
    $m add command -label "About " -un 0 -acc Ctrl-A -command tkConAbout
    $m add separator
    $m add command -label Help -un 0 -acc Ctrl-H -command tkConHelp
  }

  ## It's OK to bind to all because it's specific to each interpreter
  bind all <Control-q> exit
  bind all <Control-N> tkConNew
  bind all <Control-w> tkConDestroy
  bind all <Control-A> tkConAbout
  bind all <Control-H> tkConHelp
  bind all <Control-Key-1> {
    tkConAttach {}
    tkConPrompt \n [tkConCmdGet $tkCon(console)]
  }
  bind all <Control-Key-2> {
    if [string comp {} $tkCon(name)] {
      tkConAttach $tkCon(name)
    } else {
      tkConAttach Main
    }
    tkConPrompt \n [tkConCmdGet $tkCon(console)]
  }
  bind all <Control-Key-3> {
    tkConAttach Main
    tkConPrompt \n [tkConCmdGet $tkCon(console)]
  }
}

## tkConCheckPackages - checks which packages are currently loaded
## Requires two loops to make sure that packages which auto-load Tk
## set everything properly.
# ARGS:	w	- menu name
##
proc tkConCheckPackages {{w {}}} {
  global tkCon
  foreach pkg [lsort [lremove [package names] Tcl]] {
    if {![info exists tkCon(load$pkg)]} { set tkCon(load$pkg) 0 }
    if {$tkCon(load$pkg)==1} {
      if [catch {tkConEvalSlave package require $pkg}] {
	bgerror "$pkg cannot be loaded.  Check your pkgIndex.tcl file!!!"
	set tkCon(load$pkg) -1
      }
    }
  }
  if [string comp {} [tkConEvalSlave info commands .]] { set tkCon(loadTk) 1 }
  if ![winfo exists $w] return
  $w delete 0 end
  foreach pkg [lsort [lremove [package names] Tcl]] {
    if {$tkCon(load$pkg)==-1} {
      $w add command -label "$pkg Load Failed" -state disabled
    } elseif $tkCon(load$pkg) {
      $w add command -label "$pkg Loaded" -state disabled
    } else {
      $w add checkbutton -label "Load $pkg" -var tkCon(load$pkg) \
	  -command "tkConCheckPackages"
    }
  }
}

## tkConFillAppsMenu - fill in  in the applications sub-menu
## with a list of all the applications that currently exist.
##
proc tkConFillAppsMenu {m} {
  global tkCon

  set self	[tk appname]
  set masters	[tkConMain set tkCon(interps)]
  set masternm	[tkConSlave]
  foreach i $masternm {
    if [tkConSlave $i set tkCon(loadTk)] {
      lappend slaves [tkConSlave $i tkConEvalSlave tk appname]
    } else {
      lappend slaves "no Tk"
    }
  }
  set path [concat $tkCon(name) $tkCon(exec)]
  set tmp [tkConInterps]
  array set interps $tmp
  array set tknames [concat [lrange $tmp 1 end] [list [lindex $tmp 0]]]

  catch {$m delete 0 last}
  set cmd {tkConPrompt \n [tkConCmdGet $tkCon(console)]}
  $m add radio -label {None (use local slave) } -var tkCon(app) -value $path \
      -command "tkConAttach {}; $cmd" -acc Ctrl-1
  $m add separator
  $m add command -label "Foreign Tk Interpreters" -state disabled
  foreach i [lsort [lremove [winfo interps] \
			[concat $masters $slaves [array names tknames]]]] {
    $m add radio -label $i -var tkCon(app) -value $i \
	-command "tkConAttach [list $i] interp; $cmd"
  }
  $m add separator

  $m add command -label "TkCon Interpreters" -state disabled
  foreach i [lsort [array names interps]] {
    if [string match {} $interps($i)] { set interps($i) "no Tk" }
    if [regexp {^Slave[0-9]+} $i] {
      if [string comp $tkCon(name) $i] {
	$m add radio -label "$i ($interps($i))" -var tkCon(app) -value $i \
	    -command "tkConAttach [list $i] slave; $cmd"
      } else {
	$m add radio -label "$i ($interps($i))" -var tkCon(app) -value $i \
	    -acc Ctrl-2 \
	    -command "tkConAttach [list $i] slave; $cmd"
      }
    } else {
      set name [concat Main $i]
      if [string match Main $name] {
	$m add radio -label "$name ($interps($i))" -var tkCon(app) \
	    -value Main -acc Ctrl-3 \
	    -command "tkConAttach [list $name] slave; $cmd"
      } else {
	$m add radio -label "$name ($interps($i))" -var tkCon(app) -value $i \
	     -command "tkConAttach [list $name] slave; $cmd"
      }
    }
  }
}

## tkConAttach - called to attach tkCon to an interpreter
# ARGS:	an	- application name to which tkCon sends commands
#		  This is either a slave interperter name or tk appname.
#	type	- (slave|interp) type of interpreter we're attaching to
#		  slave means it's a TkCon interpreter
#		  interp means we'll need to 'send' to it.
# Results:	tkConEvalAttached is recreated to evaluate in the
#		appropriate interpreter
##
proc tkConAttach {an {type slave}} {
  global tkCon
  set app -
  set path [concat $tkCon(name) $tkCon(exec)]
  if [string comp {} $an] {
    array set interps [tkConInterps]
    if [string match {[Mm]ain} [lindex $an 0]] { set an [lrange $an 1 end] }
    if {[string match $path $an]} {
      set an {}
      set app $path
      set type slave
    } elseif {[info exists interps($an)]} {
      if [string match {} $an] { set an Main; set app Main }
      set type slave
    } elseif {[interp exists $an]} {
      set an [concat $tkCon(name) $an]
      set type slave
    } elseif {[interp exists [concat $tkCon(exec) $an]]} {
      set an [concat $path $an]
      set type slave
    } elseif {[lsearch [winfo interps] $an] > -1} {
      if {$tkCon(loadTk) && [string match $an [tkConEvalSlave tk appname]]} {
	set an {}
	set app $path
	set type slave
      } elseif {[set i [lsearch [tkConMain set tkCon(interps)] $an]] > -1} {
	set an [lindex [tkConMain set tkCon(slaves)] $i]
	if [string match {[Mm]ain} $an] { set app Main }
	set type slave
      } else {
	set type interp
      }
    } else {
      error "No known interpreter \"$an\""
    }
  } else {
    set app $path
  }
  if [string match - $app] { set app $an }
  set tkCon(app)     $app
  set tkCon(appname) $an
  set tkCon(apptype) $type

  ## tkConEvalAttached - evaluates the args in the attached interpreter
  ## This procedure is dynamic.  It is rewritten by the proc tkConAttach
  ## to ensure it evals in the right place.
  # ARGS:	args	- the command and args to evaluate
  ##
  switch $type {
    slave {
      if [string match {} $an] {
	interp alias {} tkConEvalAttached {} tkConEvalSlave
      } elseif [string match Main $tkCon(app)] {
	interp alias {} tkConEvalAttached {} tkConMain eval
      } elseif [string match $tkCon(name) $tkCon(app)] {
	interp alias {} tkConEvalAttached {} uplevel \#0
      } else {
	interp alias {} tkConEvalAttached {} tkConMain interp eval $tkCon(app)
      }
    }
    interp {
      if $tkCon(nontcl) {
	interp alias {} tkConEvalAttached {} tkConEvalSlave
      } else {
	interp alias {} tkConEvalAttached {} tkConEvalSend
      }
    }
    default { error "[lindex [info level 0] 0] did not specify type" }
  }
  return
}

## tkConLoad - sources a file into the console
# ARGS:	fn	- (optional) filename to source in
# Returns:	selected filename ({} if nothing was selected)
## 
proc tkConLoad {{fn {}}} {
  global tkCon
  if {[string match {} $fn] &&
      ([catch {tkFileSelect} fn] || [string match {} $fn])} return
  tkConEvalAttached source $fn
}

## tkConSave - saves the console buffer to a file
## This does not eval in a slave because it's not necessary
# ARGS:	w	- console text widget
# 	fn	- (optional) filename to save to
## 
proc tkConSave {{fn {}}} {
  global tkCon
  if {[string match {} $fn] &&
      ([catch {tkFileSelect} fn] || [string match {} $fn])} return
  if [catch {open $fn w} fid] {
    error "Save Error: Unable to open '$fn' for writing\n$fid"
  }
  puts $fid [$tkCon(console) get 1.0 end-1c]
  close $fid
}

## tkConResource - re'source's this script into current console
## Meant primarily for my development of this program.  It's seems loopy
## due to quirks in Tcl on windows.
## 
set tkCon(SCRIPT) [info script]
if [string match relative [file pathtype [info script]]] {
  set tkCon(SCRIPT) [file join [pwd] [info script]]
}
set tkCon(SCRIPT) [eval file join [file split $tkCon(SCRIPT)]]
proc tkConResource {} "uplevel \#0 [list source $tkCon(SCRIPT)]; return"

## tkConMainInit
## This is only called for the main interpreter to include certain procs
## that we don't want to include (or rather, just alias) in slave interps.
##
proc tkConMainInit {} {
  global tkCon

  if ![info exists tkCon(slaves)] {
    array set tkCon [list slave 0 slaves Main name {} interps [tk appname]]
  }
  interp alias {} tkConMain {} tkConInterpEval Main
  interp alias {} tkConSlave {} tkConInterpEval

  ## tkConNew - create new console window
  ## Creates a slave interpreter and sources in this script.
  ## All other interpreters also get a command to eval function in the
  ## new interpreter.
  ## 
  proc tkConNew {} {
    global argv0 argc argv tkCon
    set tmp [interp create Slave[incr tkCon(slave)]]
    lappend tkCon(slaves) $tmp
    load {} Tk $tmp
    lappend tkCon(interps) [$tmp eval [list tk appname "[tk appname] $tmp"]]
    $tmp eval set argc $argc \; set argv [list $argv] \; \
	set argv0 [list $argv0]
    $tmp eval [list set tkCon(name) $tmp]
    $tmp eval [list source $tkCon(SCRIPT)]
    $tmp alias tkConDestroy tkConDestroy $tmp
    $tmp alias tkConNew	    tkConNew
    $tmp alias tkConMain    tkConInterpEval Main
    $tmp alias tkConSlave   tkConInterpEval
    $tmp alias tkConInterps tkConInterps
    return $tmp
  }

  ## tkConDestroy - destroy console window
  ## This proc should only be called by the main interpreter.  If it is
  ## called from there, it will ask before exiting TkCon.  All others
  ## (slaves) will just have their slave interpreter deleted, closing them.
  ## 
  proc tkConDestroy {{slave {}}} {
    global tkCon
    if [string match {} $slave] {
      ## Main interpreter close request
      if [tk_dialog $tkCon(base).destroyme {Quit TkCon?} \
	      {Closing the Main console will quit TkCon} \
	      warning 0 "Don't Quit" "Quit TkCon"] exit
    } else {
      ## Slave interpreter close request
      set name [tkConInterpEval $slave]
      set tkCon(interps) [lremove $tkCon(interps) [list $name]]
      set tkCon(slaves)  [lremove $tkCon(slaves) [list $slave]]
      interp delete $slave
    }
  }

  ## tkConInterpEval - passes evaluation to another named interpreter
  ## If the interpreter is named, but no args are given, it returns the
  ## [tk appname] of that interps master (not the associated eval slave).
  ##
  proc tkConInterpEval {{slave {}} args} {
    if [string match {} $slave] {
      global tkCon
      return $tkCon(slaves)
    } elseif [string match {[Mm]ain} $slave] {
      set slave {}
    }
    if [string match {} $args] {
      return [interp eval $slave tk appname]
    } else {
      uplevel \#0 [list interp eval $slave $args]
    }
  }

  proc tkConInterps {{ls {}} {interp {}}} {
    if [string match {} $interp] { lappend ls {} [list [tk appname]] }
    foreach i [interp slaves $interp] {
      if [string comp {} $interp] { set i "$interp $i" }
      if [catch "interp eval [list $i] tk appname" name] {
	lappend ls $i {}
      } else {
	lappend ls $i $name
      }
      set ls [tkConInterps $ls $i]
    }
    return $ls
  }
}


## tkConStateCheckpoint - checkpoints the current state of the system
## This allows you to return to this state with tkConStateRevert
# ARGS:	ary	- an array into which several elements are stored:
#			commands  - the currently defined commands
#			variables - the current global vars
#		This is the array you would pass to tkConRevertState
##
proc tkConStateCheckpoint {ary} {
  global tkCon
  upvar $ary a
  set a(commands)  [tkConEvalAttached info commands *]
  set a(variables) [tkConEvalAttached info vars *]
  return
}

## tkConStateCompare - compare two states and output difference
# ARGS:	ary1	- an array with checkpointed state
#	ary2	- a second array with checkpointed state
# Outputs:
##
proc tkConStateCompare {ary1 ary2} {
  upvar $ary1 a1 $ary2 a2
  puts "Commands unique to $ary1:\n[lremove $a1(commands) $a2(commands)]"
  puts "Commands unique to $ary2:\n[lremove $a2(commands) $a1(commands)]"
  puts "Variables unique to $ary1:\n[lremove $a1(variables) $a2(variables)]"
  puts "Variables unique to $ary2:\n[lremove $a2(variables) $a1(variables)]"
}

## tkConStateRevert - reverts interpreter to a previous state
# ARGS:	ary	- an array with checkpointed state
##
proc tkConStateRevert {ary} {
  upvar $ary a
  foreach i [lremove [tkConEvalAttached info commands *] $a(commands)] {
    catch "tkConEvalAttached rename $i {}"
  }
  foreach i [lremove [tkConEvalAttached info vars *] $a(variables)] {
    catch "tkConEvalAttached unset $i"
  }
}


## tkcon - command that allows control over the console
# ARGS:	totally variable, see internal comments
## 
proc tkcon {args} {
  global tkCon
  switch -- [lindex $args 0] {
    close {
      ## Closes the console
      tkConDestroy
    }
    clean {
      ## 'cleans' the interpreter - reverting to original tkCon state
      ## FIX
      ## tkConStateRevert tkCon
    }
    console {
      ## Passes the args to the text widget of the console.
      eval $tkCon(console) [lreplace $args 0 0]
    }
    error {
      ## Outputs stack caused by last error.
      if [string match {} $tkCon(errorInfo)] {
	set tkCon(errorInfo) {errorInfo empty}
      }
      catch {destroy $tkCon(base).error}
      set w [toplevel $tkCon(base).error]
      button $w.close -text Dismiss -command "destroy $w"
      scrollbar $w.sy -takefocus 0 -bd 1 -command "$w.text yview"
      text $w.text -font $tkCon(font) -yscrollcommand "$w.sy set"
      pack $w.close -side bottom -fill x
      pack $w.sy -side right -fill y
      pack $w.text -fill both -expand 1
      $w.text insert 1.0 $tkCon(errorInfo)
      $w.text config -state disabled
    }
    eval {
      ## evals contents in master interpreter
      eval [lreplace $args 0 0]
    }
    font {
      ## "tkcon font ?fontname?".  Sets the font of the console
      if [string comp {} [lindex $args 1]] {
	return [$tkCon(console) config -font [lindex $args 1]]
      } else {
	return [$tkCon(console) config -font]
      }
    }
    hide {
      ## Hides the console with 'withdraw'.
      wm withdraw $tkCon(root)
    }
    iconify {
      ## Iconifies the console with 'iconify'.
      wm iconify $tkCon(root)
    }
    show - deiconify {
      ## "tkcon show|deiconify".  Deiconifies the console.
      wm deiconify $tkCon(root)
    }
    title {
      ## "tkcon title ?title?".  Retitles the console
      if [string comp {} [lindex $args 1]] {
	return [wm title $tkCon(root) [lindex $args 1]]
      } else {
	return [wm title $tkCon(root)]
      }
    }
    version {
      return $tkCon(version)
    }
    default {
      ## tries to determine if the command exists, otherwise throws error
      set cmd [lindex $args 0]
      set cmd tkCon[string toup [string index $cmd 0]][string range $cmd 1 end]
      if [string match $cmd [info command $cmd]] {
	eval $cmd [lreplace $args 0 0]
      } else {
	error "bad option \"[lindex $args 0]\": must be attach, close,\
		console, destroy, eval, font, hide, iconify,\
		load, main, new, save, show, slave, deiconify, title"
      }
    }
  }
}

##
## Some procedures to make up for lack of built-in shell commands
##

## puts
## This allows me to capture all stdout/stderr to the console window
# ARGS:	same as usual	
# Outputs:	the string with a color-coded text tag
## 
catch {rename puts tcl_puts}
proc puts args {
  set len [llength $args]
  if {$len==1} {
    eval tkcon console insert output $args stdout {\n} stdout
    tkcon console see output
  } elseif {$len==2 &&
    [regexp {(stdout|stderr|-nonewline)} [lindex $args 0] junk tmp]} {
    if [string comp $tmp -nonewline] {
      eval tkcon console insert output [lreplace $args 0 0] $tmp {\n} $tmp
    } else {
      eval tkcon console insert output [lreplace $args 0 0] stdout
    }
    tkcon console see output
  } elseif {$len==3 &&
    [regexp {(stdout|stderr)} [lreplace $args 2 2] junk tmp]} {
    if [string comp [lreplace $args 1 2] -nonewline] {
      eval tkcon console insert output [lrange $args 1 1] $tmp
    } else {
      eval tkcon console insert output [lreplace $args 0 1] $tmp
    }
    tkcon console see output
  } else {
    eval tcl_puts $args
  }
}

## clear - clears the buffer of the console (not the history though)
## This is executed in the parent interpreter
## 
proc clear {{pcnt 100}} {
  if {![regexp {^[0-9]*$} $pcnt] || $pcnt < 1 || $pcnt > 100} {
    error "invalid percentage to clear: must be 1-100 (100 default)"
  } elseif {$pcnt == 100} {
    tkcon console delete 1.0 end
  } else {
    set tmp [expr $pcnt/100.0*[tkcon console index end]]
    tkcon console delete 1.0 "$tmp linestart"
  }
}

## alias - akin to the csh alias command
## If called with no args, then it dumps out all current aliases
## If called with one arg, returns the alias of that arg (or {} if none)
# ARGS:	newcmd	- (optional) command to bind alias to
# 	args	- command and args being aliased
## 
proc alias {{newcmd {}} args} {
  if [string match {} $newcmd] {
    set res {}
    foreach a [interp aliases] {
      lappend res [list $a: [interp alias {} $a]]
    }
    return [join $res \n]
  } elseif {[string match {} $args]} {
    interp alias {} $newcmd
  } else {
    eval interp alias {{}} $newcmd {{}} $args
  }
}

## unalias - unaliases an alias'ed command
# ARGS:	cmd	- command to unbind as an alias
## 
proc unalias {cmd} {
  interp alias {} $cmd {}
}

## dump - outputs variables/procedure/widget info in source'able form.
## Accepts glob style pattern matching for the names
# ARGS:	type	- type of thing to dump: must be variable, procedure, widget
# OPTS: -nocomplain	don't complain if no vars match something
# Returns:	the values of the variables in a 'source'able form
## 
proc dump {type args} {
  set whine 1
  set code ok
  if [string match \-n* [lindex $args 0]] {
    set whine 0
    set args [lreplace $args 0 0]
  }
  if {$whine && [string match {} $args]} {
    error "wrong \# args: [lindex [info level 0] 0] ?-nocomplain? pattern ?pattern ...?"
  }
  set res {}
  switch -glob -- $type {
    v* {
      # variable
      # outputs variables value(s), whether array or simple.
      foreach arg $args {
	if {[string match {} [set vars [uplevel info vars [list $arg]]]]} {
	  if {[uplevel info exists $arg]} {
	    set vars $arg
	  } elseif $whine {
	    append res "\#\# No known variable $arg\n"
	    set code error
	    continue
	  } else continue
	}
	foreach var [lsort $vars] {
	  upvar $var v
	  if {[array exists v]} {
	    append res "array set $var \{\n"
	    foreach i [lsort [array names v]] {
	      upvar 0 v\($i\) w
	      if {[array exists w]} {
		append res "    [list $i {NESTED VAR ERROR}]\n"
		if $whine { set code error }
	      } else {
		append res "    [list $i $v($i)]\n"
	      }
	    }
	    append res "\}\n"
	  } else {
	    append res [list set $var $v]\n
	  }
	}
      }
    }
    p* {
      # procedure
      foreach arg $args {
	if {[string comp {} [set ps [info proc $arg]]]} {
	  foreach p [lsort $ps] {
	    set as {}
	    foreach a [info args $p] {
	      if {[info default $p $a tmp]} {
		lappend as [list $a $tmp]
	      } else {
		lappend as $a
	      }
	    }
	    append res [list proc $p $as [info body $p]]\n
	  }
	} elseif $whine {
	  append res "\#\# No known proc $arg\n"
	}
      }
    }
    w* {
      # widget
    }
    default {
      return -code error "bad [lindex [info level 0] 0] option\
	\"[lindex $args 0]\":\ must be procedure, variable, widget"
    }
  }
  return -code $code [string trimr $res \n]
}

## which - tells you where a command is found
# ARGS:	cmd	- command name
# Returns:	where command is found (internal / external / unknown)
## 
proc which cmd {
  if [string comp {} [info commands $cmd]] {
    if {[lsearch -exact [interp aliases] $cmd] > -1} {
      return "$cmd:\taliased to [alias $cmd]"
    } elseif [string comp {} [info procs $cmd]] {
      return "$cmd:\tinternal proc"
    } else {
      return "$cmd:\tinternal command"
    }
  } elseif [auto_execok $cmd] {
    return [auto_execpath $cmd]
  } else {
    return "$cmd:\tunknown command"
  }
}

## auto_execpath - tells you where an external command is found
## Only a slight modification from core auto_execok proc
# ARGS:	cmd	- command name
# Returns:	where command is found or {} if not found
## 
if {[string match windows $tcl_platform(platform)]} {
  proc auto_execpath name {
    global auto_execpath tcl_platform env

    if [info exists auto_execpath($name)] {
      return $auto_execpath($name)
    }
    set auto_execpath($name) {}
    if {[string comp relative [file pathtype $name]]} {
      foreach ext {{} .exe .bat .cmd} {
	if {[file exists ${name}${ext}] && \
	    ![file isdirectory ${name}${ext}]} {
	  set auto_execpath($name) $name
	}
      }
      return $auto_execpath($name)
    }
    if {[info exists env(PATH)]} {
      set path $env(PATH)
    } else {
      if [info exists env(Path)] { set path $env(Path) } else { return {} }
    }
    foreach dir [split $path {;}] {
      if {[string match {} $dir]} { set dir . }
      foreach ext {{} .exe .bat .cmd} {
	set file [file join $dir ${name}${ext}]
	if {[file exists $file] && ![file isdirectory $file]} {
	  set auto_execpath($name) $file
	  break
	}
      }
    }
    return $auto_execpath($name)
  }
} else {
  proc auto_execpath name {
    global auto_execpath env

    if [info exists auto_execpath($name)] {
      return $auto_execpath($name)
    }
    set auto_execpath($name) {}
    if {[string comp relative [file pathtype $name]]} {
      if {[file executable $name] && ![file isdirectory $name]} {
	set auto_execpath($name) $name
      }
      return $auto_execpath($name)
    }
    foreach dir [split $env(PATH) :] {
      if {[string match {} $dir]} { set dir . }
      set file [file join $dir $name]
      if {[file executable $file] && ![file isdirectory $file]} {
	set auto_execpath($name) $file
	break
      }
    }
    return $auto_execpath($name)
  }
}

## dir - directory list
# ARGS:	args	- names/glob patterns of directories to list
# OPTS:	-all	- list hidden files as well (Unix dot files)
#	-long	- list in full format "permissions size date filename"
#	-full	- displays / after directories and link paths for links
# Returns:	a directory listing
## 
proc dir {args} {
  array set s {
    all 0 full 0 long 0 0 --- 1 --x 2 -w- 3 -wx 4 r-- 5 r-x 6 rw- 7 rwx
  }
  while {[string match \-* [lindex $args 0]]} {
    set str [lindex $args 0]
    set args [lreplace $args 0 0]
    switch -glob -- $str {
      -a* {set s(all) 1} -f* {set s(full) 1} -l* {set s(long) 1} -- break
      default {
	error "Passed unknown arg $str, should be one of: -all, -full, -long"
      }
    }
  }
  set sep [string trim [file join . .] .]
  if [string match {} $args] { set args . }
  foreach arg $args {
    if {[file isdir $arg]} {
      set arg [string trimr $arg $sep]$sep
      if $s(all) {
	lappend out [list $arg [lsort [glob -nocomplain -- $arg.* $arg*]]]
      } else {
	lappend out [list $arg [lsort [glob -nocomplain -- $arg*]]]
      }
    } else {
      lappend out [list [file dirname $arg]$sep \
		       [lsort [glob -nocomplain -- $arg]]]
    }
  }
  if $s(long) {
    set old [clock scan {1 year ago}]
    set fmt "%s%9d %s %s\n"
    foreach o $out {
      set d [lindex $o 0]
      append res $d:\n
      foreach f [lindex $o 1] {
	file lstat $f st
	set f [file tail $f]
	if $s(full) {
	  switch -glob $st(type) {
	    d* { append f $sep }
	    l* { append f "@ -> [file readlink $d$sep$f]" }
	    default { if [file exec $d$sep$f] { append f * } }
	  }
	}
	if [string match file $st(type)] {
	  set mode -
	} else {
	  set mode [string index $st(type) 0]
	}
	foreach j [split [format %o [expr $st(mode)&0777]] {}] {
	  append mode $s($j)
	}
	if {$st(mtime)>$old} {
	  set cfmt {%b %d %H:%M}
	} else {
	  set cfmt {%b %d  %Y}
	}
	append res [format $fmt $mode $st(size) \
			[clock format $st(mtime) -format $cfmt] $f]
      }
      append res \n
    }
  } else {
    foreach o $out {
      set d [lindex $o 0]
      append res $d:\n
      set i 0
      foreach f [lindex $o 1] {
	if {[string len [file tail $f]] > $i} {
	  set i [string len [file tail $f]]
	}
      }
      set i [expr $i+2+$s(full)]
      set j [expr [tkcon eval set tkCon(cols)]/$i]
      set k 0
      foreach f [lindex $o 1] {
	set f [file tail $f]
	if $s(full) {
	  switch -glob [file type $d$sep$f] {
	    d* { append f $sep }
	    l* { append f @ }
	    default { if [file exec $d$sep$f] { append f * } }
	  }
	}
	append res [format "%-${i}s" $f]
	if {[incr k]%$j == 0} {set res [string trimr $res]\n}
      }
      append res \n\n
    }
  }
  return [string trimr $res]
}


## tclindex - creates the tclIndex file
# OPTS:	-ext	- extensions to auto index (defaults to *.tcl)
# ARGS:	args	- directories to auto index (defaults to pwd)
# Outputs:	tclIndex file to each directory
##
proc tclindex args {
  set ext {*.tcl}
  if [string match \-e* [lindex $args 0]] {
    set ext  [lindex $args 1]
    set args [lreplace $args 0 1]
  }
  if [string match {} $args] {
    eval auto_mkindex [list [pwd]] $ext
  } else {
    foreach dir $args {
      if [file isdir $dir] { eval auto_mkindex [list $dir] $ext }
    }
  }
}

## lremove - remove items from a list
# OPTS:	-all	remove all instances of each item
# ARGS:	l	a list to remove items from
#	is	a list of items to remove
##
proc lremove {args} {
  set all 0
  if [string match \-a* [lindex $args 0]] {
    set all 1
    set args [lreplace $args 0 0]
  }
  set l [lindex $args 0]
  eval append is [lreplace $args 0 0]
  foreach i $is {
    if {[set ix [lsearch -exact $l $i]] == -1} continue
    set l [lreplace $l $ix $ix]
    if $all {
      while {[set ix [lsearch -exact $l $i]] != -1} {
	set l [lreplace $l $i $i]
      }
    }
  }
  return $l
}


## Unknown changed to get output into tkCon window
# unknown:
# Invoked when a Tcl command is invoked that doesn't exist in the
# interpreter:
#
#	1. See if the autoload facility can locate the command in a
#	   Tcl script file.  If so, load it and execute it.
#	2. If the command was invoked interactively at top-level:
#	    (a) see if the command exists as an executable UNIX program.
#		If so, "exec" the command.
#	    (b) see if the command requests csh-like history substitution
#		in one of the common forms !!, !<number>, or ^old^new.  If
#		so, emulate csh's history substitution.
#	    (c) see if the command is a unique abbreviation for another
#		command.  If so, invoke the command.
#
# Arguments:
# args -	A list whose elements are the words of the original
#		command, including the command name.

proc unknown args {
  global auto_noexec auto_noload env unknown_pending tcl_interactive tkCon
  global errorCode errorInfo

  # Save the values of errorCode and errorInfo variables, since they
  # may get modified if caught errors occur below.  The variables will
  # be restored just before re-executing the missing command.

  set savedErrorCode $errorCode
  set savedErrorInfo $errorInfo
  set name [lindex $args 0]
  if ![info exists auto_noload] {
    #
    # Make sure we're not trying to load the same proc twice.
    #
    if [info exists unknown_pending($name)] {
      unset unknown_pending($name)
      if {[array size unknown_pending] == 0} {
	unset unknown_pending
      }
      return -code error "self-referential recursion in \"unknown\" for command \"$name\"";
    }
    set unknown_pending($name) pending;
    set ret [catch {auto_load $name} msg]
    unset unknown_pending($name);
    if {$ret != 0} {
      return -code $ret -errorcode $errorCode \
	  "error while autoloading \"$name\": $msg"
    }
    if ![array size unknown_pending] {
      unset unknown_pending
    }
    if $msg {
      set errorCode $savedErrorCode
      set errorInfo $savedErrorInfo
      set code [catch {uplevel $args} msg]
      if {$code ==  1} {
	#
	# Strip the last five lines off the error stack (they're
	# from the "uplevel" command).
	#

	set new [split $errorInfo \n]
	set new [join [lrange $new 0 [expr [llength $new] - 6]] \n]
	return -code error -errorcode $errorCode \
	    -errorinfo $new $msg
      } else {
	return -code $code $msg
      }
    }
  }
  if {[info level] == 1 && [string match {} [info script]] \
	  && [info exists tcl_interactive] && $tcl_interactive} {
    if ![info exists auto_noexec] {
      if [auto_execok $name] {
	set errorCode $savedErrorCode
	set errorInfo $savedErrorInfo
	return [uplevel exec $args]
	#return [uplevel exec >&@stdout <@stdin $args]
      }
    }
    set errorCode $savedErrorCode
    set errorInfo $savedErrorInfo
    ##
    ## History substitution moved into tkConEvalCmd
    ##
    set cmds [info commands $name*]
    if {[llength $cmds] == 1} {
      return [uplevel [lreplace $args 0 0 $cmds]]
    }
    if {[llength $cmds]} {
      if {$name == ""} {
	return -code error "empty command name \"\""
      } else {
	return -code error \
	    "ambiguous command name \"$name\": [lsort $cmds]"
      }
    }
  }
  return -code error "invalid command name \"$name\""
}


#-------------------------------------------------------------------------
# Elements of tkPriv that are used in this file:
#
# char -		Character position on the line;  kept in order
#			to allow moving up or down past short lines while
#			still remembering the desired position.
# mouseMoved -		Non-zero means the mouse has moved a significant
#			amount since the button went down (so, for example,
#			start dragging out a selection).
# prevPos -		Used when moving up or down lines via the keyboard.
#			Keeps track of the previous insert position, so
#			we can distinguish a series of ups and downs, all
#			in a row, from a new up or down.
# selectMode -		The style of selection currently underway:
#			char, word, or line.
# x, y -		Last known mouse coordinates for scanning
#			and auto-scanning.
#-------------------------------------------------------------------------

# tkConClipboardKeysyms --
# This procedure is invoked to identify the keys that correspond to
# the "copy", "cut", and "paste" functions for the clipboard.
#
# Arguments:
# copy -	Name of the key (keysym name plus modifiers, if any,
#		such as "Meta-y") used for the copy operation.
# cut -		Name of the key used for the cut operation.
# paste -	Name of the key used for the paste operation.

proc tkConClipboardKeysyms {copy cut paste} {
  bind Console <$copy>	{tkConCopy %W}
  bind Console <$cut>	{tkConCut %W}
  bind Console <$paste>	{tkConPaste %W}
}

proc tkConCut w {
  if [string match $w [selection own -displayof $w]] {
    clipboard clear -displayof $w
    catch {
      clipboard append -displayof $w [selection get -displayof $w]
      if [$w compare sel.first >= limit] {$w delete sel.first sel.last}
    }
  }
}
proc tkConCopy w {
  if [string match $w [selection own -displayof $w]] {
    clipboard clear -displayof $w
    catch {clipboard append -displayof $w [selection get -displayof $w]}
  }
}

proc tkConPaste w {
  if ![catch {selection get -displayof $w -selection CLIPBOARD} tmp] {
    if [$w compare insert < limit] {$w mark set insert end}
    $w insert insert $tmp
    $w see insert
    if [string match *\n* $tmp] {tkConEval $w}
  }
}

## Get all Text bindings into Console except Unix cut/copy/paste
## and newline insertion
foreach ev [lremove [bind Text] {<Control-Key-y> <Control-Key-w> \
				     <Meta-Key-w> <Control-Key-o>}] {
  bind Console $ev [bind Text $ev]
}
unset ev

## Redefine for Console what we need
##
tkConClipboardKeysyms F16 F20 F18
tkConClipboardKeysyms Control-c Control-x Control-v

bind Console <Insert> {catch {tkConInsert %W [selection get -displayof %W]}}

bind Console <Up> {
  if [%W compare {insert linestart} != {limit linestart}] {
    tkTextSetCursor %W [tkTextUpDownLine %W -1]
  } else {
    if {$tkCon(event) == [tkConEvalSlave history nextid]} {
      set tkCon(cmdbuf) [tkConCmdGet %W]
    }
    if [catch {tkConEvalSlave \
		   history event [incr tkCon(event) -1]} tkCon(tmp)] {
      incr tkCon(event)
    } else {
      %W delete limit end
      %W insert limit $tkCon(tmp)
      %W see end
    }
  }
}
bind Console <Down> {
  if [%W compare {insert linestart} != {end-1c linestart}] {
    tkTextSetCursor %W [tkTextUpDownLine %W 1]
  } else {
    if {$tkCon(event) < [tkConEvalSlave history nextid]} {
      %W delete limit end
      if {[incr tkCon(event)] == [tkConEvalSlave history nextid]} {
	%W insert limit $tkCon(cmdbuf)
      } else {
	%W insert limit [tkConEvalSlave history event $tkCon(event)]
      }
      %W see end
    }
  }
}
bind Console <Tab> {
  if [%W compare insert > limit] {tkConExpand %W path}
}
bind Console <Control-P> {
  if [%W compare insert > limit] {tkConExpand %W proc}
}
bind Console <Control-V> {
  if [%W compare insert > limit] {tkConExpand %W var}
}
bind Console <Control-i> {
  if [%W compare insert >= limit] {
    tkConInsert %W \t
  }
}
bind Console <Return> {
  tkConEval %W
}
bind Console <KP_Enter> [bind Console <Return>]
bind Console <Delete> {
  if {[string comp {} [%W tag nextrange sel 1.0 end]] \
	  && [%W compare sel.first >= limit]} {
    %W delete sel.first sel.last
  } elseif [%W compare insert >= limit] {
    %W delete insert
    %W see insert
  }
}
bind Console <BackSpace> {
  if {[string comp {} [%W tag nextrange sel 1.0 end]] \
	  && [%W compare sel.first >= limit]} {
    %W delete sel.first sel.last
  } elseif {[%W compare insert != 1.0] && [%W compare insert > limit]} {
    %W delete insert-1c
    %W see insert
  }
}
bind Console <Control-h> [bind Console <BackSpace>]

bind Console <KeyPress> {
  tkConInsert %W %A
}

bind Console <Control-a> {
  if [%W compare {limit linestart} == {insert linestart}] {
    tkTextSetCursor %W limit
  } else {
    tkTextSetCursor %W {insert linestart}
  }
}
bind Console <Control-d> {
  if [%W compare insert < limit] break
  %W delete insert
}
bind Console <Control-k> {
  if [%W compare insert < limit] break
  if [%W compare insert == {insert lineend}] {
    %W delete insert
  } else {
    %W delete insert {insert lineend}
  }
}
bind Console <Control-l> {
  ## Clear console buffer, without losing current command line input
  set tkCon(tmp) [tkConCmdGet %W]
  clear
  tkConPrompt {} $tkCon(tmp)
}
bind Console <Control-n> {
  ## Goto next command in history
  if {$tkCon(event) < [tkConEvalSlave history nextid]} {
    %W delete limit end
    if {[incr tkCon(event)] == [tkConEvalSlave history nextid]} {
      %W insert limit $tkCon(cmdbuf)
    } else {
      %W insert limit [tkConEvalSlave history event $tkCon(event)]
    }
    %W see end
  }
}
bind Console <Control-p> {
  ## Goto previous command in history
  if {$tkCon(event) == [tkConEvalSlave history nextid]} {
    set tkCon(cmdbuf) [tkConCmdGet %W]
  }
  if [catch {tkConEvalSlave history event [incr tkCon(event) -1]} tkCon(tmp)] {
    incr tkCon(event)
  } else {
    %W delete limit end
    %W insert limit $tkCon(tmp)
    %W see end
  }
}
bind Console <Control-r> {
  ## Search history reverse
  if {$tkCon(svnt) == [tkConEvalSlave history nextid]} {
    set tkCon(cmdbuf) [tkConCmdGet %W]
  }
  set tkCon(tmp1) [string len $tkCon(cmdbuf)]
  incr tkCon(tmp1) -1
  while 1 {
    if {[catch {tkConEvalSlave \
	history event [incr tkCon(svnt) -1]} tkCon(tmp)]} {
      incr tkCon(svnt)
      break
    } elseif {![string comp $tkCon(cmdbuf) \
	[string range $tkCon(tmp) 0 $tkCon(tmp1)]]} {
      %W delete limit end
      %W insert limit $tkCon(tmp)
      break
    }
  }
  %W see end
}
bind Console <Control-s> {
  ## Search history forward
  set tkCon(tmp1) [string len $tkCon(cmdbuf)]
  incr tkCon(tmp1) -1
  while {$tkCon(svnt) < [tkConEvalSlave history nextid]} {
    if {[incr tkCon(svnt)] == [tkConEvalSlave history nextid]} {
      %W delete limit end
      %W insert limit $tkCon(cmdbuf)
      break
    } elseif {![catch {tkConEvalSlave history event $tkCon(svnt)} tkCon(tmp)] \
	&& ![string comp $tkCon(cmdbuf) \
	[string range $tkCon(tmp) 0 $tkCon(tmp1)]]} {
      %W delete limit end
      %W insert limit $tkCon(tmp)
      break
    }
  }
  %W see end
}
bind Console <Control-t> {
  ## Transpose current and previous chars
  if [%W compare insert > limit] {
    tkTextTranspose %W
  }
}
bind Console <Control-u> {
  ## Clear command line (Unix shell staple)
  %W delete limit end
}
bind Console <Control-z> {
  ## Save command buffer
  set tkCon(tmp) $tkCon(cmdsave)
  set tkCon(cmdsave) [tkConCmdGet %W]
  if {[string match {} $tkCon(cmdsave)]} {
    set tkCon(cmdsave) $tkCon(tmp)
  } else {
    %W delete limit end-1c
  }
  tkConInsert %W $tkCon(tmp)
  %W see end
}
catch {bind Console <Key-Page_Up>   { tkTextScrollPages %W -1 }}
catch {bind Console <Key-Prior>     { tkTextScrollPages %W -1 }}
catch {bind Console <Key-Page_Down> { tkTextScrollPages %W 1 }}
catch {bind Console <Key-Next>      { tkTextScrollPages %W 1 }}
bind Console <Meta-d> {
  if [%W compare insert >= limit] {
    %W delete insert {insert wordend}
  }
}
bind Console <Meta-BackSpace> {
  if [%W compare {insert -1c wordstart} >= limit] {
    %W delete {insert -1c wordstart} insert
  }
}
bind Console <Meta-Delete> {
  if [%W compare insert >= limit] {
    %W delete insert {insert wordend}
  }
}
bind Console <ButtonRelease-2> {
  if {(!$tkPriv(mouseMoved) || $tk_strictMotif) \
	  && ![catch {selection get -displayof %W} tkCon(tmp)]} {
    if [%W compare @%x,%y < limit] {
      %W insert end $tkCon(tmp)
    } else {
      %W insert @%x,%y $tkCon(tmp)
    }
    if [string match *\n* $tkCon(tmp)] {tkConEval %W}
  }
}

##
## End weird bindings
##

##
## Bindings for doing special things based on certain keys
##
bind PostCon <Key-parenright> {
  if {$tkCon(lightbrace) && $tkCon(blinktime)>99 &&
      [string comp \\ [%W get insert-2c]]} {
    tkConMatchPair %W \( \) limit
  }
}
bind PostCon <Key-bracketright> {
  if {$tkCon(lightbrace) && $tkCon(blinktime)>99 &&
      [string comp \\ [%W get insert-2c]]} {
    tkConMatchPair %W \[ \] limit
  }
}
bind PostCon <Key-braceright> {
  if {$tkCon(lightbrace) && $tkCon(blinktime)>99 &&
      [string comp \\ [%W get insert-2c]]} {
    tkConMatchPair %W \{ \} limit
  }
}
bind PostCon <Key-quotedbl> {
  if {$tkCon(lightbrace) && $tkCon(blinktime)>99 &&
      [string comp \\ [%W get insert-2c]]} {
    tkConMatchQuote %W limit
  }
}

bind PostCon <KeyPress> {
  if {$tkCon(lightcmd) && [string comp {} %A]} { tkConTagProc %W }
}

## tkConTagProc - tags a procedure in the console if it's recognized
## This procedure is not perfect.  However, making it perfect wastes
## too much CPU time...  Also it should check the existence of a command
## in whatever is the connected slave, not the master interpreter.
##
proc tkConTagProc w {
  set i [$w index "insert-1c wordstart"]
  set j [$w index "insert-1c wordend"]
  if {[string comp {} \
	   [tkConEvalAttached info command [list [$w get $i $j]]]]} {
    $w tag add proc $i $j
  } else {
    $w tag remove proc $i $j
  }
}

## tkConMatchPair - blinks a matching pair of characters
## c2 is assumed to be at the text index 'insert'.
## This proc is really loopy and took me an hour to figure out given
## all possible combinations with escaping except for escaped \'s.
## It doesn't take into account possible commenting... Oh well.  If
## anyone has something better, I'd like to see/use it.  This is really
## only efficient for small contexts.
# ARGS:	w	- console text widget
# 	c1	- first char of pair
# 	c2	- second char of pair
# Calls:	tkConBlink
## 
proc tkConMatchPair {w c1 c2 {lim 1.0}} {
  if [string comp {} [set ix [$w search -back $c1 insert $lim]]] {
    while {[string match {\\} [$w get $ix-1c]] &&
	   [string comp {} [set ix [$w search -back $c1 $ix-1c $lim]]]} {}
    set i1 insert-1c
    while {[string comp {} $ix]} {
      set i0 $ix
      set j 0
      while {[string comp {} [set i0 [$w search $c2 $i0 $i1]]]} {
	append i0 +1c
	if {[string match {\\} [$w get $i0-2c]]} continue
	incr j
      }
      if {!$j} break
      set i1 $ix
      while {$j &&
	     [string comp {} [set ix [$w search -back $c1 $ix $lim]]]} {
	if {[string match {\\} [$w get $ix-1c]]} continue
	incr j -1
      }
    }
    if [string match {} $ix] { set ix [$w index $lim] }
  } else { set ix [$w index $lim] }
  tkConBlink $w $ix [$w index insert]
}

## tkConMatchQuote - blinks between matching quotes.
## Blinks just the quote if it's unmatched, otherwise blinks quoted string
## The quote to match is assumed to be at the text index 'insert'.
# ARGS:	w	- console text widget
# Calls:	tkConBlink
## 
proc tkConMatchQuote {w {lim 1.0}} {
  set i insert-1c
  set j 0
  while {[string comp {} [set i [$w search -back \" $i $lim]]]} {
    if {[string match {\\} [$w get $i-1c]]} continue
    if {!$j} {set i0 $i}
    incr j
  }
  if [expr $j%2] {
    tkConBlink $w $i0 [$w index insert]
  } else {
    tkConBlink $w [$w index insert-1c] [$w index insert]
  }
}

## tkConBlink - blinks between 2 indices for a specified duration.
# ARGS:	w	- console text widget
# 	i1	- start index to blink region
# 	i2	- end index of blink region
# 	dur	- duration in usecs to blink for
# Outputs:	blinks selected characters in $w
## 
proc tkConBlink {w i1 i2} {
  global tkCon
  $w tag add blink $i1 $i2
  after $tkCon(blinktime) $w tag remove blink $i1 $i2
  return
}


## tkConInsert
## Insert a string into a text console at the point of the insertion cursor.
## If there is a selection in the text, and it covers the point of the
## insertion cursor, then delete the selection before inserting.
# ARGS:	w	- text window in which to insert the string
# 	s	- string to insert (usually just a single char)
# Outputs:	$s to text widget
## 
proc tkConInsert {w s} {
  if {[string match {} $s] || [string match disabled [$w cget -state]]} {
    return
  }
  if [$w comp insert < limit] {
    $w mark set insert end
  }
  catch {
    if {[$w comp sel.first <= insert] && [$w comp sel.last >= insert]} {
      $w delete sel.first sel.last
    }
  }
  $w insert insert $s
  $w see insert
}

## tkConExpand - 
# ARGS:	w	- text widget in which to expand str
# 	type	- type of expansion (path / proc / variable)
# Calls:	tkConExpand(Pathname|Procname|Variable)
# Outputs:	The string to match is expanded to the longest possible match.
#		If tkCon(showmultiple) is non-zero and the user longest match
#		equaled the string to expand, then all possible matches are
#		output to stdout.  Triggers bell if no matches are found.
# Returns:	number of matches found
## 
proc tkConExpand {w type} {
  set exp "\[^\\]\[ \t\n\r\[\{\"\$]"
  set tmp [$w search -back -regexp $exp insert limit]
  if [string compare {} $tmp] {append tmp +2c} else {set tmp limit}
  if [$w compare $tmp >= insert] return
  set str [$w get $tmp insert]
  switch -glob $type {
    pa* { set res [tkConExpandPathname $str] }
    pr* { set res [tkConExpandProcname $str] }
    v*  { set res [tkConExpandVariable $str] }
    default {set res {}}
  }
  set len [llength $res]
  if $len {
    $w delete $tmp insert
    $w insert $tmp [lindex $res 0]
    if {$len > 1} {
      global tkCon
      if {$tkCon(showmultiple) && [string match [lindex $res 0] $str]} {
	puts stdout [lreplace $res 0 0]
      }
    }
  } else bell
  return [incr len -1]
}

## tkConExpandPathname - expand a file pathname based on $str
## This is based on UNIX file name conventions
# ARGS:	str	- partial file pathname to expand
# Calls:	tkConExpandBestMatch
# Returns:	list containing longest unique match followed by all the
#		possible further matches
## 
proc tkConExpandPathname str {
  set pwd [tkConEvalAttached pwd]
  if [catch {tkConEvalAttached cd [file dir $str]} err] {
    return -code error $err
  }
  if [catch {lsort [tkConEvalAttached glob [file tail $str]*]} m] {
    set match {}
  } else {
    if {[llength $m] > 1} {
      set tmp [tkConExpandBestMatch $m [file tail $str]]
      if [string match ?*/* $str] {
	set tmp [file dir $str]/$tmp
      } elseif [string match /* $str] {
	set tmp /$tmp
      }
      regsub -all { } $tmp {\\ } tmp
      set match [linsert $m 0 $tmp]
    } else {
      ## This may look goofy, but it handles spaces in path names
      eval append match $m
      if [file isdir $match] {append match /}
      if [string match ?*/* $str] {
	set match [file dir $str]/$match
      } elseif [string match /* $str] {
	set match /$match
      }
      regsub -all { } $match {\\ } match
      ## Why is this one needed and the ones below aren't!!
      set match [list $match]
    }
  }
  tkConEvalAttached cd $pwd
  return $match
}

## tkConExpandProcname - expand a tcl proc name based on $str
# ARGS:	str	- partial proc name to expand
# Calls:	tkConExpandBestMatch
# Returns:	list containing longest unique match followed by all the
#		possible further matches
## 
proc tkConExpandProcname str {
  set match [tkConEvalAttached info commands $str*]
  if {[llength $match] > 1} {
    regsub -all { } [tkConExpandBestMatch $match $str] {\\ } str
    set match [linsert $match 0 $str]
  } else {
    regsub -all { } $match {\\ } match
  }
  return $match
}

## tkConExpandVariable - expand a tcl variable name based on $str
# ARGS:	str	- partial tcl var name to expand
# Calls:	tkConExpandBestMatch
# Returns:	list containing longest unique match followed by all the
#		possible further matches
## 
proc tkConExpandVariable str {
  if [regexp {([^\(]*)\((.*)} $str junk ary str] {
    ## Looks like they're trying to expand an array.
    set match [tkConEvalAttached array names $ary $str*]
    if {[llength $match] > 1} {
      set vars $ary\([tkConExpandBestMatch $match $str]
      foreach var $match {lappend vars $ary\($var\)}
      return $vars
    } else {set match $ary\($match\)}
    ## Space transformation avoided for array names.
  } else {
    set match [tkConEvalAttached info vars $str*]
    if {[llength $match] > 1} {
      regsub -all { } [tkConExpandBestMatch $match $str] {\\ } str
      set match [linsert $match 0 $str]
    } else {
      regsub -all { } $match {\\ } match
    }
  }
  return $match
}

## tkConExpandBestMatch2 - finds the best unique match in a list of names
## Improves upon the speed of the below proc only when $l is small
## or $e is {}.
# ARGS:	l	- list to find best unique match in
# Returns:	longest unique match in the list
## 
proc tkConExpandBestMatch2 {l {e {}}} {
  set ec [lindex $l 0]
  if {[llength $l]>1} {
    set ei [string length $ec]; incr ei -1
    foreach l $l {
      while {$ei>0 && [string first $ec $l]} {
	set ec [string range $ec 0 [incr ei -1]]
      }
    }
  }
  return $ec
}

## tkConExpandBestMatch - finds the best unique match in a list of names
## The extra $e in this argument allows us to limit the innermost loop a
## little further.  This improves speed as $l becomes large or $e becomes long.
# ARGS:	l	- list to find best unique match in
# 	e	- currently best known unique match
# Returns:	longest unique match in the list
## 
proc tkConExpandBestMatch {l {e {}}} {
  set ec [lindex $l 0]
  if {[llength $l]>1} {
    set e  [string length $e]; incr e -1
    set ei [string length $ec]; incr ei -1
    foreach l $l {
      while {$ei>=$e && [string first $ec $l]} {
	set ec [string range $ec 0 [incr ei -1]]
      }
    }
  }
  return $ec
}


## Initialize only if we haven't yet
##
if [catch {winfo exists $tkCon(root)}] tkConInit
