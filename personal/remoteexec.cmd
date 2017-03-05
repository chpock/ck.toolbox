::if no { -*- mode: tcl; tab-width: 4; -*-
:: vim: set syntax=tcl shiftwidth=4:
@remotetool --script "%~f0" %*
@goto :eof
}

namespace eval ::remoteexec {

    variable shell_script
    variable shell_command

    proc init { } {
        variable shell_script
        variable shell_command

        if { ![llength $::argv] } {
            puts "Error: no shell script to execute."
            exit 1
        }

        set shell_script [lindex $::argv 0]

        if { \
            [string match -nocase "-cmd*" $shell_script] || \
            [string match -nocase "--cmd*" $shell_script] || \
            [string match -nocase "-command*" $shell_script] || \
            [string match -nocase "--command*" $shell_script] \
        } {

            unset shell_script

            if { [llength $::argv] < 2 } {
                puts "Error: no command specified in command line"
                exit 1
            }

            set shell_command [lindex $::argv 1]

        } {

            if { ![file exists $shell_script] } {
                puts "Error: shell script file not found: [lindex $::argv 0]"
                exit 1
            }

        }

    }

    proc run { host config } {
        variable shell_script
        variable shell_command

        if { ![check_ssh_connect $config 2000] } {
            puts -nonewline " OFFLINE"
        } {

            set cmd [auto_execok plink]

            lappend cmd [dict get $config UserName]@[dict get $config HostName]

            if { [dict exists $config Password] && [dict get $config Password] ne "" } {
                lappend cmd -pw [dict get $config Password]
            }

            if { [info exists shell_script] } {
                lappend cmd -m [file nativename $shell_script]
            } {
                lappend cmd $shell_command
            }

            if { [catch { exec -- {*}$cmd  << "y\ny" 2>@1 } err] } {
                puts -nonewline " ERROR: "
            } {
                puts -nonewline " OK :"
            }

            regsub {The server's \w+? key fingerprint is:\n.+?\(y/n\)\s+} $err {} err
            regsub {The first key-exchange algorithm supported by the server.+?\(y/n\)\s+} $err {} err
            regsub {The server's host key is not cached in the registry\. You.+?think it is\.\s+} $err {} err

            puts -nonewline $err

        }

    }

}