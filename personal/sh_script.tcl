
namespace eval ::exec_shell_script {

    variable shell_script

    proc init { } {
        variable shell_script

        if { ![llength $::argv] } {
            puts "No shell script to execute."
            exit 1
        }

        set shell_script [file normalize [lindex $::argv 0]]

        if { ![file exists $shell_script] } {
            puts "Shell script file not found: [lindex $::argv 0]"
            exit 1
        }

    }

    proc run { host config } {
        variable shell_script

        if { ![check_ssh_connect $config] } {
            puts -nonewline " OFFLINE"
        } {

            if { [catch { exec -- {*}[auto_execok plink] -m [file nativename $shell_script] [dict get $config UserName]@[dict get $config HostName] << "y\ny" 2>@1 } err] } {
                puts -nonewline " ERROR: "
            } {
                puts -nonewline " OK :"
            }

            puts -nonewline $err

        }

    }

}