::if no { -*- mode: tcl; tab-width: 4; -*-
:: vim: set syntax=tcl shiftwidth=4:
@tclsh "%~f0" %*
@goto :eof
}

set version "1.0"

set common {
    UserName        build
    RootName        root

    PathKTX         .
    BaseKTX         "Base.ktx"
    ExecKTX         yes

    PathNetBox      .
    BaseNetBox      "Base.netbox"
    ExecNetBox      yes

    PathRDP         .
    BaseRDP         "Base.rdpc"
    ExecRDP         yes

    ExecPublicKeys  no

    RemoteDirectory NULL
    NoRoot          no

    Disabled        no

    DomainControl   no
    Domain          ""

    ## PuTTY advanced settings
    # 0 - backspace is "Control-?" (default)
    # 1 - backspace is "Control-H"
    BackspaceIsDelete 0
    # 0 - home/end: standard - \e[1~ and \e[4~
    # 3 - home/end: xterm    - \eOH  and \eOF
    RXVTHomeEnd 0
}

regsub -all -line {^\s*#.*?$} $common {} common

set remoteconfig "remoteconfig.tcl"
if { ![file exists $remoteconfig] } {
   set remoteconfig "remoteconfig.cmd"
   if { ![file exists $remoteconfig] } {
        puts "Error: Config not found!"
        exit 1
   }
}

set fd [open $remoteconfig r]
set data [read $fd]
close $fd

if { [catch {lindex $data 0} _] || [catch {lindex $data 1} __] } {
    puts "Error while parse config file: $remote_config"
    exit 1
}

if { $_ eq {::if} && [string is boolean -strict $__] } {
    set data [lrange $data 3 end]
}

foreach { section data } $data {
    if { $section eq "common" } {
        foreach { key val } $data {
            dict set common $key $val
        }
    } elseif { $section eq "hosts" } {
        set hosts $data
    } else {
        puts "Error: Unknown folder '$section' in config!"
    }
    unset section data
}

for { set i 0 } { $i < [llength $argv] } { incr i } {
    set opt [lindex $argv $i]
    if { $opt eq "-script" || $opt eq "--script" } {
       if { [info exists script] } {
           puts stderr "Error, double script option"
           exit 1
       }
       set script [lindex $argv [incr i]]
       continue
    }
    if { $opt eq "-filter" || $opt eq "--filter" || $opt eq "-mask" || $opt eq "--mask" } {
        if { [info exists host-mask] } {
            puts stderr "Error, double host mask"
            exit 1
        }
        set host-mask [lindex $argv [incr i]]
        continue
    }
    set argv [lrange $argv $i end]
}

if { $i == [llength $argv] } {
    set argv [list]
}

if { [info exists script] } {

    set script_orig $script

    set script [file normalize $script]

    if { ![file exists $script] } {
        append script ".tcl"
    }

    if { ![file exists $script] } {
        puts "Error, can't find custom script: [lindex $argv 0]"
    }

    set script_prefix [file rootname [file tail $script]]

    if { [catch { source $script } err] } {
        puts "Error while loading custom execute script:"
        puts $errorInfo
        exit 1
    }

    if { ![llength [info commands "::${script_prefix}::run"]] } {
        puts "Error, can't find run procedure for custom script: ::${script_prefix}::run"
        exit 1
    }

    dict set common ExecKTX no
    dict set common ExecNetBox no
    dict set common ExecPublicKeys no
    dict set common ExecRDP no

    puts "Executing custom script: $script_orig"
    unset script_orig
}

catch { file delete -force remotetool.log }

rename ::puts ::puts_rmgn
proc puts { args } {

    if { [lindex $args 0] eq "-nonewline" } {
        set nonewline 1
        if { [llength $args] > 2 } {
            set channel [lindex $args 1]
            set message [lindex $args 2]
        } {
            set channel "stdout"
            set message [lindex $args 1]
        }
    } {
        if { [llength $args] > 1 } {
            set channel [lindex $args 0]
            set message [lindex $args 1]
        } {
            set channel "stdout"
            set message [lindex $args 0]
        }
    }

    set result [catch [concat ::puts_rmgn $args] return]
    set ec $::errorCode
    set ei $::errorInfo

    if { [info exists nonewline] && $channel eq "stdout" } {
        flush stdout
    }

    if { $channel eq "stdout" && ![catch { open remotetool.log a+ } fd] } {
        if { [info exists nonewline] } {
            ::puts_rmgn -nonewline $fd $message
        } {
            ::puts_rmgn $fd $message
        }
        close $fd
    }

    return -code $result -errorcode $ec -errorinfo $ei $return

}

if { [info exists script_prefix] && [llength [info commands "::${script_prefix}::init"]] } {
    ::${script_prefix}::init
}

set exec_ktx    [dict get $common ExecKTX]
set exec_netbox [dict get $common ExecNetBox]
set exec_keys   [dict get $common ExecPublicKeys]
set exec_rdp    [dict get $common ExecRDP]

if { $exec_ktx } {

    set out_ktx  [file normalize [dict get $common PathKTX]]
    set base_ktx [file normalize [dict get $common BaseKTX]]
    puts "Create KTX files in directory: [dict get $common PathKTX]"
    puts "Base KTX file: [dict get $common BaseKTX]"

    if { ![file exists $base_ktx] } {
        puts "Error: BaseKTX file not exists."
        exit 1
    }

    set fd [open $base_ktx r]
    set base_ktx [split [read $fd] \n]
    close $fd

    if { ![file exists $out_ktx] } {
        file mkdir $out_ktx
    }

    if { ![file exists [file join $out_ktx root]] } {
        file mkdir [file join $out_ktx root]
    }

    foreach fn [concat \
        [glob -nocomplain -directory $out_ktx "*.ktx"] \
        [glob -nocomplain -directory [file join $out_ktx root] "*.ktx"] \
    ] {
        if { [string equal -nocase [file tail $fn] [file tail [file normalize [dict get $common BaseKTX]]]] } \
            continue
        file delete $fn
    }

} {
    puts "Creating KTX files is OFF"
}

if { $exec_netbox } {

    set out_netbox  [file normalize [dict get $common PathNetBox]]
    set base_netbox [file normalize [dict get $common BaseNetBox]]
    puts "Create NetBox files in directory: [dict get $common PathNetBox]"
    puts "Base NetBox file: [dict get $common BaseNetBox]"

    if { ![file exists $base_netbox] } {
        puts "Error: BaseNetBox file not exists."
        exit 1
    }

    set fd [open $base_netbox r]
    set base_netbox [split [read $fd] \n]
    close $fd

    if { ![file exists $out_netbox] } {
        file mkdir $out_netbox
    }

    if { ![file exists [file join $out_netbox root]] } {
        file mkdir [file join $out_netbox root]
    }

    foreach fn [concat \
        [glob -nocomplain -directory $out_netbox "*.netbox"] \
        [glob -nocomplain -directory [file join $out_netbox root] "*.netbox"] \
    ] {
        if { [string equal -nocase [file tail $fn] [file tail [file normalize [dict get $common BaseNetBox]]]] } \
            continue
        file delete $fn
    }

} {
    puts "Creating NetBox files is OFF"
}

if { $exec_rdp } {

    set out_rdp  [file normalize [dict get $common PathRDP]]
    set base_rdp [file normalize [dict get $common BaseRDP]]
    puts "Create RDP files in directory: [dict get $common PathRDP]"
    puts "Base RDP file: [dict get $common BaseRDP]"

    if { ![file exists $base_rdp] } {
        puts "Error: BaseRDP file not exists."
        exit 1
    }

    set fd [open $base_rdp r]
    set base_rdp [split [read $fd] \n]
    close $fd

    if { ![file exists $out_rdp] } {
        file mkdir $out_rdp
    }

    foreach fn [glob -nocomplain -directory $out_rdp "*.rdpc"] {
        if { [string equal -nocase [file tail $fn] [file tail [file normalize [dict get $common BaseRDP]]]] } \
            continue
        file delete $fn
    }

} {
    puts "Creating RDP files is OFF"
}

if { $exec_keys } {
    if { ![llength [auto_execok ssh-copy-id]] } {
        puts "Error: ssh-copy-id command not found in current directory or through PATH environment variable."
    }
} {
    puts "Adding public key on hosts is OFF"
}

proc check_connect { host port { timeout 10000 } } {

    set sk [socket -async $host $port]
    set timeout [expr { [clock milliseconds] + $timeout }]

    while { [fconfigure $sk -connecting] } {
       if { [clock milliseconds] >= $timeout } {
           catch { close $sk }
           return 0
       }
       after 50
    }

    if { [fconfigure $sk -error] ne "" } {
        catch { close $sk }
        return 0
    }

    close $sk
    return 1

}

proc check_ssh_connect { config { timeout 10000 } } {
    return [check_connect [dict get $config HostName] 22 $timeout]
}

puts "Global domain control: [dict get $common DomainControl]"
if { [info exists host-mask] } {
    puts "Host-mask: ${host-mask}"
}
puts ""
puts "Updating:"

foreach { host config } $hosts {
    puts "  Host group: '$host'..."

    set gen [list apply {{ template config gen } {
        set parsed [regexp -inline {^(.*?)(?:\[(\d+?)-(\d+?)\])(.*)$} $template]
        set out    [list]
        if { [llength $parsed] } {
            for { set i [lindex $parsed 2] } { $i <= [lindex $parsed 3] } { incr i } {
                if { [lindex $parsed 4] eq "" } {
                    lappend out "[lindex $parsed 1]$i" \
                        [dict set config Prefix [format %s%0[string length [lindex $parsed 3]]d [lindex $parsed 1] $i]]
                } {
                    foreach postfix [{*}$gen [lindex $parsed 4] $config $gen] {
                        lappend out "[lindex $parsed 1]$i$postfix" \
                            [dict set config Prefix [format %s%0[string length [lindex $parsed 3]]d%s [lindex $parsed 1] $i $postfix]]
                    }
                }
            }
        } elseif { $template ne "" } {
            lappend out $template \
                [dict set config Prefix $template]
        }
        return $out
    }}]

    proc urlencode { str } {
        set out ""
        foreach char [split $str ""] {
            if { ![string match {[a-zA-Z0-9._~/]} $char] } {
                switch -- $char {
                    " "     { set char %20 }
                    "\n"    { set char %0D%0A }
                    "-"     { }
                    default { set char %[format %.2X [scan $char %c]] }
                }
            }
            append out $char
        }
        return $out
    }

    set make_ktx [list apply {{ base config } {

        set out [list]
        foreach line $base {
            foreach { key val } $config {

                # KiTTY accept property "PrivateKeyFile" as "PublicKeyFile"
                # so ignore actual "PublicKeyFile" property and
                # rename "PrivateKeyFile" property to "PublicKeyFile" before proceed
                if { $key eq "PublicKeyFile" } \
                   continue
                if { $key eq "PrivateKeyFile" } {
                    set key "PublicKeyFile"
                }

                if { $key ni {HostName UserName PublicKeyFile RemoteCommand BackspaceIsDelete RXVTHomeEnd} } \
                    continue

                if { [string first "$key\\" $line] == 0 } {
                    set line "$key\\[urlencode $val]\\"
                    break
                }

            }
            lappend out $line
        }

        return $out
    }}]

    set make_netbox [list apply {{ base config } {
        set out [list]

        if { [dict exists $config PrivateKeyFile] } {
            dict set config TlsCertificateFile [dict get $config PrivateKeyFile]
            dict set config PublicKeyFile      [dict get $config PrivateKeyFile]
        }

        foreach line $base {

            regsub {(<Session name=")(.*?)(">)} $line "\\1[urlencode "Session [dict get $config Prefix]"]\\3" line

            foreach { key val } $config {

                if { $key ni {HostName UserName RemoteDirectory TlsCertificateFile PublicKeyFile} } \
                    continue

                if { $val eq "NULL" } {
                    regsub "(<$key>)(.*?)(</$key>)" $line "" line
                } {
                    regsub "(<$key>)(.*?)(</$key>)" $line "<$key>[urlencode $val]</$key>" line
                }

            }

            if { [string trim $line] ne "" } {
                lappend out $line
            }
        }

        return $out
    }}]

    set make_rdp [list apply {{ base config } {

        set out [list]
        foreach line $base {
            foreach { key val } $config {

                if { $key ni {HostName UserName Password Domain} } \
                    continue

                if { [string first ":$key" $line] != -1 } {
                    regsub ":$key" $line ":$val" line
                    break
                }

            }
            lappend out $line
        }

        return $out
    }}]

    unset -nocomplain DomainDone
    array set DomainDone {}

    foreach { host config } [{*}$gen $host $config $gen] {

        set config [dict merge $common [list HostName $host] $config]

        if { [dict get $config Disabled] } \
            continue

        if { [info exists host-mask] && ![string match ${host-mask} $host] } \
            continue

        puts -nonewline "    $host:"

        if { $exec_ktx } {

            puts -nonewline " KTX: "

            set fd [open [file join $out_ktx "[dict get $config Prefix].ktx"] w]
            fconfigure $fd -translation lf
            puts -nonewline $fd [join [{*}$make_ktx $base_ktx [dict merge $config [list Password [dict get $config UserPass]]]] \n]
            close $fd

            if { ![dict get $config NoRoot] } {
                set fd [open [file join $out_ktx root "[dict get $config Prefix].ktx"] w]
                fconfigure $fd -translation lf
                puts -nonewline $fd [join [{*}$make_ktx $base_ktx \
                    [dict merge $config [list \
                        UserName [dict get $config RootName] \
                        Password [dict get $config RootPass] \
                    ]] \
                ] \n]
                close $fd
            }

            puts -nonewline "OK."

        }

        if { $exec_netbox } {

            puts -nonewline " NetBox: "

            set fd [open [file join $out_netbox "[dict get $config Prefix].netbox"] w]
            fconfigure $fd -translation lf
            puts -nonewline $fd [join [{*}$make_netbox $base_netbox [dict merge $config [list Password [dict get $config UserPass]]]] \n]
            close $fd

            if { ![dict get $config NoRoot] } {
                set fd [open [file join $out_netbox root "[dict get $config Prefix].netbox"] w]
                fconfigure $fd -translation lf
                puts -nonewline $fd [join [{*}$make_netbox $base_netbox \
                    [dict merge $config [list \
                        UserName [dict get $config RootName] \
                        Password [dict get $config RootPass] \
                    ]] \
                ] \n]
                close $fd
            }

            puts -nonewline "OK."

        }

        if { $exec_rdp && [dict get $config ExecRDP] } {

            puts -nonewline " RDP: "

            set fd [open [file join $out_rdp "[dict get $config Prefix].rdpc"] w]
            fconfigure $fd -translation lf
            puts -nonewline $fd [join [{*}$make_rdp $base_rdp [dict merge $config [list Password [dict get $config UserPass]]]] \n]
            close $fd

            puts -nonewline "OK."

        }

        if { $exec_keys } {

            puts -nonewline " Key(user): "
            if { [info exists DomainDone([dict get $config Domain])] } {
                puts -nonewline "SKIP."
            } {
                catch { exec -- {*}[auto_execok ssh-copy-id] -q -i "[dict get $config PublicKeyFile]" [dict get $config UserName]@[dict get $config HostName] [dict get $config UserPass] } out
                puts -nonewline "$out."
            }

            if { ![dict get $config NoRoot] } {
                puts -nonewline " Key(root): "
                catch { exec -- {*}[auto_execok ssh-copy-id] -q -i "[dict get $config PublicKeyFile]" [dict get $config RootName]@[dict get $config HostName] [dict get $config RootPass] } out
                puts -nonewline "$out."
            }

        }

        if { [info exists script_prefix] } {

            if { [info exists DomainDone([dict get $config Domain])] } {
                puts -nonewline " SKIP."
            } {
                if { [catch { ::${script_prefix}::run $host [dict merge $config [list Password [dict get $config UserPass]]] } err] } {
                    puts ""
                    puts "Unexpected error while running custom script command:"
                    puts "Error: $::errorInfo"
                }
            }

        }

        puts ""

        if { [dict get $config DomainControl] && [dict get $config Domain] ne "" } {
            set DomainDone([dict get $config Domain]) 1
        }
    }

}

puts "Done."
