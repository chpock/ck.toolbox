::if no { -*- mode: tcl; tab-width: 4; -*-
:: vim: set syntax=tcl shiftwidth=4:
@tclsh "%~f0" %*
@goto :eof
}

proc usage_and_exit { {errmsg {}} } {

    if { $::quiet == 1 } {
        puts "ERROR"
        exit 1
    } elseif { $::quiet == 2 } {
        exit 1
    }

    if { $errmsg ne "" } {
        puts "Error: $errmsg"
        puts ""
    }
    puts {Usage: ssh-copy-id [-q|-qq] [-i identity_file] [user@]host [password]}
    puts ""
    puts "Parameters:"
    puts "  -q                 Quiet mode. Only \"OK\" or \"ERROR\" will be"
    puts "                     printed."
    puts ""
    puts "  -qq                Silent mode without any messages. If something"
    puts "                     will went wrong then exit code will be 1."
    puts ""
    puts "  -i identity_file   The file which contains public key to add."
    puts "                     Default identity_file is 'id_rsa.pub'"
    puts ""
    puts "plink.exe must be in current directory or accessible through PATH environment variable."
    exit 1
}

set quiet 0

if { [lindex $argv 0] eq "-q" } {
    set quiet 1
    set argv [lrange $argv 1 end]
} elseif { [lindex $argv 0] eq "-qq" } {
    set quiet 2
    set argv [lrange $argv 1 end]
}

if { [lindex $argv 0] eq "-i" } {
    if { [llength $argv] < 2 } {
        usage_and_exit "No pub key specified by -i parameter"
    } {
        set keyfile [lindex $argv 1]
        set argv [lrange $argv 2 end]
    }
} {
    set keyfile "id_rsa.pub"
}

if { [llength $argv] < 1 } {
    usage_and_exit "No host specified"
} {
    set host [lindex $argv 0]
    set argv [lrange $argv 1 end]
}

if { [llength $argv] == 1 } {
    set pass [lindex $argv 0]
} elseif { [llength $argv] > 1 } {
    usage_and_exit "Wrong # of parameters"
}

if { ![file exists $keyfile] || ![file isfile $keyfile] || ![file readable $keyfile] } {
   usage_and_exit "The key file not exists or not readable: $keyfile"
}

set fd [open $keyfile]
set key [string trim [read $fd]]
close $fd

set key [split $key \n]

if { [llength $key] == 0 } {
    usage_and_exit "Key file is empty: $keyfile"
} elseif { [llength $key] == 1 } {
    if { ![regexp {^(ssh-(dss|rsa)|ecdsa-sha2-(nistp256|nistp384|nistp521)) [\w\+/]} $key] } {
        usage_and_exit "Unknown format of the key file: $keyfile"
    }
} else {
    foreach line $key {
        incr line_num

        if { $line eq "" || [string index $line 0] eq "#" } \
            continue

        if { ![info exists key_start] } {
            if { [string first {---- BEGIN } $line] == 0 } {
                set key_start 1
            }
            continue
        }

        if { [string first {---- END } $line] == 0 } {
            set key_end 1
            break
        }

        if { [regexp {^Comment: .rsa-key-} $line] } {
            set key_type "ssh-rsa"
        } elseif { [regexp {^[\w+/]+=*$} $line] } {
            append key_parsed $line
        } else {
            usage_and_exit "Can't parse line $line_num of the key file: $keyfile"
        }
    }

    if { ![info exists key_end] } {
        usage_and_exit "The end of the key is not found in the key file: $keyfile"
    }

    if { ![info exists key_type] } {
        usage_and_exit "Unknown type of key in the key file: $keyfile"
    }

    if { ![info exists key_parsed] } {
        usage_and_exit "Can't find key in the key file: $keyfile"
    }

    set key "$key_type $key_parsed"

    unset key_parsed key_type line line_num key_end key_start
}

if { ![regexp {^.+@.+} $host] } {
    puts -nonewline "Username: "
    flush stdout
    set user [gets stdin]
    set host "$user@$host"
    unset user
}

if { ![info exists pass] } {
    puts -nonewline "Password: "
    flush stdout
    set pass [gets stdin]
}

if { !$quiet } {
    puts -nonewline "Checking connection to the host: $host ..."
    flush stdout
}

proc plink_error { message } {

    if { !$::quiet } {
        puts " ERROR"
        puts ""
        puts "Error while running plink.exe:"
        puts $message
    } elseif { $::quiet == 1 } {
        puts "ERROR"
    }

    exit 1
}

if { [catch { exec -- {*}[auto_execok plink] $host -pw $pass "exit" << "y\ny" 2>@1 } err] } {
    regsub {^.+?\(y/n\)\s+} $err {} err
    plink_error $err
}

if { !$quiet } {
    puts " OK"
    puts -nonewline "Checking key ..."
    flush stdout
}

if { [catch { exec -- {*}[auto_execok plink] $host -pw $pass "test -d ~/.ssh && test -f ~/.ssh/authorized_keys && cat ~/.ssh/authorized_keys || echo" << "y\ny" 2>@1 } err] } {
    plink_error $err
}

foreach line [split $err \n] {
    if { $line eq $key } {
        if { !$quiet } {
            puts " Key has been found in .ssh/authorized_keys"
        } elseif { $quiet == 1 } {
            puts "OK"
        }
        exit 0
    }
}

if { !$quiet } {
    puts -nonewline " Adding key..."
    flush stdout
}

if { [catch { exec -- {*}[auto_execok plink] $host -pw $pass "umask 077; test -d ~/.ssh || mkdir ~/.ssh ; echo \"$key\" >> ~/.ssh/authorized_keys ; cat ~/.ssh/authorized_keys" << "y\ny" 2>@1 } err] } {
    plink_error
}

foreach line [split $err \n] {
    if { $line eq $key } {
        if { !$quiet } {
            puts " OK"
        } elseif { $quiet == 1 } {
            puts "OK"
        }
        exit 0
    }
}

if { !$quiet } {
    puts " ERROR"
    puts ""
    puts "Error: added key not found in the file: .ssh/authorized_keys"
} elseif { $quiet == 1 } {
    puts "ERROR"
}

exit 1
