# web2go

Comand web2go is an attempt to mechanically translate tex.web to Go. (Work in progress.)

This program is being developed and tested using a tex.web file annotated as

    % Version 3.141592653 was similar but more extensive (January 2021)

and will probably crash on any other version or modification of it.  It's a
non-goal to handle anything else than this one file with this single
version.

Installation

    $ go install -u modernc.org/web2go

Documentation: [godoc.org/modernc.org/web2go](http://godoc.org/modernc.org/web2go)
