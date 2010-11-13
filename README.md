[MetaKappa](http://kappalanguage.org/) - The kappa meta-language
================================

Basic information
---------------------------------------

MetaKappa is a meta-language for Kappa generation that allows the creation of protein hierarchies and the concise representation of protein-interaction rules. These structures can then be expanded into Kappa.


What you need to compile MetaKappa
---------------------------------------

* OCaml 3.09.2
[http://caml.inria.fr/download.en.html](OCaml download page)
* TK/lablTK (for the graphical interface)
* graphviz
[http://www.graphviz.org/](graphviz.org)

On Windows
---------------------------------------
You will also need Cygwin for make and gcc, FlexDll for linking, and ActiveTcl for the graphical interface.

Cygwin (http://www.cygwin.com/)
 * make sure you install gcc v. 3. You may need to rename c:/cygwin/bin/gcc-3.exe to c:/cygwin/bin/gcc.exe

FlexDll (http://alain.frisch.fr/flexdll.html)

ActiveTcl (http://www.activestate.com/activetcl/downloads)
 * make sure to install version 8.4
 

How to compile MetaKappa
-----------------------------

In the main directory of the distribution (the one that this file is in), type:

`make`

Or to include the graphical interface:

`make full`

Binaries are created in /bin.

For more information you can type:

`make help`


