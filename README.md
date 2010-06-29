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


How to compile Kappa
-----------------------------

In the main directory of the distribution (the one that this file is in), type:

`make`

Or to include the graphical interface:

`make full`

Binaries are created in /bin.

For more information you can type:

`make help`


