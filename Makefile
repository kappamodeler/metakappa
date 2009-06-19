all: metakappa_full

metakappa_light: 
	make TKREP=light TKINCLUDES="" TK_CMXA="" KEY="without_key" metakappa


metakappa_full: 
	make metakappa TKREP=full TKINCLUDES="-I +labltk" TK_CMXA="labltk.cmxa jpflib.cmxa frxlib.cmxa -cclib -lpthread -cclib -lXau -cclib -lXdmcp" KEY="without_key" 


INSTALL_DIR= /usr/bin
LOCAL_DIR?=$(HOME)/fedora/bin
VERSION?=X.YY

OPTIONS?=
TKREP?=full
METAKAPPAREP?=

BIN = ./bin
KEY?=without_key

$(BIN): 
	mkdir bin

OCAMLC=		$(OCAMLPREFIX)ocamlc -thread
OCAMLCI=	$(OCAMLPREFIX)ocamlc -thread
OCAMLOPT= 	$(OCAMLPREFIX)ocamlopt.opt -thread
OCAMLYACC=	$(OCAMLPREFIX)ocamlyacc -v
OCAMLLEX=	$(OCAMLPREFIX)ocamllex

TKINCLUDES? = 

OCAMLINCLUDES= 	-I $(METAKAPPAREP)lib/$(TKREP) \
		-I $(METAKAPPAREP)automatically_generated \
		-I $(METAKAPPAREP)lib \
		-I $(METAKAPPAREP)config \
		-I $(METAKAPPAREP)tools \
		-I $(METAKAPPAREP)agent_interfaces \
		-I $(METAKAPPAREP)data_structures \
		-I $(METAKAPPAREP)macro_processing \
		-I $(METAKAPPAREP)agent_tree \
		-I $(METAKAPPAREP)rename_agent \
		-I $(METAKAPPAREP)rename_rule \
		-I $(METAKAPPAREP)pretty_printing \
		-I $(METAKAPPAREP)frontend \
		$(TKINCLUDES) 

OCAMLFLAGS=	$(OCAMLINCLUDES)
OCAMLLIBDIR=	$(shell ocamlc -where)
CFLAGS=		-I $(OCAMLLIBDIR) -Wall -Werror -Wno-unused -DPENTIUM_III_COMPATIBLE

# TK
TK_CMA?  = labltk.cma  jpflib.cma  frxlib.cma
TK_CMXA? = 

LIBS_MLFILES = 
LIBS_CMOFILES = $(LIBS_MLFILES:%.ml=%.cmo)
LIBS_CMXFILES = $(LIBS_MLFILES:%.ml=%.cmx)

AUTOGENML=
AUTODURINGCOMMIT=
MLFULL? = 

TKFILE=

FIRST_OBJS = ./$(METAKAPPAREP)tools/map2.cmo \
	./$(MEtAKAPPAREP)automatically_generated/svn_number.cmo \
	./$(METAKAPPAREP)data_structures/data_structures_metakappa.cmo \
	./$(METAKAPPAREP)lib/superarg.cmo \
	./$(METAKAPPAREP)lib/$(TKREP)/superargTk.cmo \
	./$(METAKAPPAREP)macro_processing/macro_processing.cmo \
	./$(METAKAPPAREP)config/config_metakappa.cmo \
	./$(METAKAPPAREP)tools/exceptions.cmo \
	./$(METAKAPPAREP)tools/error_handler_common.cmo \
	./$(METAKAPPAREP)tools/error.cmo \
	./$(METAKAPPAREP)tools/error_handler.cmo \
	./$(METAKAPPAREP)pretty_printing/pretty_printing.cmo \
	./$(METAKAPPAREP)agent_interfaces/agent_interfaces.cmo \
	./$(METAKAPPAREP)agent_tree/agent_tree.cmo \
	./$(METAKAPPAREP)rename_agent/rename_agent.cmo \
	./$(METAKAPPAREP)rename_rule/rename_rule.cmo 

OBJS = 	$(FIRST_OBJS) \
	./$(METAKAPPAREP)frontend/meta_parse.cmo \
	./$(METAKAPPAREP)frontend/meta_lex.cmo \
	./$(METAKAPPAREP)frontend/compile_rule.cmo \
	./$(METAKAPPAREP)frontend/compile_directives.cmo 


METAKAPPA_MAIN = ./$(METAKAPPAREP)/main.ml 


NATIVE_FIRST = $(FIRST_OBJS:cmo=cmx)
NATIVE_OBJS = $(OBJS:cmo=cmx) 
MLFILES = $(OBJS:cmo=ml) $(SIMPLX_MAIN) $(COMPLX_MAIN)

MLI =  	./$(METAKAPPAREP)/frontend/meta_parse.mli 

CMI = $(FIRST_OBJS) $(MLI:mli=cmi)
CMA = unix.cma threads.cma str.cma nums.cma
CMXA = unix.cmxa threads.cmxa str.cmxa nums.cmxa

METAKAPPA_OUT = metakappa
OUTPUT = $(METAKAPPA_OUT) 

LIB_OPT = 
LIB_BYTE = 

DOCS = $(MLI) $(OBJS:cmo=ml)
DOCREP = ./document
DOCTYPE = 

gen_doc :  
	ocamldoc -$(DOCTYPE) $(DOCS) $(OCAMLINCLUDES) -d $(DOCREP) 

html_doc : 
	make KEY=without_key DOCTYPE=html gen_doc

dot_doc :
	make KEY=without_key DOCTYPE=dot gen_doc


dep :  
	ocamldep $(OCAMLINCLUDES) $(MLFILES)


LINE = $(OCAMLOPT) $(OCAMLFLAGS) $(TKINCLUDES) $(CMXA) $(TK_CMXA) $(LIBSC_CMXA)  $(NATIVE_OBJS) 


metakappa: $(CMI) $(BIN) $(LIBSC_CMXA) $(NATIVE_OBJS) $(METAKAPPA_MAIN)
	$(LINE) $(METAKAPPA_MAIN) -o $(BIN)/$(METAKAPPA_OUT)


./$(METAKAPPAREP)/frontend/meta_parse.ml ./$(METAKAPPAREP)/frontend/meta_parse.mli : ./$(METAKAPPAREP)/frontend/meta_parse.mly 
	ocamlyacc -v ./$(METAKAPPAREP)/frontend/meta_parse.mly 


./$(METAKAPPAREP)/frontend/meta_parse.cmo: ./$(METAKAPPAREP)/frontend/meta_parse.mli ./$(METAKAPPAREP)/frontend/meta_parse.ml 
	$(OCAMLC) $(OCAMLFLAGS) -c ./$(METAKAPPAREP)/frontend/meta_parse.mli ./$(METAKAPPAREP)/frontend/meta_parse.ml 


./$(METAKAPPAREP)/frontend/meta_lex.ml: ./$(METAKAPPAREP)/frontend/meta_lex.mll
	ocamllex ./$(METAKAPPAREP)/frontend/meta_lex.mll


%.cmi : %.mli
	$(OCAMLC) $(OCAMLFLAGS) $<

%.cmo : %.ml 
	$(OCAMLC) $(OCAMLFLAGS) -c $<

%.cmx : %.ml 
	$(OCAMLOPT) $(OCAMLFLAGS) -c $<

install:  bin/metakappa
	sudo ln -sf $(PWD)/bin/* $(INSTALL_DIR) 

install_in_local: bin/metakappa 
	 ln -sf $(PWD)/bin/* $(LOCAL_DIR) 

uninstall_of_local: clean
	cd $(LOCAL_DIR) ; rm -f $(OUTPUT)

uninstall: clean
	cd $(INSTALL_DIR) ; sudo rm -f $(OUTPUT)

$(HOME)/tmp: 
	mkdir $(HOME)/tmp 

tar:tar_prorep
tar_prorep: $(HOME)/tmp
	make clean_all 
	rm -rf $(HOME)/tmp/ProRepPlx-$(VERSION)
	mkdir $(HOME)/tmp/ProRepPlx-$(VERSION)
	cp -r * $(HOME)/tmp/ProRepPlx-$(VERSION)/
	cd $(HOME)/tmp ; tar czf ProRepPlx-$(VERSION).tgz ProRepPlx-$(VERSION)/*
	cp $(HOME)/tmp/ProRepPlx-$(VERSION).tgz $(HOME)/
	rm $(HOME)/tmp/ProRepPlx-$(VERSION).tgz 



install_light:
	cd $(METAKAPPAREP) ; make install_light

clean:
	rm -f *~ ; 
	make -f cleanup


clean_all: clean 
	rm -f $(AUTOGENML) ;
	rm -f simplx_rep/sim complx_rep/compress complx_rep/compress_light  simplx complx_light bd_influence_map bd_influence_map_light complx *.options* $(OUTPUT)

./$(METAKAPPAREP)automatically_generated/svn_number.ml:
	make grab_svn_version_number

grab_svn_version_number:
	svn up | tail -n 1 | sed -e "s/\([^0-9]*\)\([0-9]*\)\./let svn_number = \2 +1/" > automatically_generated/svn_number.ml 

commit:
	make grab_svn_version_number
	svn commit 

help: 
	@echo Usage: ;\
	echo make all: create the simulator sim and the compressor compress and the meta-language preprocessor ;\
	echo make metakappa_full: create the meta-language preprocessor ;\
	echo make metakappa_light: create the light version of the meta-language preprocessor;\
	echo make VERSION=X.YY tar: create all tarballs in your home directory;\
	echo make commit: update config file with the svn number before doing a commit;\
	echo make clean: clean compiled files;\
	echo make clean_data: clean analysis results;\
	echo make clean_all: clean all	
