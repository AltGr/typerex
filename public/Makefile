# This Makefile will build all OCaml code, for development.
# Sub-projects may have their own Makefiles internally to build
# non-ocaml files and targets.

include Makefile.config



OCPBUILD=ocp-build
OCPBUILD_FLAGS=

all: 
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -scan
init: 
	$(OCPBUILD) root
	$(OCPBUILD_FLAGS) configure -scan
verbose: 
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -v 5
byte: 
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -byte
opt: 
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -asm
noscan: 
	$(OCPBUILD) build $(OCPBUILD_FLAGS) -no-scan

scan: 
	$(OCPBUILD) build -scan
sanitize: 
	$(OCPBUILD) -sanitize
ocpbuild: 
	$(OCPBUILD) ocp-build

clean-temps:

clean: clean-temps
	$(OCPBUILD) -clean
distclean: clean 
	rm -f Makefile.config

TO_INSTALL = ocp-fix-errors ocp-edit-mode ocp-spotter ocp-type-from-loc

uninstall:
	$(OCPBUILD) -uninstall

installed:
	$(OCPBUILD) -installed

install:
	$(OCPBUILD) install \
	  -install-bundle typerex \
          -install-lib $(LIBDIR) \
          -install-bin $(BINDIR) \
          -install-data $(TYPEREXDIR)

install-destdir:
	$(OCPBUILD) install \
	  -install-destdir $(HOME)/typerex-root \
          -install-lib $(LIBDIR) \
          -install-bin $(BINDIR) \
          -install-data $(TYPEREXDIR)

uninstall-destdir:
	$(OCPBUILD) uninstall \
	  -install-destdir $(HOME)/typerex-root \
          -install-lib $(LIBDIR) 

install-manager:
	sudo cp _obuild/ocaml-manager/ocaml-manager.asm /usr/bin/ocaml-manager
	sudo ocaml-manager -update

fabrice-upload:
	git checkout fabrice-typerex
	git push origin fabrice-typerex
	git push ocamlpro fabrice-typerex

doc:
	cd docs/user-manual; $(MAKE)



configure: configure.ac m4/*.m4
	aclocal -I m4
	autoconf
	./configure $(CONFIGURE_ARGS)

tag:
	git tag typerex.$(VERSION)
	git push ocamlpro typerex.$(VERSION)

force_tag:
	git tag -f typerex.$(VERSION)
	git push -f ocamlpro typerex.$(VERSION)

opamize:
	$(MAKE) opamize-typerex

opamize-typerex:
	./_obuild/ocp-opamer/ocp-opamer.asm \
	 	-descr packages/opam/typerex.descr \
		-opam packages/opam/typerex.opam  \
		typerex $(VERSION) \
		https://github.com/OCamlPro/typerex/tarball/typerex.$(VERSION)

