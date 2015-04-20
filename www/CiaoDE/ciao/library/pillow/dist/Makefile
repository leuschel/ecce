include ../../../SETTINGS
include ../../../SHARED

CP=cp -f
CHMOD=chmod

DISTDIR=pillow1.1.$(SYSTEM)

WWWDIR=/home/clip/public_html/Software/pillow
WWWDIRURL=~clip/Software/pillow
MAIN=pillow_doc
DISTDOCFORMATS="texi ps html htmlindex"
COMPRESSDVIPS=
EXECMODE = 775
DATAMODE = 664
LPDOCLIB = /home/clip/lib/lpdoc
PILLOWSRC=/home/clip/Systems/ciao/library/pillow
DOWNLOAD = http://www.clip.dia.fi.upm.es/download_cgi/download.cgi
# Mailing list recommended (leave empty for no list)
MAILLIST = ciao-users

#-------------------------------------------------------------------------

alldist: gensrc genreadmes gendoc
	$(MAKE) SYSTEM=sicstus dist
	$(MAKE) SYSTEM=swi     dist
	$(MAKE) SYSTEM=yap     dist
	$(MAKE) SYSTEM=eclipse dist
	-$(CHMOD) -R ug+rwX *


gensrc:
	./stripsource
	cp ../ops.pl ops_sicstus.pl
	cp ../ops.pl ops_yap.pl

genreadmes:
	cd readmes; $(MAKE)

gendoc:
	cd doc; $(MAKE) all

dist:
	mkdir -p $(DISTDIR)
	cat ../Copyright header.pl ops_$(SYSTEM).pl html.pl http.pl pillow_aux.pl adds_$(SYSTEM).pl > $(DISTDIR)/pillow.pl
	$(CP) icon_address.pl $(DISTDIR)
	$(CP) -R ../examples $(DISTDIR)
	$(CP) README.examples $(DISTDIR)/examples/README
	$(CP) -R ../images $(DISTDIR)
	$(CP) ../Copyright $(CIAODESRC)/LGPL $(DISTDIR)
	$(CP) README_pillow $(DISTDIR)/README
	$(CP) INSTALL_pillow  $(DISTDIR)/INSTALL
	mkdir -p $(DISTDIR)/doc
	$(CP) doc/pillow_doc.info \
              doc/pillow_doc.infoindex \
	      doc/pillow_doc.ps $(DISTDIR)/doc
	$(CP) -R doc/pillow_doc_html $(DISTDIR)/doc
	$(CP) ../doc/* $(DISTDIR)/doc

distclean:
	/bin/rm -rf pillow_aux.pl html.pl http.pl header.pl \
                    ops_sicstus.pl ops_yap.pl README_pillow INSTALL_pillow
	/bin/rm -rf $(DISTDIR)eclipse
	/bin/rm -rf $(DISTDIR)sicstus
	/bin/rm -rf $(DISTDIR)swi
	/bin/rm -rf $(DISTDIR)yap
	/bin/rm -f stripsource.itf
	/bin/rm -f stripsource.po
	-cd readmes ; $(LPMAKEREADME)  distclean
	-cd doc     ; $(LPMAKEINSTALL) distclean

#-------------------------------------------------------------------------

install:
	-mkdir -p $(WWWDIR)
	-chmod $(EXECMODE) $(WWWDIR)
	$(MAKE) installtarindex
	$(MAKE) installwebdescr
	$(MAKE) installsrc
	-ln -s index.html $(WWWDIR)/pillow.html

# Installs the banner and manuals that are posted on the distribution site.
installwebdescr:
	-cd doc; 'rm' $(MAIN).htmlindex
	cd doc; $(MAKE) all install \
	  DOCDIR=$(WWWDIR)                                \
	  DOCFORMATS=$(DISTDOCFORMATS)                    \
	  COMPRESSDVIPS=$(COMPRESSDVIPS)                  \
	  EXECMODE=$(EXECMODE)                            \
	  DATAMODE=$(DATAMODE)			  	  \
	  HTMLSTYLE=$(LPDOCLIB)/clip.css                  \
	  HTMLINDEXHEADFILE=$(PILLOWSRC)/dist/PillowDistHead.html \
	  HTMLINDEXTAILFILE=$(PILLOWSRC)/dist/PillowDistTail.html


# Installs distribution banner page and pointers to tar files and docs.
installtarindex: 
	@echo "*** ---------------------------------------------------------"
	@echo "*** Installing html description pointers..."
	@echo "*** ---------------------------------------------------------"
	-cp $(CIAODESRC)/LGPL $(WWWDIR)
	-chmod $(DATAMODE) $(WWWDIR)/LGPL
	-cp INSTALL_pillow $(WWWDIR)/INSTALL.pillow
	-chmod $(DATAMODE) $(WWWDIR)/INSTALL.pillow
	-cp README_pillow $(WWWDIR)/README.pillow
	-chmod $(DATAMODE) $(WWWDIR)/README.pillow
	-echo "<H2>List of all available PiLLoW distributions\
               (tared, gzipped)</H2>" > $(WWWDIR)/pillow_doc_dist.htmlindex
	-cat TarSrcText_ciao.html >> $(WWWDIR)/pillow_doc_dist.htmlindex
	$(MAKE) SYSTEM=eclipse tarline
	$(MAKE) SYSTEM=sicstus tarline
	$(MAKE) SYSTEM=swi     tarline
	$(MAKE) SYSTEM=yap     tarline
	-chmod $(DATAMODE) $(WWWDIR)/pillow_doc_dist.htmlindex
	@echo "*** ---------------------------------------------------------"
	@echo "*** Done"
	@echo "*** ---------------------------------------------------------"

tarline:
	-echo "<UL><LI><A HREF=""\
           $(DOWNLOAD)?url=$(WWWDIRURL)/$(DISTDIR).tar.gz&list=$(MAILLIST)""> \
           <B class=applname>$(DISTDIR).tar.gz</B></A>:" \
           >> $(WWWDIR)/pillow_doc_dist.htmlindex
	-cat TarSrcText_$(SYSTEM).html >> $(WWWDIR)/pillow_doc_dist.htmlindex
	-echo "</UL>" >> $(WWWDIR)/pillow_doc_dist.htmlindex

installsrc:
	$(MAKE) SYSTEM=eclipse installtar
	$(MAKE) SYSTEM=sicstus installtar
	$(MAKE) SYSTEM=swi     installtar
	$(MAKE) SYSTEM=yap     installtar

installtar:
	tar cf - $(DISTDIR) | gzip --best -c > $(WWWDIR)/$(DISTDIR).tar.gz
	-chmod $(DATAMODE) $(WWWDIR)/$(DISTDIR).tar.gz
