# -*- mode: Makefile; -*-
#----------------------------------------------------------------------------
# 
# ****                lpdoc document generation Makefile              *****
# 
# You may not have to change things in this file (most things you want to 
# change are in SETTINGS), but you should probably check just in case.
#----------------------------------------------------------------------------
# gmake	all		generate documentation files according to SETTINGS
# gmake	install		install documentation in dirs indicated in SETTINGS
# gmake	allinstall	make and install docs in dirs indicated in SETTINGS
# gmake	uninstall	uninstall documentation from dirs indicated in SETTINGS
# 
# gmake dvi	 	(or ps, html, etc.) makes manual in that format
#
# gmake clean	 	deletes intermediate files, but leaves .texic & docs
# gmake distclean 	clean + deletes .texic: leaves only docs (.ps,.dvi,...)
# gmake realclean 	clean + deletes docs: leaves only .texic
# gmake braveclean 	realclean + deletes also the .texic files
# 
# gmake view 		calls xdvi 
# 			(also, gmake dviview, gmake svgaview, gmake xgaview)
# gmake psview 		calls ghostview
# gmake pdfview		calls ghostview
# gmake infoview	calls info
# gmake htmlview	calls netscape
#----------------------------------------------------------------------------
# The trickiness (playing with functions for paths, basenames, etc.) 
# ensures that dependencies are maintained even though the sources and 
# targets may be in diferent directories and the names passed 
# to the application may themselves be different from either source or target
#----------------------------------------------------------------------------
# The two entries below are set to the installation values
# Override is so that we make sure we are using the right library 
# (many makefiles define LIBDIR and it is a common mistake to 
# include one of those in a SETTINGS and get lpdoc confused)
# 
#override LIBDIR=/home/clip/lib/lpdoc-1.9-for-ciao-1.11
#override LPDOC=/home/clip/bin/lpdoc-1.9-for-ciao-1.11
# ----------------------------------------------------------------------------
# Define this to be the command that runs tex in your system
# 
TEX=tex 
# Alternative (sometimes smarter about number of times it needs to run):
# TEX=texi2dvi 
# (but insists on checking the links, which is a pain...)
# ----------------------------------------------------------------------------
# Define this to be the command that runs texindex in your system
# 
# Not needed if texi2dvi is installed
TEXINDEX=texindex
# ----------------------------------------------------------------------------
# Define this to be the command that builds .bbl files from .bib bibliography
# files in your system
# 
BIBTEX=bibtex
# ----------------------------------------------------------------------------
# Define this to be the command that converts dvi to postscript in
# your system. Make sure it generates postscript fonts, not bitmaps (selecting 
# -Ppdf often does the trick). -z preserves hypertext links.
# 
DVIPS=dvips -z -Ppdf 
# ----------------------------------------------------------------------------
# Define this to be the command that views dvi files in your system
# 
XDVI=xdvi
# ----------------------------------------------------------------------------
# Define this to be the default size at which manuals are viewed
# This is typically an integer (1-10 usually) and unfortunately changes 
# depending on the version of xdvi used.
# 
XDVISIZE=4
# ----------------------------------------------------------------------------
# If set to yes the texinfo.tex file that comes with the lpdoc distribution 
# will be used. Otherwise, the system texinfo file will be used. 
# 
LIBTEXINFO=yes
# ----------------------------------------------------------------------------
# Define this to be the command that converts postscript to pdf in
# your system. Make sure it generates postscript fonts, not bitmaps (selecting 
# -Ppdf in dvips often does the trick)
# 
PSPDF=ps2pdf 
# ----------------------------------------------------------------------------
# (alt) Define this to be the command that converts tex to pdf in your system
# 
# TEXPDF=pdftex
# ----------------------------------------------------------------------------
# Define this to be the command that converts texinfo files into html
# files in your system.
# While they add support for the @image command, we use a patched version 
# of texi2html 1.52 
# 
TEXI2HTML=$(LIBDIR)/texi2html
# ----------------------------------------------------------------------------
# Define this to be the command that converts texinfo files into info
# files in your system. Set also the appropriate flags.
# 
MAKEINFO=makeinfo
# ----------------------------------------------------------------------------
# Define this to be the command that performs "echo"
# Using printf is best for portability...
# 
ifeq ($(CIAOARCH),SunOS4Sparc)
    ECHO=echo
else
ifeq ($(CIAOARCH),BSDi86)
    ECHO=echo
else
    ECHO=printf "%b\n" 
endif
endif
#----------------------------------------------------------------------------
# This is a command that groups the .texic files and updates menus and pointers
# 
TEXIUPDATE= emacs -batch -l $(LIBDIR)/emacs-library.el \
                   -f update-all-nodes-in-one-file
# OLD
# This is a command that updates texinfo node pointers across
# multiple .texi files, and rewrites them as .ltxi files
#TEXIUPDATE= emacs -batch -l $(LIBDIR)/emacs-library.el \
#                  -f update-all-nodes 
#----------------------------------------------------------------------------
# This is a command that updates the bibligraphic references in .texic files
# 
REFSUPD= emacs -batch -l $(LIBDIR)/emacs-library.el \
                  -f update-all-references
#----------------------------------------------------------------------------
# This is a command that converts .eps files into .gif and .ppm files
# (the -debug option of pstogif does this!)
# 
PSTOGIF= /usr/local/bin/pstogif -debug 
#----------------------------------------------------------------------------
# This is a command that converts .ppm files into .jpg files on stdout
# 
PPMTOJPEG= cjpeg 
#----------------------------------------------------------------------------
# This is a pointer to your version of makertf 
# (converts .texi files into .rtf files) 
# 
MAKERTF= makertf
#----------------------------------------------------------------------------
# This is a pointer to your version of MS hc31 
# (converts .rtf files into Win32 .HLP files) 
# 
RTFTOHLP= hc31
#----------------------------------------------------------------------------
#           You should probably not change anything below this 
#----------------------------------------------------------------------------
# Default values in case not defined in SETTINGS (e.g., in older versions)
DATAMODE = 664       # Permissions for automatically generated data files
EXECMODE = 775       # Perms for automatically generated exec files and dirs
MAINOPTS =           # Options for main file
COMPOPTS =           # Options for component files
STARTPAGE=1          # Starting page
PAPERTYPE=afourpaper # Paper type (output format)
#----------------------------------------------------------------------------
include SETTINGS
#----------------------------------------------------------------------------
# Build filenames
VPATH   = $(FILEPATHS) $(SYSTEMPATHS) $(BASEMAIN)_html
COMPTEXI=$(addsuffix .texic,$(basename $(notdir $(COMPONENTS))))
COMPNAMS=$(addsuffix .pl,$(basename $(COMPONENTS)))
BASEMAIN=$(basename $(notdir $(MAIN)))
#----------------------------------------------------------------------------
# Set targets, doit
.PHONY: default all
default:
	@$(ECHO) "No targets selected in this invocation of $(MAKE)"

all: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Generating docs ($(DOCFORMATS)) for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	$(MAKE) $(patsubst %,$(BASEMAIN).%,$(DOCFORMATS))
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Done."
	@$(ECHO) "*** --------------------------------------------------------"
#----------------------------------------------------------------------------
# MAIN needs special cases because the process is different

# Eliminated in all targets below dependency on $(LPDOC):
# Gives problems in installations (lpdoc may not be accessible)

.PHONY: texi dvi ps pdf ascii info html rtf HLP
texi:  $(BASEMAIN).texi
dvi:   $(BASEMAIN).dvi
ps:    $(BASEMAIN).ps
pdf:   $(BASEMAIN).pdf
ascii: $(BASEMAIN).ascii
info:  $(BASEMAIN).info
html:  $(BASEMAIN).html
rtf:   $(BASEMAIN).rtf
HLP:   $(BASEMAIN).HLP

$(BASEMAIN).texic texic: $(MAIN) $(COMPTEXI) $(SETTINGS)
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $(BASEMAIN).pl to $(BASEMAIN).texic ..."
	@$(ECHO) "*** --------------------------------------------------------"
	@$(LPDOC) $(MAINOPTS) -l $(FILEPATHS) -s $(SYSTEMPATHS) \
	           -i $(INDICES) -u $(PATHSFILE) -p $(STARTPAGE) \
	           -t $(PAPERTYPE) \
	           -main $(notdir $<) $(COMPTEXI)
	-@chmod $(DATAMODE) $(BASEMAIN).texic
	-@chmod $(DATAMODE) $(BASEMAIN).refs
	-@chmod $(DATAMODE) auto*.eps
ifeq (,$(findstring norefs,$(MAINOPTS)))
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Generating $(BASEMAIN)refs.texic, resolving refs..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@$(ECHO) "*** Gathering all references from .refs files..."
	-@cat *.refs > $*refs.aux
	-@$(ECHO) "\\\bibstyle{$(LIBDIR)/alist}"    >> $*refs.aux
	-@$(ECHO) "\\\bibdata{$(BIBFILES)}"         >> $*refs.aux
	-@$(ECHO) "*** Building citation string database using BiBTeX..."
	-@$(BIBTEX) $*refs
	-@mv $*refs.bbl $*refs.el
	-@$(ECHO) "*** Resolving citations in main and all components..."
	-@$(REFSUPD) -file $*.texic -refsfile $*refs.el \
                     -components $(COMPTEXI) 
	-@chmod $(DATAMODE) *.texic
	-@$(ECHO) "*** Gathering again all references from .refs files..."
	-@cat *.refs > $*refs.aux
	-@$(ECHO) "\\\bibstyle{$(LIBDIR)/lpdoc}"  >> $*refs.aux
	-@$(ECHO) "\\\bibdata{$(BIBFILES)}"         >> $*refs.aux
	-@$(ECHO) "*** Building references chapter using BiBTeX..."
	-@$(BIBTEX) $*refs
	-@mv $*refs.bbl $*refs.texic
	-@chmod $(DATAMODE) $*refs.aux $*refs.blg 
	-@chmod $(DATAMODE) $*refs.texic $*refs.el
	-@$(ECHO) "Done."
endif

$(BASEMAIN).manl manl: $(MAIN) $(SETTINGS)
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $(BASEMAIN).pl to $(BASEMAIN).manl ..."
	@$(ECHO) "*** --------------------------------------------------------"
	@$(LPDOC) -l $(FILEPATHS) -s $(SYSTEMPATHS) -u $(PATHSFILE) \
                    -man $(notdir $<)
	-@chmod $(DATAMODE) $(BASEMAIN).manl
	-@chmod $(DATAMODE) auto*.eps

$(BASEMAIN).htmlindex htmlindex $(BASEMAIN).htmlbullet htmlbullet: \
	         $(MAIN) $(SETTINGS)
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $(BASEMAIN).pl to $(BASEMAIN).htmlindex ..."
	@$(ECHO) "*** --------------------------------------------------------"
	@$(LPDOC) $(MAINOPTS) -l $(FILEPATHS) -s $(SYSTEMPATHS) \
                   -u $(PATHSFILE) -htmlindex $(notdir $<)
	-@chmod $(DATAMODE) $(BASEMAIN).htmlbullet
	-@chmod $(DATAMODE) $(BASEMAIN).htmlindex
	-@chmod $(DATAMODE) auto*.eps
	-@cp $(HTMLINDEXHEADFILE) Indexhead_$(BASEMAIN).html
	-@chmod $(DATAMODE) Indexhead_$(BASEMAIN).html
	-@cp $(HTMLINDEXTAILFILE) Indextail_$(BASEMAIN).html
	-@chmod $(DATAMODE) Indextail_$(BASEMAIN).html
ifdef HTMLSTYLE
	-@cp $(HTMLSTYLE) lpdoc.css
	-@chmod $(DATAMODE) lpdoc.css
else
endif
	$(MAKE) jpgimages

$(BASEMAIN).infoindex infoindex: $(MAIN) $(SETTINGS)
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $(BASEMAIN).pl to $(BASEMAIN).infoindex ..."
	@$(ECHO) "*** --------------------------------------------------------"
	@$(LPDOC) -l $(FILEPATHS) -s $(SYSTEMPATHS) -u $(PATHSFILE) \
                    -infoindex $(notdir $<)
	-@chmod $(DATAMODE) $(BASEMAIN).infoindex
	-@chmod $(DATAMODE) auto*.eps
	-@cp $(INFODIRHEADFILE) Indexhead_$(BASEMAIN).info
	-@chmod $(DATAMODE) Indexhead_$(BASEMAIN).info
	-@cp $(INFODIRTAILFILE) Indextail_$(BASEMAIN).info
	-@chmod $(DATAMODE) Indextail_$(BASEMAIN).info
	$(MAKE) txtimages

jpgimages:
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $(BASEMAIN) image files to .jpg ..."
	@$(ECHO) "*** --------------------------------------------------------"
ifneq (,$(addsuffix .jpg,$(basename $(wildcard auto*.eps))))
	$(MAKE) $(addsuffix .jpg,$(basename $(wildcard auto*.eps)))
	-@mkdir -p $(BASEMAIN)_html
	-@chmod $(EXECMODE) $(BASEMAIN)_html
	-mv auto*.jpg $(BASEMAIN)_html
endif

txtimages:
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $(BASEMAIN) image files to .txt ..."
	@$(ECHO) "*** --------------------------------------------------------"
ifneq (,$(addsuffix .txt,$(basename $(wildcard auto*.eps))))
	$(MAKE) $(addsuffix .txt,$(basename $(wildcard auto*.eps)))
endif

#----------------------------------------------------------------------------
# General cases
.SUFFIXES: .src .pl .texic .texi .dvi .ps .pdf .html .info .ascii .rtf .HLP \
           .eps .gif .ppm .jpg .txt 

.src.texic: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $*.src (texinfo) to $*.texic ..."
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "@c -----------------------------------------" >  $*.texic
	@$(ECHO) "@c Do not edit -- edit the .src file instead" >> $*.texic
	@$(ECHO) "@c -----------------------------------------" >> $*.texic
	-@cat $*.src >> $*.texic
	-@chmod $(DATAMODE) $*.texic

.pl.texic: $(SETTINGS)
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $*.pl to $*.texic ..."
	@$(ECHO) "*** --------------------------------------------------------"
	@$(LPDOC) $(COMPOPTS) -l $(FILEPATHS) -s $(SYSTEMPATHS) \
                   -i $(INDICES) -u $(PATHSFILE) \
                   -component $(filter $*.pl, $(COMPNAMS))
	-@chmod $(DATAMODE) $*.texic
	-@chmod $(DATAMODE) $*.refs
	-@chmod $(DATAMODE) auto*.eps

# This is to cancel the internal rule so that the one below is used!
     %.dvi : %.texi

.texic.dvi: $(SETTINGS)
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $*.texic to $*.dvi ..."
	@$(ECHO) "*** --------------------------------------------------------"
ifeq ($(LIBTEXINFO),yes)
	-@ln -s $(LIBDIR)/texinfo.tex 
endif
# Depends on how smart your $(TEX) is...
	-@$(TEX) '\nonstopmode\input $*.texic'
	-$(TEXINDEX) $*.??
	-@$(TEX) '\nonstopmode\input $*.texic'
# Obviously an overkill, but it generates many, many files...
	-@chmod $(DATAMODE) $**.*
	$(MAKE) texclean

.texic.ps: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $*.texic (via $*.dvi) to $*.ps ..."
	@$(ECHO) "*** --------------------------------------------------------"
ifeq ($(LIBTEXINFO),yes)
	-@ln -s $(LIBDIR)/texinfo.tex 
endif
# Depends on how smart your $(TEX) is...
	-@$(TEX) '\nonstopmode\input $*.texic'
	-$(TEXINDEX) $*.??
	-@$(TEX) '\nonstopmode\input $*.texic'
# Obviously an overkill, but it generates many, many files...
	-@chmod $(DATAMODE) $**.*
	$(DVIPS) $*.dvi -o $*.ps
	-@chmod $(DATAMODE) $*.ps
# This, really to fix a bug in some versions of dvips:
	-@rm -f head.tmp body.tmp
ifeq (,$(findstring dvi,$(DOCFORMATS)))
	-@rm -f $*.dvi
endif
	$(MAKE) texclean

# Alternative, using PSPDF. Good for ps figures, but must make sure 
# that no bitmap fonts are generated (at least -Ppdf in dvips MUST be set)
.texic.pdf: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $*.texic (via dvi and ps) to $*.pdf ..."
	@$(ECHO) "*** --------------------------------------------------------"
ifeq ($(LIBTEXINFO),yes)
	-@ln -s $(LIBDIR)/texinfo.tex 
endif
# Depends on how smart your $(TEX) is...
	-@$(TEX) '\nonstopmode\input $*.texic'
	-$(TEXINDEX) $*.??
	-@$(TEX) '\nonstopmode\input $*.texic'
# Obviously an overkill, but it generates many, many files...
	-@chmod $(DATAMODE) $**.*
	$(DVIPS) $*.dvi -o $*.ps
	-@chmod $(DATAMODE) $*.ps
# This, really to fix a bug in some versions of dvips:
	-@rm -f head.tmp body.tmp
ifeq (,$(findstring dvi,$(DOCFORMATS)))
	-@rm -f $*.dvi
endif
	$(MAKE) texclean
	$(PSPDF) $*.ps > $*.pdf
	-@chmod $(DATAMODE) $*.pdf
ifeq (,$(findstring ps,$(DOCFORMATS)))
	-@rm -f $*.ps
endif


# Alternative, using pdftex (but problems with ps figures)
#.texic.pdf: 
#	@$(ECHO) "*** --------------------------------------------------------"
#	@$(ECHO) "*** Converting $*.texic to $*.pdf..."
#	@$(ECHO) "*** --------------------------------------------------------"
#ifeq ($(LIBTEXINFO),yes)
#	-@ln -s $(LIBDIR)/texinfo.tex 
#endif
#	$(TEXPDF) '\nonstopmode\input $*.texic'
#	-$(TEXINDEX) $*.??
#	$(TEXPDF) '\nonstopmode\input $*.texic'
## Obviously an overkill, but it generates many, many files...
#	-@chmod $(DATAMODE) $**.*
#	-@chmod $(DATAMODE) $*.pdf
#ifeq (,$(findstring dvi,$(DOCFORMATS)))
#	-@rm -f $*.dvi
#endif
#	$(MAKE) texclean

.texic.texi:
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting all $*.texic to $*.texi ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-$(TEXIUPDATE) -file $*.texic 
	-@chmod $(DATAMODE) $*.texi

.texi.html:
	$(MAKE) jpgimages
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $*.texi to $*.html ..."
	@$(ECHO) "*** --------------------------------------------------------"
ifdef HTMLSTYLE
	-$(TEXI2HTML) -stylesheet $*.css -expandinfo -split_chapter \
	              -glossary -menu $*.texi
else
	-$(TEXI2HTML) -expandinfo -split_chapter \
	              -glossary -menu $*.texi
endif
	-@mkdir -p $(BASEMAIN)_html
	-@chmod $(EXECMODE) $(BASEMAIN)_html
	-@rm -f $(BASEMAIN)_html/$*.html
	-mv $(BASEMAIN)*.html $(BASEMAIN)_html
ifdef HTMLSTYLE
	-@cp $(HTMLSTYLE) $(BASEMAIN)_html/$*.css
	-@chmod $(DATAMODE) $(BASEMAIN)_html/$*.css
else
endif
	-@cd $(BASEMAIN)_html; ln -s $*_toc.html $*.html
	-@chmod $(DATAMODE) $(BASEMAIN)_html/*.html

# This is to cancel the internal rule so that the one below is used!
#     %.info : %.texi

# Not in all distributions: --force (only in newer versions of exinfo), 
# but needed: otherwise incomplete info file generated if there are any errors
# As an alternative, set error limit very high... --error-limit 100000
.texi.info:
	$(MAKE) txtimages
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $*.texi to $*.info ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-$(MAKEINFO) --error-limit 100000 --force --no-split --verbose \
	            --fill-column=70 --output $*.info $*.texi
	-@chmod $(DATAMODE) $*.info

.texic.ascii:
	$(MAKE) txtimages
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $*.texic to $*.ascii ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-$(MAKEINFO) --no-validate --error-limit 100000 --force --no-split \
             --verbose --no-headers --fill-column=70 --output $*.ascii $*.texic
	-@chmod $(DATAMODE) $*.ascii

.texi.rtf:
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $*.texi to $*.rtf ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-$(MAKERTF) --hpj $*.hpj -o $*.rtf $*.texi
	-@chmod $(DATAMODE) $*.hpj $*.rtf

.rtf.HLP:
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Converting $*.rtf to $*.HLP ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-$(RTFTOHLP) $*
	-@chmod $(DATAMODE) $*.HLP

# The following support the inclusion of images. The source is a .eps file.
# $(PSTOGIF) also generates ppm
.eps.gif:
	@$(ECHO) "Converting $*.eps to $*.gif ..."
	-$(PSTOGIF) $*.eps
	-@chmod $(DATAMODE) $*.gif
	-@rm -f $*.ppm
	-@rm -f $*.ppm.tmp

.eps.ppm:
	@$(ECHO) "Converting $*.eps to $*.ppm ..."
	-$(PSTOGIF) $*.eps
	-@rm -f $*.gif
	-@chmod $(DATAMODE) $*.ppm
	-@rm -f $(DATAMODE) $*.ppm.tmp

.ppm.jpg:
	@$(ECHO) "Converting $*.ppm to $*.jpg ..."
	-$(PPMTOJPEG) $*.ppm > $*.jpg
	-@chmod $(DATAMODE) $*.jpg

.eps.txt:
	@$(ECHO) "Converting $*.eps to $*.txt ..."
	@$(ECHO) ""                      > $*.txt
	@$(ECHO) "[Image file: $*.eps]" >> $*.txt
	-@chmod $(DATAMODE) $*.txt

#----------------------------------------------------------------------------
# INSTALLATION: first, install manuals; then, install index entries.
allinstall: 
	$(MAKE) all
	$(MAKE) install

install: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing manuals for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	$(MAKE) $(filter-out %index, \
              $(patsubst %,install%,$(DOCFORMATS)))
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing indices for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	$(MAKE) $(filter %index, \
	      $(patsubst %,install%,$(DOCFORMATS)))
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Done."
	@$(ECHO) "*** --------------------------------------------------------"

#----------------------------------------------------------------------------
# Installation (and deinstallation) of manuals 
# Eliminated dependencies so that regeneration of manuals is never forced 
# during installation. This means that a prior 'gmake' has to be done by hand.

# $(BASEMAIN).texi
installtexi: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing .texi file for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@mkdir -p $(DOCDIR)
	-@chmod $(EXECMODE) $(DOCDIR)
	-@cp $(BASEMAIN).texi $(DOCDIR) 
	-@cp auto*.eps $(DOCDIR) 
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN).texi
	-@chmod $(DATAMODE) $(DOCDIR)/auto*.eps
ifdef COMPRESSDVIPS
	-$(COMPRESSDVIPS) $(DOCDIR)/$(BASEMAIN).texi
	-@$(ECHO) "<UL class=htmlindex><LI>\
              <A HREF=$(BASEMAIN).texi.gz><B class=applname>$(BASEMAIN)</B> \
              Reference Manual - texinfo,gzipped</A></UL>" \
              > $(DOCDIR)/$(BASEMAIN)_texi.htmlindex
else
	-@$(ECHO) "<UL class=htmlindex><LI>\
              <A HREF=$(BASEMAIN).texi><B  class=applname>$(BASEMAIN)</B> \
              Reference Manual - texinfo</A></UL>" \
              > $(DOCDIR)/$(BASEMAIN)_texi.htmlindex
endif
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN)_texi.htmlindex

uninstalltexi: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Uninstalling .texi file for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(DOCDIR)/$(BASEMAIN).texi*
	-@rm -f $(DOCDIR)/$(BASEMAIN)_texi.htmlindex
	-@rm -f $(DOCDIR)/auto*.eps

# $(BASEMAIN).dvi 
installdvi: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing .dvi file for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@mkdir -p $(DOCDIR)
	-@chmod $(EXECMODE) $(DOCDIR)
	-@cp $(BASEMAIN).dvi $(DOCDIR) 
	-@cp auto*.eps $(DOCDIR) 
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN).dvi
	-@chmod $(DATAMODE) $(DOCDIR)/auto*.eps
ifdef COMPRESSDVIPS
	-$(COMPRESSDVIPS) $(DOCDIR)/$(BASEMAIN).dvi
	-@$(ECHO) "<UL class=htmlindex><LI>\
            <A HREF=$(BASEMAIN).dvi.gz><B class=applname>$(BASEMAIN)</B> \
            Reference Manual - dvi,gzipped</A></UL>" \
            > $(DOCDIR)/$(BASEMAIN)_dvi.htmlindex
else
	-@$(ECHO) "<UL class=htmlindex><LI>\
            <A HREF=$(BASEMAIN).dvi><B class=applname>$(BASEMAIN)</B> \
            Reference Manual - dvi</A></UL>" \
            > $(DOCDIR)/$(BASEMAIN)_dvi.htmlindex
endif
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN)_dvi.htmlindex

uninstalldvi: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Uninstalling .dvi file for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(DOCDIR)/$(BASEMAIN).dvi*
	-@rm -f $(DOCDIR)/$(BASEMAIN)_dvi.htmlindex

# This is really not safe: there may be figures from other documents here...
#	-@rm -f $(DOCDIR)/auto*.eps

# $(BASEMAIN).ps
installps: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing .ps file for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@mkdir -p $(DOCDIR)
	-@chmod $(EXECMODE) $(DOCDIR)
	-@cp $(BASEMAIN).ps $(DOCDIR) 
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN).ps
ifdef COMPRESSDVIPS
	$(COMPRESSDVIPS) $(DOCDIR)/$(BASEMAIN).ps
	-@$(ECHO) "<UL class=htmlindex><LI>\
            <A HREF=$(BASEMAIN).ps.gz><B class=applname>$(BASEMAIN)</B> \
            Reference Manual - postscript,gzipped</A></UL>" \
            > $(DOCDIR)/$(BASEMAIN)_ps.htmlindex
else
	-@$(ECHO) "<UL class=htmlindex><LI>\
            <A HREF=$(BASEMAIN).ps><B class=applname>$(BASEMAIN)</B> \
            Reference Manual - postscript</A></UL>" \
            > $(DOCDIR)/$(BASEMAIN)_ps.htmlindex
endif
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN)_ps.htmlindex

uninstallps: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Uninstalling .ps for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(DOCDIR)/$(BASEMAIN).ps*
	-@rm -f $(DOCDIR)/$(BASEMAIN)_ps.htmlindex

# $(BASEMAIN).pdf
installpdf: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing .pdf file for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@mkdir -p $(DOCDIR)
	-@chmod $(EXECMODE) $(DOCDIR)
	-@cp $(BASEMAIN).pdf $(DOCDIR) 
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN).pdf
ifdef COMPRESSDVIPS
	$(COMPRESSDVIPS) $(DOCDIR)/$(BASEMAIN).pdf
	-@$(ECHO) "<UL class=htmlindex><LI>\
            <A HREF=$(BASEMAIN).pdf.gz><B class=applname>$(BASEMAIN)</B> \
            Reference Manual - pdf,gzipped</A></UL>" \
            > $(DOCDIR)/$(BASEMAIN)_pdf.htmlindex
else
	-@$(ECHO) "<UL class=htmlindex><LI>\
            <A HREF=$(BASEMAIN).pdf><B class=applname>$(BASEMAIN)</B> \
            Reference Manual - pdf</A></UL>" \
            > $(DOCDIR)/$(BASEMAIN)_pdf.htmlindex
endif
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN)_pdf.htmlindex

uninstallpdf: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Uninstalling .pdf for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(DOCDIR)/$(BASEMAIN).pdf*
	-@rm -f $(DOCDIR)/$(BASEMAIN)_pdf.htmlindex

# $(BASEMAIN).html
installhtml: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing .html file(s) for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@mkdir -p $(DOCDIR)/$(BASEMAIN)_html
	-@chmod $(EXECMODE) $(DOCDIR)/$(BASEMAIN)_html
	-@cp $(BASEMAIN)_html/$(BASEMAIN)*.html $(DOCDIR)/$(BASEMAIN)_html
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN)_html/$(BASEMAIN)*.html
	-@$(ECHO) "<UL class=htmlindex><LI>\
            <A HREF=$(BASEMAIN)_html/$(BASEMAIN)_toc.html>\
            <B class=applname>$(BASEMAIN)</B> Reference Manual - \
            html</A></UL>" \
            > $(DOCDIR)/$(BASEMAIN)_html.htmlindex
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN)_html.htmlindex
	-@cp $(BASEMAIN)_html/auto*.jpg $(DOCDIR)/$(BASEMAIN)_html
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN)_html/auto*.jpg
	-@cp $(BASEMAIN)_html/*.css $(DOCDIR)/$(BASEMAIN)_html
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN)_html/*.css


uninstallhtml: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Uninstalling .html file(s) for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(DOCDIR)/$(BASEMAIN)_html/$(BASEMAIN)*.html
	-@rm -f $(DOCDIR)/$(BASEMAIN)_html/auto*.jpg
	-@rm -f $(DOCDIR)/$(BASEMAIN)_html/$(BASEMAIN).css
	-@rmdir $(DOCDIR)/$(BASEMAIN)_html
	-@rm -f $(DOCDIR)/$(BASEMAIN)_html.htmlindex

# $(BASEMAIN).info 
installinfo: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing .info file(s) for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@mkdir -p $(DOCDIR)
	-@chmod $(EXECMODE) $(DOCDIR)
	-@cp $(BASEMAIN).info* $(DOCDIR) 
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN).info*
	-@$(ECHO) "<UL class=htmlindex><LI> \
              <A HREF=$(BASEMAIN).info><B class=applname>$(BASEMAIN)</B> \
              Reference Manual - gnu info</A></UL>" \
              > $(DOCDIR)/$(BASEMAIN)_info.htmlindex
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN)_info.htmlindex
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Remember to add entry to info dir (or select infoindex)!"
	@$(ECHO) "*** Remember to add $(DOCDIR) to INFOPATH!"
	@$(ECHO) "*** --------------------------------------------------------"

uninstallinfo: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Uninstalling .info file(s) for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(DOCDIR)/$(BASEMAIN).info*
	-@rm -f $(DOCDIR)/$(BASEMAIN)_info.htmlindex

# $(BASEMAIN).ascii
installascii: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing .ascii file(s) for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@mkdir -p $(DOCDIR)
	-@chmod $(EXECMODE) $(DOCDIR)
	-@cp $(BASEMAIN).ascii $(DOCDIR)
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN).ascii
	-@$(ECHO) "<UL class=htmlindex><LI> \
              <A HREF=$(BASEMAIN).ascii> \
              <B class=applname>$(BASEMAIN)</B> Reference Manual - \
              ascii txt</A></UL>" \
              > $(DOCDIR)/$(BASEMAIN)_txt.htmlindex
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN)_txt.htmlindex

uninstallascii: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Uninstalling .ascii file(s) for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(DOCDIR)/$(BASEMAIN).ascii*
	-@rm -f $(DOCDIR)/$(BASEMAIN)_txt.htmlindex

# $(BASEMAIN).manl
installmanl: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing .manl as .l for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@mkdir -p $(DOCDIR)/manl
	-@chmod $(EXECMODE) $(DOCDIR)/manl
	-@cp $(BASEMAIN).manl $(DOCDIR)/manl/$(BASEMAIN).l
	-@chmod $(DATAMODE) $(DOCDIR)/manl/$(BASEMAIN).l
	-@$(ECHO) "<UL class=htmlindex><LI> \
              <A HREF=manl/$(BASEMAIN).l><B class=applname>$(BASEMAIN)</B> \
              Unix man entry (source)</A></UL>" \
              > $(DOCDIR)/$(BASEMAIN)_man.htmlindex
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN)_man.htmlindex
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Remember to add $(DOCDIR) to MANPATH!"
	@$(ECHO) "*** --------------------------------------------------------"

uninstallmanl: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Uninstalling .l file for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(DOCDIR)/manl/$(BASEMAIN).l
	-@rm -f $(DOCDIR)/$(BASEMAIN)_man.htmlindex
	-@rmdir $(DOCDIR)/manl

#----------------------------------------------------------------------------
# Installation of index entries

# $(BASEMAIN).htmlindex $(HTMLINDEXHEADFILE) $(HTMLINDEXTAILFILE)
installhtmlindex: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing html index for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@mkdir -p $(DOCDIR)
	-@chmod $(EXECMODE) $(DOCDIR)
	-@cp $(BASEMAIN).htmlbullet $(DOCDIR)
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN).htmlbullet
	-@cp $(BASEMAIN).htmlindex $(DOCDIR) 
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN).htmlindex
# Not really needed?
	-@chmod $(DATAMODE) $(DOCDIR)/*.html
ifdef HTMLSTYLE
	-@cp lpdoc.css $(DOCDIR)
	-@chmod $(DATAMODE) $(DOCDIR)/lpdoc.css
else
endif
ifdef EXTRAFILES
	-@cp $(EXTRAFILES) $(DOCDIR)
	-@cd $(DOCDIR) && chmod $(DATAMODE) $(EXTRAFILES)
else
endif
	$(MAKE) updatehtmlindex
# In this case the figures must be in $(DOCDIR)!
	-@cp auto*.jpg $(DOCDIR)
	-@chmod $(DATAMODE) $(DOCDIR)/auto*.jpg
	$(MAKE) installreadme

updatehtmlindex: 
	-@cat Indexhead_$(BASEMAIN).html > $(DOCDIR)/index.html
	-@cd $(DOCDIR) && cat *.htmlbullet >> index.html
	-@cd $(DOCDIR) && cat *.htmlindex >> index.html
	-@$(ECHO) "<p><hr><p><b>Last modified on:</b> `date`.<p>" \
                      >> $(DOCDIR)/index.html
	-@cat Indextail_$(BASEMAIN).html >> $(DOCDIR)/index.html
	-@chmod $(DATAMODE) $(DOCDIR)/index.html

uninstallhtmlindex: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Uninstalling html index for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(DOCDIR)/$(BASEMAIN).htmlbullet
	-@rm -f $(DOCDIR)/$(BASEMAIN).htmlindex
	-@rm -f $(DOCDIR)/auto*.jpg
ifdef EXTRAFILES
	-@cd $(DOCDIR) && rm -f $(EXTRAFILES)
else
endif
	$(MAKE) updatehtmlindex
	$(MAKE) uninstallreadmeanddir

installreadme:
	@$(ECHO) "*** WARNING: Files automatically generated -- do not edit!" \
	      > $(DOCDIR)/README
	-@chmod $(DATAMODE) $(DOCDIR)/README

uninstallreadmeanddir:
	-@rm -f $(DOCDIR)/README
	-@rmdir $(DOCDIR)

# $(BASEMAIN).infoindex $(INFODIRHEADFILE) $(INFODIRTAILFILE)
installinfoindex: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Installing info index for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@mkdir -p $(DOCDIR)
	-@chmod $(EXECMODE) $(DOCDIR)
	-@cp $(BASEMAIN).infoindex $(DOCDIR) 
	-@chmod $(DATAMODE) $(DOCDIR)/$(BASEMAIN).infoindex
# Not really needed?
	-@chmod $(DATAMODE) $(DOCDIR)/*.info
#	-@cp $(INFODIRHEADFILE) $(INFODIRTAILFILE) $(DOCDIR) 
	$(MAKE) updateinfoindex
	$(MAKE) installreadme

updateinfoindex:
	-@cat Indexhead_$(BASEMAIN).info $(DOCDIR)/*.infoindex \
	     Indextail_$(BASEMAIN).info > $(DOCDIR)/dir
	-@chmod $(DATAMODE) $(DOCDIR)/dir

uninstallinfoindex: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Uninstalling info index for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(DOCDIR)/$(BASEMAIN).infoindex
	$(MAKE) updateinfoindex
	$(MAKE) uninstallreadmeanddir

#----------------------------------------------------------------------------
# Cleanup

# Eliminates all files installed  (provided SETTINGS file has not changed)
uninstall: 
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Uninstalling manuals and indices for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	$(MAKE) $(patsubst %,uninstall%,$(DOCFORMATS))
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Done."
	@$(ECHO) "*** --------------------------------------------------------"

# Eliminates the tex-related intermediate files
texclean:
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Removing tex-related tmp files for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f *.aux *.cp *.cps *.fn *.fns *.ky *.kys *.log *.tp *.tps
	-@rm -f *.op *.ops *.fi *.fis *.pd *.pds *.pr *.prs *.ug *.ugs
	-@rm -f *.co *.cos *.fu *.fus *.li *.lis 
	-@rm -f *.pg *.pgs *.ap *.aps *.mo *.mos *.gl *.gls
	-@rm -f *.toc *.te *.tes *.vr *.vrs *.de *.des
	-@rm -f *.bbl *.blg
ifeq ($(LIBTEXINFO),yes)
	-@rm -f texinfo.tex 
endif

# Eliminates all intermediate files, leaves .texic files and targets (docs)
clean: texclean
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Removing other tmp files for $(BASEMAIN) ..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(BASEMAIN)refs.el
	-@rm -f *~
	-@rm -f *.asr *.itf *.po

# Eliminates all intermediate file >and the .texic<, but leaves targets (docs)
distclean: clean
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Removing intermediate files for $(BASEMAIN) docs..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm *.texic *.refs

# Eliminates the doc files, but not the .texic
realclean: clean
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Removing target doc files for $(BASEMAIN)..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm -f $(BASEMAIN)*.texi
	-@rm -f $(BASEMAIN)*.dvi 
	-@rm -f $(BASEMAIN)*.ps
	-@rm -f $(BASEMAIN)*.pdf
	-@rm -f $(BASEMAIN)*.ascii
	-@rm -f $(BASEMAIN)_html/$(BASEMAIN)*.html
	-@rm -f $(BASEMAIN)_html/$(BASEMAIN).css
	-@rm -f $(BASEMAIN)_html/auto*.jpg
	-@rmdir $(BASEMAIN)_html
	-@rm -f $(BASEMAIN)*.ppm
	-@rm -f $(BASEMAIN)*.gif
	-@rm -f $(BASEMAIN).html
	-@rm -f $(BASEMAIN)*.htmlindex
	-@rm -f $(BASEMAIN)*.htmlbullet
	-@rm -f Indexhead_$(BASEMAIN).html
	-@rm -f Indextail_$(BASEMAIN).html
	-@rm -f Indexhead_$(BASEMAIN).info
	-@rm -f Indextail_$(BASEMAIN).info
	-@rm -f $(BASEMAIN)*.htmlsumm
	-@rm -f $(BASEMAIN)*.l $(BASEMAIN)*.manl
	-@rm -f $(BASEMAIN)*.info*
	-@rm -f lpdoc.css

# Eliminates the .texic doc (i.e., the source) files also
braveclean: realclean
	@$(ECHO) "*** --------------------------------------------------------"
	@$(ECHO) "*** Removing all other doc files for $(BASEMAIN)..."
	@$(ECHO) "*** --------------------------------------------------------"
	-@rm *.texic *.refs auto*.eps auto*.txt
#----------------------------------------------------------------------------
# Some handy viewers:
view dviview: # $(BASEMAIN).dvi
	$(XDVI) -s $(XDVISIZE) -expert -geometry 623x879-0-0 $(BASEMAIN).dvi &

largedviview: # $(BASEMAIN).dvi
	$(XDVI) -s 5 -expert -geometry 984x760+30-0 $(BASEMAIN).dvi &

svgaview: # $(BASEMAIN).dvi
	$(XDVI) -s 8 -expert -geometry 643x590+1280+0 $(BASEMAIN).dvi &

xgaview: # $(BASEMAIN).dvi
	$(XDVI) -s 8 -expert -geometry 643x759-0-0 $(BASEMAIN).dvi &

psview: # $(BASEMAIN).ps
	ghostview -magstep -1 -portrait -geometry +349+10 $(BASEMAIN).ps &

pdfview: # $(BASEMAIN).pdf
	ghostview -magstep -1 -portrait -geometry +349+10 $(BASEMAIN).pdf &

infoview: # $(BASEMAIN).info
	xterm -fn 9x15 -e info --file $(BASEMAIN).info &

htmlview: # $(BASEMAIN).html
	htmlview file:`pwd`/$(BASEMAIN)_html/$(BASEMAIN).html &
# Above for modern linux; alternatives:
# netscape -remote openURL\(file:`pwd`/$(BASEMAIN)_html/$(BASEMAIN).html\) &
# END
#----------------------------------------------------------------------------
