#        PiLLoW 1.1                             #
#                                               #
#  Copyright (C) 1996-2004 CLIP                 #
#             see file LGPL                     #
#################################################
include ../../SETTINGS
include ../../SHARED

pillow: icon_address.pl
	$(SETLOCALCIAO) $(CIAOC) -x Make_aux.pl && \
        rm -f Make_aux.itf Make_aux.po Make_aux

icon_address.pl: ../../SETTINGS
	echo "icon_base_address('$(WEB_IMAGES_URL)')." > icon_address.pl

# Called from the main Makefile
all: pillow

# install:
# 	@-if test ! -d $(WEB_IMAGES_PATH) ; then \
#           echo 'Warning: The value of WEB_IMAGES_PATH=$(WEB_IMAGES_PATH) is not properly set, Pillow will not be correctly configured'; \
# 	else \
#           cp -pr images/* $(WEB_IMAGES_PATH); \
# 	fi

dist: Makefile \
      html.pl http.pl ops.pl pillow_aux.pl pillow_doc.pl pillow_types.pl
	cd dist ; $(MAKE)

distclean:
	/bin/rm -f *.asr *.itf *.po
