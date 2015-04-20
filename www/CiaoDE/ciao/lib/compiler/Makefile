include ../../SETTINGS
include ../../SHARED

include $(SYSDEP_FILES)/mkf-$(CIAOARCH)

INSTALLED_ENGINE_PATH_TO_CHANGE=installed_engine_path
EXEC_SUFFIX_TO_CHANGE=exec_suffix
HEADER_SKEL=header.skel
HEADER=header

exe_header:
	$(CIAOSRC)/etc/substitute_string $(INSTALLED_ENGINE_PATH_TO_CHANGE) \
		$(INSTALLEDENGINE) $(EXEC_SUFFIX_TO_CHANGE) "\"$(EXECSUFFIX)\"" \
		$(HEADER_SKEL) $(HEADER)
	-@chmod $(DATAMODE) $(HEADER)

distclean:
	/bin/rm -f *.itf *.po *.asr
	/bin/rm -f browse_builtins
	/bin/rm -f browse_builtins.cpx


