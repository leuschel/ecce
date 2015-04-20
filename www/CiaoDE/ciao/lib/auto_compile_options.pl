foreign_compiler_options('LINUX','i86','gcc',['-fPIC']).
foreign_linker_options('LINUX','i86','ld',['-shared']).
foreign_compiler_options('Win32','i86','i386-mingw32-gcc',[]).
foreign_linker_options('Win32','i86','i386-mingw32-gcc',['-c']).
foreign_compiler_options('DARWIN','ppc','cc',['-fPIC']).
foreign_linker_options('DARWIN','ppc','cc',['-flat_namespace -bundle -undefined suppress']).
foreign_compiler_options('IRIX','mips','cc',[]).
foreign_linker_options('IRIX','mips','ld',['-shared']).
foreign_compiler_options('LINUX','alpha','gcc',['-fPIC']).
foreign_linker_options('LINUX','alpha','gcc',['-shared']).
foreign_compiler_options('LINUX','armv4l','gcc',['-fPIC']).
foreign_linker_options('LINUX','armv4l','gcc',['-shared']).
foreign_compiler_options('LINUX','armv5tel','arm-linux-gcc ',['-fPIC']).
foreign_linker_options('LINUX','armv5tel','arm-linux-gcc ',['-shared']).
foreign_compiler_options('LINUX','i86','gcc',['-fPIC']).
foreign_linker_options('LINUX','i86','ld',['-shared']).
foreign_compiler_options('LINUX','i86_64','gcc',['-fPIC']).
foreign_linker_options('LINUX','i86_64','ld',['-shared']).
foreign_compiler_options('LINUX','ppc','gcc',['-fPIC']).
foreign_linker_options('LINUX','ppc','gcc',['-shared']).
foreign_compiler_options('LINUX','Sparc','gcc',['-fPIC']).
foreign_linker_options('LINUX','Sparc','gcc',['-shared']).
foreign_compiler_options('LINUX','Sparc64','gcc',['-fPIC']).
foreign_linker_options('LINUX','Sparc64','gcc',['-shared']).
foreign_compiler_options('Solaris','i86','gcc',[]).
foreign_linker_options('Solaris','i86','ld',['-G']).
foreign_compiler_options('Solaris','Sparc','gcc',[]).
foreign_linker_options('Solaris','Sparc','ld',['-G']).
foreign_compiler_options('SunOS4','Sparc','gcc',['-fPIC']).
foreign_linker_options('SunOS4','Sparc','ld',[]).
foreign_compiler_options('Dynix','i86','cc',[]).
foreign_linker_options('Dynix','i86','ld',[]).
foreign_compiler_options('Win32','alpha','gcc',[]).
foreign_linker_options('Win32','alpha','gcc',['-c']).
foreign_compiler_options('Win32','i86','gcc',['-shared']).
foreign_linker_options('Win32','i86','gcc',['-c']).
