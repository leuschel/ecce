:- use_module(library(jasper)).

japro_stop :-
  jasper_initialize([classpath(['.'])],JVM),
  jasper_call(JVM,
               method('ecce/ecce_source/JavaPrologFlag','setFlag',[static]), % Which method
               setFlag(+boolean), % Types of arguments
               setFlag(true)). % The arguments.
japro_getFlag(Flag) :-
  jasper_initialize([classpath(['.'])],JVM),
  jasper_call(JVM,
               method('ecce/ecce_source/JavaPrologFlag','getFlag',[static]), % Which method
               get_flag([-boolean]), % Types of arguments
               get_flag(Flag)). % The arguments.

