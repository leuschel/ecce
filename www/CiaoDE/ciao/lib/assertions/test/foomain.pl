
:- use_package([assertions]).

:- comment(title,"(Test of image inclusion in lpdoc documents)").

:- comment(author,"M.H.").

:- comment(copyright,"This is the copyright.").

:- comment(summary,"This @bf{module} is very nice and is written in
   @concept{Prolog} and CIAO. You can get it from @uref{our home
   page}{http://localhost/}, located at: @uref{http://localhost/} or
   contact us at our @email{email address}{clip@@clip.dia.fi.upm.es}
   which is @email{clip@@clip.dia.fi.upm.es}. Also, our group logo is
   @image{ciao_figure}, or, alternatively,
   @image{ciaoalt}{30}{40}. Nice, huh?").

:- comment(module,"This @concept{module} is very nice and is written
   in @concept{Prolog} @cite{iso-prolog} and CIAO
   @cite{ciao-assertions-manual-tr,bar,iso-prolog,foo} and @cite{foo}
   @uref{Hello Dolly}{http://localhost/}
   uses CLP @cite{survey94}.

   @begin{cartouche}
   This will be inside a box.
   @end{cartouche}

   Now here there will be a footnote.@footnote{And this is the footnote text.}
   ").

main.
