%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% entrada
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
entry(1,
      'Imprime los teatros que echan a Hamlet a las 20',
       [
        ("BAS0AC0001",_,_,_),
        ("TEA1ELTEAT",_,_,_),
        ("BAS0SXRELA",_,_,_),
        ("TEA1ASTEOB",_,_,_),
        ("BAS0SXPRPO",_,_,_),
        ("TEA1VANOO3",_,_,_),
        ("TEA1SXPRPO",_,_,_),
        ("TEA1ELHSES",_,_,_)
       ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
entry(2,
      'Imprime los Hamlets de Madrid',
      [
       ("BAS0AC0001",_,_,_),
       ("TEA1VANOO3",_,_,_),
       ("TEA1VANOC1",_,_,_)
      ]).



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
entry(3,
      'Dime los Autores de Madrid de las 15:00 con dramas',
      [
       ("BAS0AC0007",_,_,_),
       ("TEA1ELAUTO",_,_,_),
       ("TEA1SXPRPO",_,_,_),
       ("TEA1VANOC1",_,_,_),
       ("TEA1SXPRPO",_,_,_),
       ("TEA1VAHSE1",_,_,_),
       ("TEA1SXPRPO",_,_,_),
       ("TEA1VAESOB",_,_,_)
      ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 
entry(4,
      'Imprime todas las obras de Shakespeare que se echan en Madrid',
      [
       ("BAS0AC0001",_,_,_),           %Imprime
       ("BAS0SXCTFC",'ALL',_,_),       %todas
       ("TEA1ELOBRA",_,_,_),           %las obras
       ("BAS0SXPRPO",_,_,_),           %de 
       ("TEA1VANOA1",_,_,_),           %Shakespeare
       ("BAS0SXRELA",_,_,_),           %que
       ("BAS0SXPRPO",_,_,_),           %se /como preposicion :-{
       ("TEA1ASTEOB",_,_,_),           %echan
       ("BAS0SXPRPO",_,_,_),           %en
       ("TEA1VANOC1",_,_,_)            %Madrid
      ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
entry(5,
      'Imprime teatros de Madrid',
      [
       ("BAS0AC0001",_,_,_),
       ("TEA1ELTEAT",_,_,_),
       ("TEA1VANOC1",_,_,_)
      ]).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 
entry(6,
      'Imprime teatros con dramas',
      [
       ("BAS0AC0001",_,_,_),
       ("TEA1ELTEAT",_,_,_),
       ("TEA1VAESOB",_,_,_)
      ]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% 
entry(7,
      'Imprime todos los dramas',
      [
       ("BAS0AC0001",_,_,_),
       ("BAS0SXCTFC",'ALL',_,_),
       ("TEA1VAESOB",_,_,_)
      ]).
