<html lang="en">
<head>
  <link rel="stylesheet" type="text/css" href="../mecce.css"/>
  <title>About Plug-Ins</title>
</head>

<body>

<div style="width:270px">

<h1>Plug-Ins</h1>
<p>
The RUL and NFTA regular approximation tools and the Domain Model tool were
developed by <a href="http://www.ruc.dk/~jpg/">John Gallagher</a>
with support from EU project
<a href="http://www.clip.dia.fi.upm.es/Projects/ASAP/">ASAP</a>.
The CiaoPP tools were
developed by the <a href="http://www.clip.dia.fi.upm.es">CLIP Lab of the University of Madrid</a>
with support from EU project
<a href="http://www.clip.dia.fi.upm.es/Projects/ASAP/">ASAP</a>.
The PolyType tool was developed by <a href="http://www.ruc.dk/~jpg/">John Gallagher</a>
and Maurice Bruynooghe
with support from EU project
<a href="http://www.clip.dia.fi.upm.es/Projects/ASAP/">ASAP</a>.
</p>

<p>
More details about the Plug-Ins:
<ol>
<li> <b>RUL</b>: Performs a goal-directed abstract interpretation
  of the source program, inferring regular types expressed in Regular Unary Logic.
  A magic set transformation is used to make the analysis goal-directed.
  The inferred types are printed in the destination pane.
</li>
<li> <b>RUL-BUP</b>: Same as above, but without the magic set transformation.
  I.e., this is a goal-independent analysis and does not require a goal to be
  entered.
</li>
<li> <b>NFTA</b>: Performs a goal-directed abstract interpretation
  of the source program, inferring success types using
  Non-Deterministic Finite Tree Automaton.
  A magic set transformation is used to make the analysis goal-directed.
  The inferred information is used to detect useless clauses (clauses that
  must fail or loop); these are grayed out just as in Ecce's slicing option.
</li>
<li> <b>NFTA-BUP</b>: Same as above, but without the magic set transformation.
  I.e., this is a goal-independent analysis and does not require a goal to be
  entered.
</li>
<li> <b>CiaoPP-Analyse</b>: This uses CiaoPP to make a default abstract
interpretation of the program. The analysis information is printed in the
source pane.
</li>
<li> <b>CiaoPP-Optimize</b>: The same as above but the analysis information is used
  to perform specializaton and an optimized program is produced.
</li>
<li> <b>PolyTypes</b>:
   This plug-in performs polymorphic type inference.
   The inferred types are displayed in the destination pane.
  </li>
<li> <b>Domain Model</b>: The tool is parametric wrt the domain, which
  is specified by a regular type. The analysis converts the regular types into
  disjoint types (a pre-interpretation) and computes the least model over
  these types, which is displayed in the destination pane. If no regular type is supplied this
  button performs
  ground-nonground analysis (i.e. the Pos domain).
</li>
</div> <!-- fixed width: 270 -->

</body>
</html>
