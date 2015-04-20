echo "Starting Tests"
ccecce append_rev.pl -pe "append([a,b,c],L,R)" -o tmp.pl
sicstus -l tmp.pl --goal "append([a,b,c],[d],R), print(test1(R)),nl,halt."
ccecce append_rev.pl -pe "rev([a,b,c],L,R)" -o tmp.pl
sicstus -l tmp.pl --goal "rev([a,b,c],[],R), print(test2(R)),nl,halt."
echo "Done"