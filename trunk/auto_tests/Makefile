
default: clean match append transpose multiple_filters liftsolve

match: 
	source ./test match "testfiles/match.pl" "match.spec.pl" "match([a,b,c], [a,a,a,a,a,a,b,b,b,b,b,a,b,c,a,b,b,c,b,a,b,c,a,b,c,c,c,c,a,a,a,a,b,c])"
	source ./test match "testfiles/match.pl" "match.spec.pl" "match([a,b,c], [a,a,a,a,a,a,b,b,b,b,b,a,b,c,a,b,b,c,b,a,b,c,a,b,c,c,c,c,a,a,a,a,b,c])" -config classic
	source ./test match "testfiles/match.pl" "match.spec.pl" "match([a,b,c], [a,a,a,a,a,a,b,b,b,b,b,a,b,c,a,b,b,c,b,a,b,c,a,b,c,c,c,c,a,a,a,a,b,c])" -config mixtus
	source ./test match "testfiles/match.pl" "match.spec.pl" "match([a,b,c], [a,a,a,a,a,a,b,b,b,b,b,a,b,c,a,b,b,c,b,a,b,c,a,b,c,c,c,c,a,a,a,a,b,c])" -config fast
	source ./test match "testfiles/match.pl" "match.spec.pl" "match([a,b,c], [a,a,a,a,a,a,b,b,b,b,b,a,b,c,a,b,b,c,b,a,b,c,a,b,c,c,c,c,a,a,a,a,b,c])" -config minimal
	source ./test match "testfiles/match.pl" "match.spec.pl" "match([a,b,c], [a,a,a,a,a,a,b,b,b,b,b,a,b,c,a,b,b,c,b,a,b,c,a,b,c,c,c,c,a,a,a,a,b,c])" -pp max
	
	
append:
	source ./test append "testfiles/append.pl" "append.spec.pl" "test([x,y,z,r],_)"
	source ./test_wo_check append "testfiles/append.pl" "append.spec.pl" "app(_X,_R,_R)"

transpose:
	source ./test transpose "testfiles/transpose.pl" "transpose.spec.pl" "transpose([[1,2,3],[4,5,6],[7,8,9],[10,11,12]],_)"

multiple_filters:
	source ./test multiple_filters "testfiles/multiple_filters.pl" "multiple_filters.spec.pl" "test(_)"

liftsolve:
	source ./test liftsolve "testfiles/liftsolve.pl" "liftsolve.spec.pl" "test(term(cons,[term(1,[]),term(cons,[term(2,[]),term(null,[])])]),term(cons,[term(3,[]),term(null,[])]),_RES)"
	source ./test liftsolve "testfiles/liftsolve.pl" "liftsolve.spec.pl" "test(term(cons,[term(1,[]),term(cons,[term(2,[]),term(null,[])])]),term(cons,[term(3,[]),term(null,[])]),_RES)" -config classic


clean:
	find . -name "*.ast" -delete
	find . -name "*.itf" -delete
	find . -name "*.po" -delete
	find . -name "*.spec" -delete
	find . -name "*.orig" -delete
	find . -name "*.gx" -delete
	rm -f *.pl
