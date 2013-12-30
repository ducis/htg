SHELL := bash
TIDY=tidy -i -utf8 -w 0 --tidy-mark n 

htg: .always
	ghc htg.hs
htgTest1:
	cat test1.htg
	runghc htgTest1.hs < test1.htg | perl html5.pl | tee test1.htm \
		| $(TIDY) >test1a.htm
.always:
