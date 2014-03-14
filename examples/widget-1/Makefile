SHELL := bash
bschtest.html:bsch.div bschtest.pl
	#
	##
	###
	perl bschtest.pl <bsch.div | ./tidy | grep -v '^$$' | tee bschtest.html 
	###
	##
	#
bsch.div:bsch.htg
	#
	##
	###
	htg <bsch.htg | tee bsch.div
	###
	##
	#



