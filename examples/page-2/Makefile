SHELL := bash
TESTPAGE=perl -pe 's/(.*)/{head \$$commonhead; body \$$\1}/'
HTML5=perl html5.pl
TEST5=$(TESTPAGE) | htg | $(HTML5)

all:index.html dialogtest.html register.html login.html example.html
index.html: *.htg
	htg <index.htg | $(HTML5) | tee ~~index.html | ./tidy >index.html
dialogtest.html: *.htg
	htg <dialogtest.htg | $(HTML5) | ./tidy > dialogtest.html
register.html: *.htg
	echo register | $(TEST5) | tee ~~register.html | ./tidy >register.html
login.html: *.htg
	echo login | $(TEST5) | tee ~~login.html | ./tidy >login.html
example.html: *.htg
	htg <example.htg | $(HTML5) | tee ~~example.html | ./tidy >example.html
.always:

