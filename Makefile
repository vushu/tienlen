test:
	swipl -s tienlen.pl -g run_tests,halt -t 'halt(1)'
	swipl -s server_tests.pl -g run_tests,halt -t 'halt(1)'