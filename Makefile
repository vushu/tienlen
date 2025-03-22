test:
	swipl -s tienlen.pl -g run_tests,halt -t 'halt(1)'

test_server:
	swipl -s server.pl -g run_tests,halt -t 'halt(1)'