flex: gpp_lexer.l	
	@flex -o gpp_lexer.c gpp_lexer.l
	@gcc gpp_lexer.c -o gpp_lexer
	@./gpp_lexer

DFA:
	@clisp gpp_lexer.lisp test1.g++
	@clisp gpp_lexer.lisp test2.g++
	@clisp gpp_lexer.lisp test3.g++