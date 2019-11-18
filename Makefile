##
## EPITECH PROJECT, 2019
## hal
## File description:
## Makefile
##

all:
	stack build
	stack install --local-bin-path ./

tests_run:
	stack test

clean:
	stack clean
	rm hal
