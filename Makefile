LATEX_OUT = report/*.out report/*.aux report/*.log report/*.synctex.gz report/*.toc
ERL_OUT = ebin/*.beam

.PHONY: all
all:
	erl -make

.PHONY: test
test: all
	erl -pa ebin/

.PHONY: clean
clean:
	rm -rf $(ERL_OUT)

.PHONY: distclean
distclean:
	rm -rf $(LATEX_OUT) $(ERL_OUT)