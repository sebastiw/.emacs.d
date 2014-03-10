GIT-P="$(shell which git)"
EDTS="https://github.com/tjarvstrand/edts.git"

ifeq ($(GIT-P),"")
$(error "You need ``git'' to carry on.")
endif

edts:
	$(GIT-P) clone $(EDTS) && \
	cd edts && \
	make
