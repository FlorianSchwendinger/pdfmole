
R ?= R

.PHONY: all
all:
	$(MAKE) clean
	$(MAKE) doc
	$(MAKE) build
	$(MAKE) install
	$(MAKE) test
	$(MAKE) check

.PHONY: some
some:
	$(MAKE) clean
	$(MAKE) build
	$(MAKE) install
	$(MAKE) test

.PHONY: clean
clean:
	$(RM) pdfmole_1.0.tar.gz

.PHONY: doc
doc:
	$(R) -e 'devtools::document()'

.PHONY: build
build:
	$(R) CMD build . --no-build-vignettes

.PHONY: install
install:
	$(R) CMD INSTALL pdfmole_1.0.tar.gz

.PHONY: uninstall
uninstall:
	$(R) CMD REMOVE pdfmole || true

.PHONY: test
test:
	$(R) -e 'require(pdfmole)'

.PHONY: check
check:
	_R_CHECK_CRAN_INCOMING_REMOTE_=false $(R) CMD check pdfmole_1.0.tar.gz --as-cran --ignore-vignettes --no-stop-on-test-error


