COVERAGE_DIR?=coverage
DINGHY_DIR?=dinghy
DIST_DIR?=dist
LOCAL_DEPS?=$(DIST_DIR)
EMACS?=emacs
PACKAGE_SUFFIX?=el
PACKAGE_VERSION=$(shell cask version)
TEST_DIR?=test
UPDATE_VERSION=$(DINGHY_DIR)/scripts/update-version.sh

# Run `make V=1 {cmd}` to print commands
$(V).SILENT:

# -- Default goal

ifdef CI
install: ci
else
install: local
endif

.PHONY: package-install
package-install: $(DIST_DIR)
	$(EMACS) --batch -f package-initialize --eval "(package-install-file \"$(CURDIR)/$(DIST_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION).$(PACKAGE_SUFFIX)\")"

.PHONY: clean-install
clean-install: clean install

.PHONY: ci
ci: .cask

.PHONY: local
local: $(LOCAL_DEPS) $(LOCAL_PHONY_DEPS)

$(DIST_DIR): .cask
	cask build
	cask package

.cask: $(CASK_DEPS)
	cask install

# -- Checks

# Run tests using cask
.PHONY: test
test: .cask
	mkdir -p $(COVERAGE_DIR)
ifdef ERT_RUN
	cask $(EMACS) --batch -L . -L $(TEST_DIR)/ -l $(TEST_DIR)/test-helper.el -f ert-run-tests-batch
else
	$(TEST_PRE_ARGS) cask exec ert-runner $(TEST_ARGS)
endif

.PHONY: local-test
local-test: test
	cat $(COVERAGE_DIR)/results.txt

.PHONY: coverage
coverage: TEST_PRE_ARGS=COVERAGE_WITH_JSON=true
coverage: test

# -- Clean-up

.PHONY: clean
clean:
	cask clean-elc
	rm -rf $(LOCAL_DEPS)
