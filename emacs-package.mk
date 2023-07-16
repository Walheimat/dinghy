# -- Variables

EMACS?=emacs

DINGHY_DIR?=dinghy
TEST_DIR?=test
COVERAGE_DIR?=coverage
DIST_DIR?=dist

LOCAL_DEPS?=$(DIST_DIR)

PACKAGE_SUFFIX?=el
PACKAGE_VERSION=$(shell cask version)

UPDATE_VERSION=$(DINGHY_DIR)/scripts/update-version.sh
UPDATE_VERSION_FILES?=Cask $(PACKAGE_NAME).el
CURRENT_PACKAGE_VERSION?="none"

# -- Default goal

ifdef CI
install: ci
else
install: local
endif

.PHONY: package-install
package-install: clean install
	$(EMACS) --batch -f package-initialize \
		--eval "(setq load-prefer-newer t)" \
		--eval "(package-install-file \"$(CURDIR)/$(DIST_DIR)/$(PACKAGE_NAME)-$(PACKAGE_VERSION).$(PACKAGE_SUFFIX)\")"

.PHONY: clean-install
clean-install: clean install

.PHONY: ci
ci: .cask $(CI_DEPS)

.PHONY: local
local: $(LOCAL_DEPS) $(LOCAL_PHONY_DEPS)

$(DIST_DIR): .cask
	cask build
	cask package

.cask: $(CASK_DEPS)
	cask install

# -- Checks

.PHONY: test
test: .cask cask-clean
	mkdir -p $(COVERAGE_DIR)
ifdef ERT_RUN
	cask $(EMACS) --batch -L . -L $(TEST_DIR) \
		--eval '(dolist (f (nthcdr 2 (directory-files "$(TEST_DIR)" t))) (load-file f))' \
		--eval '(ert-run-tests-batch-and-exit "$(TEST_SELECTOR)")'
else
	$(TEST_PRE_ARGS) cask exec ert-runner $(TEST_ARGS)
endif
ifndef CI
	[ -f $(COVERAGE_DIR)/results.txt ] || exit 0 && cat $(COVERAGE_DIR)/results.txt
endif

.PHONY: coverage
coverage: TEST_PRE_ARGS=COVERAGE_WITH_JSON=true
coverage: test

# -- Clean-up

.PHONY: cask-clean
cask-clean:
	cask clean-elc

.PHONY: clean
clean: cask-clean
	rm -rf $(LOCAL_DEPS)

# -- Utility

.PHONY: update-version
update-version:
ifneq ($(CURRENT_PACKAGE_VERSION), "none")
	$(UPDATE_VERSION) $(CURRENT_PACKAGE_VERSION) $(UPDATE_VERSION_FILES)
else
	$(info You need to set CURRENT_PACKAGE_VERSION in your Makefile)
endif

.PHONY: pacify
pacify: $(PACIFY_DEPS)
	cask $(EMACS) --batch -L . -l $(DINGHY_DIR)/dinghy-pacify.el $(PACIFY_PRE_EXEC) -f dinghy-pacify-check

# Run `make V=1 {cmd}` to print commands
$(V).SILENT:
