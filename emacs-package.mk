# -- Variables

EMACS?=emacs

DINGHY_DIR?=dinghy
DINGHY_VERSION=0.2.2

DIST_DIR?=dist

LOCAL_DEPS?=$(DIST_DIR)

PACKAGE_SUFFIX?=el
PACKAGE_VERSION=$(shell cask version)

PACIFY=$(DINGHY_DIR)/scripts/dinghy-pacify.el

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
	$(info Installed (CI) using dinghy v$(DINGHY_VERSION))

.PHONY: local
local: $(LOCAL_DEPS) $(LOCAL_PHONY_DEPS)
	$(info Installed using dinghy v$(DINGHY_VERSION))

$(DIST_DIR): .cask
	cask package

.cask: $(CASK_DEPS)
	cask install

# -- Checks

TEST_DIR?=test
TEST_HELPER?=$(TEST_DIR)/test-helper.el
TEST_COVERAGE_DIR?=coverage
TEST_EXECUTE_BEFORE=true

.PHONY: test
test: .cask cask-clean
	mkdir -p $(TEST_COVERAGE_DIR)
ifdef TEST_USE_ERT_RUNNER
	$(TEST_EXECUTE_BEFORE) && cask exec ert-runner $(TEST_ARGS)
else
	$(TEST_EXECUTE_BEFORE) && cask $(EMACS) --batch -L . -L $(TEST_DIR) \
		--eval '(load-file "$(TEST_HELPER)")' \
		--eval '(dolist (f (nthcdr 2 (directory-files "$(TEST_DIR)" t))) (unless (or (file-directory-p f) (string-suffix-p "$(TEST_HELPER)" f)) (load-file f)))' \
		--eval '(ert-run-tests-batch-and-exit "$(TEST_SELECTOR)")'
endif

.PHONY: coverage
coverage: TEST_EXECUTE_BEFORE=export COVERAGE_WITH_JSON=true
coverage: test

# -- Clean-up

.PHONY: cask-clean
cask-clean:
	rm -rf $(DIST_DIR)
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
	cask $(EMACS) --batch -L . -l $(PACIFY) $(PACIFY_PRE_EXEC) -f dinghy-pacify-check

CASK_PACKAGE_DIRECTORY=$(shell cask package-directory)

.phony: upgrade-bidy
upgrade-bidy:
	$(info Removing bydi from $(CASK_PACKAGE_DIRECTORY))
	cd $(CASK_PACKAGE_DIRECTORY) && rm -rf bydi*
	cask install

# Run `make V=1 {cmd}` to print commands
$(V).SILENT:
