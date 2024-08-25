include helpers.mk

PROJECT := jsonrpc2
SUBPROJECTS := api server-lwt dream client
PACKAGES := $(addprefix ${PROJECT}-,${SUBPROJECTS}) 
OPAM_FILES := $(addsuffix .opam,${PACKAGES})
DUNE_PACKAGES_LIST := $(subst $(space),$(comma),$(foreach pkg,${PACKAGES},$(strip ${pkg})))
DESTDIR ?= ${HOME}/.local
OCAML_COMPILER ?= ocaml-base-compiler.4.14.2
BUILD_PROFILE ?= release

.PHONY: all
all: build

.PHONY: build
build:
	dune build --profile=${BUILD_PROFILE} -p ${DUNE_PACKAGES_LIST}

_opam/.created:
	@opam switch create . --no-install --packages "${OCAML_COMPILER}",dune --deps-only -y || true
	@touch $@

%.opam: _opam/.created dune-project
	@dune build $@

.PHONY: build-deps
build-deps: ${OPAM_FILES}
	@opam update
	@$(foreach pkg,${PACKAGES},opam pin ${pkg} . --no-action -y$(newline))
	@opam install ${PACKAGES} --deps-only -y

.PHONY: build-dev-deps
build-dev-deps: ${OPAM_FILES} ${PROJECT}-dev.opam
	@opam update
	@$(foreach pkg,${PACKAGES},opam pin ${pkg} . --no-action -y$(newline))
	@opam pin ${PROJECT}-dev . --no-action -y
	@opam install ${PACKAGES} ${PROJECT}-dev --deps-only -y

# Disable parallel execution to ensure we don’t invoke `dune' in parallel.
.NOTPARALLEL:
