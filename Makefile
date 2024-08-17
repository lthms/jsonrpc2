PROJECT := nightmare
DESTDIR ?= ${HOME}/.local
OCAML_COMPILER ?= ocaml-base-compiler.4.14.2
BUILD_PROFILE ?= release

.PHONY: all
all: build

.PHONY: build
build:
	dune build --profile=${BUILD_PROFILE} -p ${PROJECT}

_opam/.created:
	@opam switch create . --no-install --packages "${OCAML_COMPILER}",dune --deps-only -y || true
	@touch $@

%.opam: _opam/.created dune-project
	@dune build $@

.PHONY: build-deps
build-deps: ${PROJECT}.opam
	@opam update
	@opam pin ${PROJECT} . --no-action -y
	@opam install ${PROJECT} --deps-only -y

.PHONY: build-dev-deps
build-dev-deps: ${PROJECT}.opam ${PROJECT}-dev.opam
	@opam update
	@opam pin ${PROJECT} . --no-action -y
	@opam pin ${PROJECT}-dev . --no-action -y
	@opam install ${PROJECT} ${PROJECT}-dev --deps-only -y

# Disable parallel execution to ensure we don’t invoke `dune' in parallel.
.NOTPARALLEL:
