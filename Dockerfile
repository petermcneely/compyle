FROM ubuntu:20.04

ARG OCAML_VERSION='4.14.0'

WORKDIR /workspace

RUN apt update && \
    apt install -y opam

RUN opam init --disable-sandboxing -y
RUN eval $(opam env)
RUN opam switch create ${OCAML_VERSION}
RUN eval $(opam env)
RUN opam install ocamlbuild
RUN eval $(opam env)
RUN opam install ocaml.${OCAML_VERSION}

ENV OPAM_SWITCH_PREFIX=/root/.opam/${OCAML_VERSION}
ENV CAML_LD_LIBRARY_PATH=/root/.opam/${OCAML_VERSION}/lib/stublibs:/root/.opam/${OCAML_VERSION}/lib/ocaml/stublibs:/root/.opam/${OCAML_VERSION}/lib/ocaml
ENV OCAML_TOPLEVEL_PATH=/root/.opam/${OCAML_VERSION}/lib/toplevel
ENV MANPATH=:/root/.opam/${OCAML_VERSION}/man
ENV PATH=/root/.opam/${OCAML_VERSION}/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

ENTRYPOINT ["ocamlbuild"]