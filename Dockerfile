FROM petermcneely/docker-ocaml:4.14

COPY ast.ml /workspace/ast.ml
COPY parser.mly /workspace/parser.mly
COPY scanner.mll /workspace/scanner.mll
COPY test.ml /workspace/test.ml

RUN ocamlbuild test.native

CMD ["/workspace/test.native"]