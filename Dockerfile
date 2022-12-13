FROM petermcneely/docker-ocaml:4.14-2

WORKDIR /workspace

COPY bin/ /workspace/bin/
COPY lib/ /workspace/lib/
COPY test/ /workspace/test/
COPY dune-project /workspace/dune-project
COPY dune /workspace/dune

ENTRYPOINT ["dune"]
CMD ["test"]
