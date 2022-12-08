FROM petermcneely/docker-ocaml:4.14-2

WORKDIR /workspace

COPY bin/ /workspace/bin/
COPY lib/ /workspace/lib/
COPY test/ /workspace/test/
COPY dune-project /workspace/dune-project

ENTRYPOINT ["dune"]
CMD ["test"]
