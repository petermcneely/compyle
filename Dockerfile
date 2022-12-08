FROM petermcneely/docker-ocaml:4.14

WORKDIR /workspace

RUN apt-get update && DEBIAN_FRONTEND=noninteractive apt-get -y install \
    lsb-release \
    wget \
    software-properties-common \
    gnupg \
    cmake

RUN wget -O - https://apt.llvm.org/llvm.sh > llvm.sh
RUN sed 's/CURRENT_LLVM_STABLE=15/CURRENT_LLVM_STABLE=14/' llvm.sh > llvm-14.sh
RUN chmod +x llvm-14.sh
RUN ./llvm-14.sh
RUN opam install -y llvm
RUN opam install -y dune

COPY bin/ /workspace/bin/
COPY lib/ /workspace/lib/
COPY test/ /workspace/test/
COPY dune-project /workspace/dune-project

ENTRYPOINT ["dune"]
CMD ["test"]
