FROM ocaml/opam:ubuntu

RUN sudo apt-get update && \
    sudo apt-get install -y --no-install-recommends \
    m4 \
    pkg-config \
    libgmp-dev \
    && sudo rm -rf /var/lib/apt/lists/*

WORKDIR /workspace
COPY . .

RUN opam install . --deps-only --with-test --yes
RUN opam install dune

CMD ["dune", "runtest"]