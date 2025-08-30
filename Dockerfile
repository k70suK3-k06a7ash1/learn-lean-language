FROM ubuntu:22.04

RUN apt-get update && apt-get install -y \
    git \
    curl \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

RUN curl https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh -sSf | sh -s -- -y

ENV PATH="/root/.elan/bin:${PATH}"

WORKDIR /workspace

CMD ["bash"]