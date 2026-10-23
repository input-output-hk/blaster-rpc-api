FROM ubuntu:22.04 AS build
RUN apt-get update && apt-get install -y \
    ca-certificates \
    binutils \
    clang \
    curl \
    git \
    upx-ucl \
    lld \
    libgmp-dev \
    xz-utils \
    && rm -rf /var/lib/apt/lists/*
ENV ELAN_TOOLCHAIN=leanprover/lean4:v4.24.0
ENV PATH="/root/.elan/bin:${PATH}"
RUN curl -sSf https://raw.githubusercontent.com/leanprover/elan/master/elan-init.sh | sh -s -- -y
WORKDIR /src
COPY . .
RUN lake build \
    -K=lean.c.flags=-Oz \
    -K=lean.c.flags=-flto \
    -K=lean.c.flags=-ffunction-sections \
    -K=lean.c.flags=-fdata-sections \
    -K=lean.c.flags=-fno-unwind-tables \
    -K=lean.c.flags=-fno-asynchronous-unwind-tables \
    -K=lean.c.flags=-fvisibility=hidden \
    -K=lean.c.linkArgs=-Wl,--gc-sections \
    -K=lean.c.linkArgs=-Wl,--icf=safe \
    -K=lean.c.linkArgs=-Wl,-O1 \
    -K=lean.c.linkArgs=-Wl,--strip-all
RUN upx --best --lzma .lake/build/bin/blaster-rpc-api
RUN mkdir -p /out && cp .lake/build/bin/blaster-rpc-api /out/blaster-rpc-api

FROM scratch
COPY --from=build /out/ /out/
