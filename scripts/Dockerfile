FROM leanprovercommunity/lean4:latest AS build
WORKDIR /src
COPY . .
RUN lake build
RUN mkdir -p /out && cp .lake/build/bin/afv-rpc-api /out/afv-rpc-api

FROM scratch
COPY --from=build /out/ /out/