# syntax=docker.io/docker/dockerfile:1.7-labs

FROM haskell:9.4.7 AS build

COPY . /feedback
WORKDIR /feedback
RUN stack config set system-ghc --global true && make build && make install

FROM frolvlad/alpine-glibc
COPY --from=build --chmod=0711 /root/.local/bin/decker-engine-exe /bin/feedback/feedback
COPY static /bin/feedback/static
COPY db /bin/feedback/db
RUN apk add gmp 
#gcompat
CMD ["/bin/feedback/feedback"]
