FROM mitchty/alpine-ghc

# Further userspace dependencies
RUN apk add --update build-base zlib-dev

# First, build the dependencies and cache them in a separate layer.
# Changing the source code under this directory should not trigger
# a reexecution at this layer.
COPY stack.yaml *.cabal /src/
RUN cd /src && stack install --only-dependencies

# Second, build the actual project and cleanup
COPY . /src/
RUN cd /src && \
        stack build && \
        stack install \
        --local-bin-path /usr/local/bin
