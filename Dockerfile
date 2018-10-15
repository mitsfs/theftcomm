# This should match the stack version
FROM alpine:3.7

WORKDIR /src

RUN apk update && apk add ghc cabal make xz git musl-dev gmp-dev zlib-dev

ADD https://github.com/nh2/stack/releases/download/v1.6.5/stack-1.7.1-x86_64-unofficial-fully-static-musl /usr/local/bin/stack
RUN chmod 755 /usr/local/bin/stack

# Download stack package files
#RUN stack --no-terminal --resolver=lts-8.5 setup

RUN git clone https://github.com/pweaver/iCalendar.git iCalendar && \
    cd iCalendar && \
    git checkout -b build b1f78042f8a38e543d1d9706b265fbcbe02dbf6a

COPY stack.yaml theftcomm.cabal Setup.hs /src/

# Install dependencies before we copy the app over
RUN stack --no-terminal test --dry-run --prefetch
RUN stack --no-terminal test --only-dependencies

COPY . /src/

RUN stack --no-terminal test

RUN stack --local-bin-path /usr/local/bin --no-terminal install

CMD ["theftcomm"]