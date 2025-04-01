FROM alpine:latest

WORKDIR /app

COPY . .

RUN apk add --no-cache curl bash build-base ncurses-dev sbcl git

RUN curl -o quicklisp.lisp https://beta.quicklisp.org/quicklisp.lisp
RUN sbcl --noinform --no-userinit --no-sysinit --non-interactive --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --eval "(ql-util:without-prompting (ql:add-to-init-file))"

RUN curl -L https://qlot.tech/installer | bash

RUN qlot install && \
    qlot exec sbcl --noinform --load scripts/build-ncurses.lisp

ENTRYPOINT qlot exec sbcl --noinform --eval "(ql:quickload :lem-ncurses)" --eval "(lem:lem)" --quit
