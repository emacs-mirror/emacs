FROM ubuntu:14.04
RUN apt-get update && apt-get install -y \
    build-essential curl libtool \
 && apt-get clean \
 && rm -rf /var/lib/apt/lists/*
RUN cd /tmp \
 && curl -O https://www.leonerd.org.uk/code/libvterm/libvterm-0.3.3.tar.gz \
 && tar xf libvterm-0.3.3.tar.gz \
 && cd /tmp/libvterm-0.3.3 \
 && export CXXFLAGS="$CXXFLAGS -fPIC" \
 && export CFLAGS="$CFLAGS -fPIC" make \
 && make install clean \
 && rm -rf /tmp/libvterm-0.3.3
#docker build . -t libvterm && docker run -v $PWD:/build --rm libvterm /bin/bash -c "cd /build/;gcc terminal.c -Wl,-Bstatic -lvterm -Wl,-Bdynamic -o terminal.so -shared -fPIC -lutil"
