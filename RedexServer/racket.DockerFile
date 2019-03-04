
FROM ubuntu

RUN apt update && \
    apt upgrade -y && \
    apt install -y gnupg ca-certificates xvfb patch && \
    echo "deb http://ppa.launchpad.net/plt/racket/ubuntu bionic main" > /etc/apt/sources.list.d/racket.list && \
    echo "deb-src http://ppa.launchpad.net/plt/racket/ubuntu bionic main" >> /etc/apt/sources.list.d/racket.list && \
    apt-key adv --keyserver keyserver.ubuntu.com --recv-keys D9D33FCD84D82C17288BA03B3C9A6980F827E01E && \
    apt update && \
    apt install -y racket && \
    apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/* && \
    useradd -m runner

# As the runner user
USER "runner"
WORKDIR /home/runner/
COPY xvfb.patch xvfb.patch
RUN cp /usr/bin/xvfb-run ~/xvfb-run &&\
    patch xvfb-run xvfb.patch && \
    touch run.rkt && \
    mkdir /home/runner/data /home/runner/server

CMD ~/xvfb-run -n 0 -- racket run.rkt
