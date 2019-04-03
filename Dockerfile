FROM node:10-stretch-slim
MAINTAINER Frank Celler <info@arangodb.com>


# Alternative: voyager
ARG GRAPHREDEX_TYPE=graphredex

ENV ARANGO_ROOT_PASSWORD="bad.pw"


ENV ARCHITECTURE amd64
ENV DEB_PACKAGE_VERSION 1
ENV ARANGO_VERSION 3.4.4
ENV ARANGO_URL https://download.arangodb.com/arangodb34/DEBIAN/amd64
ENV ARANGO_PACKAGE arangodb3_${ARANGO_VERSION}-1_amd64.deb
ENV ARANGO_PACKAGE_URL ${ARANGO_URL}/${ARANGO_PACKAGE}
ENV ARANGO_SIGNATURE_URL ${ARANGO_PACKAGE_URL}.asc

RUN apt-get update && \
    apt-get install -y --no-install-recommends gpg dirmngr \
    && \
    rm -rf /var/lib/apt/lists/* && \
    echo "deb http://ppa.launchpad.net/plt/racket/ubuntu cosmic main" > /etc/apt/sources.list.d/racket.list

RUN gpg --keyserver hkps://hkps.pool.sks-keyservers.net --recv-keys CD8CB0F1E0AD5B52E93F41E7EA93F5E56E751E9B
RUN apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 --recv-keys D9D33FCD84D82C17288BA03B3C9A6980F827E01E # Racket key

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
        libjemalloc1 \
        ca-certificates \
        pwgen \
        curl \
        watch \
        racket \
    && \
    rm -rf /var/lib/apt/lists/*

RUN mkdir /docker-entrypoint-initdb.d

# see
#   https://docs.arangodb.com/latest/Manual/Administration/Configuration/Endpoint.html
#   https://docs.arangodb.com/latest/Manual/Administration/Configuration/Logging.html
# run as arangodb:arangodb

RUN curl --fail -O ${ARANGO_SIGNATURE_URL} &&       \
    curl --fail -O ${ARANGO_PACKAGE_URL} &&         \
    gpg --verify ${ARANGO_PACKAGE}.asc && \
    (echo arangodb3 arangodb3/password password test | debconf-set-selections) && \
    (echo arangodb3 arangodb3/password_again password test | debconf-set-selections) && \
    DEBIAN_FRONTEND="noninteractive" dpkg -i ${ARANGO_PACKAGE} && \
    rm -rf /var/lib/arangodb3/* && \
    sed -ri \
        -e 's!127\.0\.0\.1!0.0.0.0!g' \
        -e 's!^(file\s*=).*!\1 -!' \
        -e 's!^#\s*uid\s*=.*!uid = arangodb!' \
        -e 's!^#\s*gid\s*=.*!gid = arangodb!' \
        /etc/arangodb3/arangod.conf \
    && \
    rm -f ${ARANGO_PACKAGE}*

RUN adduser runner
COPY --chown=runner:runner Docker  /home/runner/Docker
COPY --chown=runner:runner RedexServer /home/runner/RedexServer
COPY --chown=runner:runner APIServer /home/runner/APIServer
COPY --chown=runner:runner demo /home/runner/demo

RUN /home/runner/Docker/setup.sh ${GRAPHREDEX_TYPE}

WORKDIR /home/runner
ENTRYPOINT ["sh","/home/runner/Docker/startup.sh" ]

# docker build -t test -f Docker/Dockerfile ..
