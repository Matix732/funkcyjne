FROM ubuntu:24.04

RUN apt-get update && \
    apt-get install -y vim python3 python3-pip curl wget gpg ca-certificates && \
    apt-get install -y openjdk-8-jdk && \
    
    # repo scali
    curl -sL "https://keyserver.ubuntu.com/pks/lookup?op=get&search=0x2EE0EA64E40A89B84B2DF73499E82A75642AC823" | gpg --dearmor > /usr/share/keyrings/sbt-keyring.gpg && \
    # repo sbt
    echo "deb [signed-by=/usr/share/keyrings/sbt-keyring.gpg] https://repo.scala-sbt.org/scalasbt/debian /" > /etc/apt/sources.list.d/sbt.list && \
    
    apt-get update && \
    apt-get install -y scala sbt && \
    
    # usuń tymczasowe
    rm -rf /var/lib/apt/lists/*