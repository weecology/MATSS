FROM rocker/tidyverse:latest

MAINTAINER Weecology "https://github.com/weecology/rdataretriever"

# Write enviromental options to config files
RUN echo "options(repos='https://cran.mtu.edu/')" >> ~/.Rprofile
RUN echo "options(repos='https://cran.mtu.edu/')" >> ~/.Renviron
RUN echo "R_LIBS=\"/usr/lib/R/library\"">> ~/.Rprofile
RUN echo "R_LIBS=\"/usr/lib/R/library\"">> ~/.Renviron
RUN echo "R_LIBS_USER=\"/usr/lib/R/library\"">> ~/.Renviron

RUN apt-get update
RUN apt-get install -y build-essential  --allow-downgrades --allow-remove-essential --allow-change-held-packages
RUN apt-get install -y wget git locales locales-all   --allow-downgrades --allow-remove-essential --allow-change-held-packages
RUN apt-get install  -y postgresql-client mysql-client   --allow-downgrades --allow-remove-essential --allow-change-held-packages

RUN apt-get install -y libgsl0-dbg libgsl0-dev
RUN apt-get install -y libreadline-gplv2-dev libncursesw5-dev libssl-dev libsqlite3-dev tk-dev libgdbm-dev libc6-dev libbz2-dev

# RUN apt-get install --allow libgsl0ldbl # not available

# Set encoding
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV LANGUAGE en_US.UTF-8

# Remove python2 and install python3
RUN apt-get remove -y python && apt-get install -y python3  python3-pip curl
RUN rm -f /usr/bin/python && ln -s /usr/bin/python3 /usr/bin/python
RUN rm -f /usr/bin/pip && ln -s /usr/bin/pip3 /usr/bin/pip



RUN echo "export PATH="/usr/bin/python:$PATH"" >> ~/.profile
RUN echo "export PYTHONPATH="/usr/bin/python:$PYTHONPATH"" >> ~/.profile
RUN echo "export PGPASSFILE="~/.pgpass"" >> ~/.profile

# Add permissions to config files
RUN chmod 0644 ~/.Renviron
RUN chmod 0644 ~/.Rprofile
RUN chmod 0644 ~/.profile

# Install retriever python package
RUN pip install git+https://git@github.com/weecology/retriever.git
RUN retriever ls > /dev/null
RUN pip install  psycopg2 pymysql > /dev/null
RUN R_RETICULATE_PYTHON="/usr/bin/python" | echo $R_RETICULATE_PYTHON >>  ~/.Renviron

COPY . ./MATSS
# Use entrypoint to run more configurations.
# set permissions.
# entrypoint.sh will set out config files
# RUN chmod 0755 MATSS/cli_tools/entrypoint.sh
# ENTRYPOINT ["/cli_tools/entrypoint.sh"]

WORKDIR ./MATSS

# Change permissions to config files
# Do not run these cmds before Entrypoint.
# RUN chmod 600 cli_tools/.pgpass
# RUN chmod 600 cli_tools/.my.cnf

CMD ["bash", "-c", "retriever -v"]
