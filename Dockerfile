FROM r-base

RUN apt-get update -qq \
    && apt-get -y --no-install-recommends install \
    ## rJava for snowflake.connector
    default-jdk \
    default-jre \
    ## Curl, ssl for predicteroo
    aptitude \
    libcurl4-openssl-dev \
    libxml2-dev \
    libssl-dev \
    curl
    	
## Download snowflake jar field
ENV SNOWFLAKE_JAR='./snowflake_jdbc.jar'
RUN wget -O $SNOWFLAKE_JAR https://repo1.maven.org/maven2/net/snowflake/snowflake-jdbc/3.6.9/snowflake-jdbc-3.6.9.jar

## Reconfigure R to use latest Java JDK
RUN R CMD javareconf

# Install snowflake.connector
RUN R -e 'install.packages("remotes");'