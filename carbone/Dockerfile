FROM node:14
LABEL maintainer "Alexander Pehm <alexander@alexanderpehm.at>"

WORKDIR /tmp
# Download LibreOffice debian package. Select the right one (64-bit or 32-bit) for your OS.
# Get the latest from http://download.documentfoundation.org/libreoffice/stable
# or download the version currently "carbone-tested":
RUN wget https://downloadarchive.documentfoundation.org/libreoffice/old/7.0.4.2/deb/x86_64/LibreOffice_7.0.4.2_Linux_x86-64_deb.tar.gz

RUN echo "deb http://deb.debian.org/debian stretch main contrib non-free \
deb http://deb.debian.org/debian stretch-updates main contrib non-free \
deb http://security.debian.org/ stretch/updates main contrib non-free" | tee /etc/apt/sources.list.d/multiverse.list 

# Install required dependencies on ubuntu server for LibreOffice 7.0+
# Uncompress package
RUN apt update \
  && apt install -y libxinerama1 libfontconfig1 libdbus-glib-1-2 libcairo2 libcups2 libglu1-mesa libsm6 \
  && tar -zxvf LibreOffice_7.0.4.2_Linux_x86-64_deb.tar.gz

WORKDIR /tmp/LibreOffice_7.0.4.2_Linux_x86-64_deb/DEBS

# Install LibreOffice
RUN dpkg -i *.deb
# If you want to use Microsoft fonts in reports, you must install the fonts
# Andale Mono, Arial Black, Arial, Comic Sans MS, Courier New, Georgia, Impact,
# Times New Roman, Trebuchet, Verdana,Webdings)
RUN apt install -y ttf-mscorefonts-installer

# If you want to use special characters, such as chinese ideograms, you must install a font that support them
# For example:
RUN apt install -y fonts-wqy-zenhei

WORKDIR /home/node
COPY package.json index.js /home/node/
RUN npm install
EXPOSE 3000
ENTRYPOINT ["node", "index.js"]