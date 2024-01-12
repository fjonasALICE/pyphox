FROM rootproject/root:6.30.02-ubuntu22.04
WORKDIR /home

# Avoid timezone interactive dialog
ENV DEBIAN_FRONTEND=noninteractive

RUN apt-get update 
RUN apt-get install -y build-essential
RUN apt-get install -y python3
RUN apt-get install --fix-missing -y git
RUN apt-get install -y vim

# install LHAPDF
RUN mkdir lhapdf
RUN wget https://lhapdf.hepforge.org/downloads/?f=LHAPDF-6.5.4.tar.gz -O LHAPDF-6.5.4.tar.gz
RUN tar -xzf LHAPDF-6.5.4.tar.gz
RUN cd LHAPDF-6.5.4 && ./configure --prefix=/home/lhapdf && make && make install

# Download some often used PDF sets
RUN mkdir pdfsets
RUN wget http://lhapdfsets.web.cern.ch/lhapdfsets/current/NNPDF40_nnlo_as_01180.tar.gz -O pdfsets/NNPDF40_nnlo_as_01180.tar.gz
RUN wget http://lhapdfsets.web.cern.ch/lhapdfsets/current/nNNPDF30_nlo_as_0118_A208_Z82.tar.gz -O pdfsets/nNNPDF30_nlo_as_0118_A208_Z82.tar.gz
RUN wget http://lhapdfsets.web.cern.ch/lhapdfsets/current/CT14nlo.tar.gz -O pdfsets/CT14nlo.tar.gz
RUN wget http://lhapdfsets.web.cern.ch/lhapdfsets/current/EPPS16nlo_CT14nlo_Pb208.tar.gz -O pdfsets/EPPS16nlo_CT14nlo_Pb208.tar.gz
# unpack all pdf sets
RUN cd pdfsets && for f in *.tar.gz; do tar -xzf $f; done


env LHAPDF_DATA_PATH="/home/pdfsets:${LHAPDF_DATA_PATH}"
env PATH="/home/lhapdf/bin:${PATH}"
env LD_LIBRARY_PATH="/home/lhapdf/lib:${LD_LIBRARY_PATH}"
env PYTHONPATH="/home/lhapdf/lib/python3.8/site-packages:${PYTHONPATH}"

# Download pythox
# RUN git clone https://github.com/fjonasALICE/pyphox.git
RUN mkdir PyPhox
env PERL5LIB="/home/PyPhox/jptemplate/working:${PERL5LIB}"

# make default directory PyPhox
WORKDIR /home/PyPhox
# define default command (open shell)
CMD ["bash"]






