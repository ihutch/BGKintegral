
ACCISLIB=accis/libaccisX.a
libraries = -L/usr/X11R6/lib/ -L./accis/ -laccisX -lX11
G77=gfortran
COMPILE-SWITCHES = -Wall -O2 -g -fbounds-check

%.o : %.f makefile;
	$(G77) -c $(COMPILE-SWITCHES) $*.f

% : %.f holegcom.f makefile $(ACCIS) ;
	$(G77)  -o $* $(COMPILE-SWITCHES) $*.f  $(libraries)

BGKint : *.f $(ACCISLIB)

$(ACCISLIB) : makefile accis/*.f
	make -C accis VECX=vecx

accis/*.f :
	git submodule init
	git submodule update

noX : $(ACCISLIB) accis/libaccis.a
	$(G77)  -o BGKint $(COMPILE-SWITCHES) BGKint.f  accis/libaccis.a
