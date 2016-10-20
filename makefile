
libraries = -L/usr/X11R6/lib/ -L./accis/ -laccisX -lX11 -lGL -lGLU
#libraries = -static -L/usr/X11R6/lib/ -L/home/hutch/accis/ -laccis
G77=gfortran
COMPILE-SWITCHES = -Wall -O2 -g -fbounds-check

#pattern rule, compile using the external definitions of commons, no backslash.
%.o : %.f makefile;
	$(G77) -c $(COMPILE-SWITCHES) $*.f

%.o : %.F makefile;
	$(G77) -c $(COMPILE-SWITCHES) $*.F

% : %.f holegcom.f makefile;
	$(G77)  -o $* $(COMPILE-SWITCHES) $*.f  $(libraries)

% : %.F
	$(G77)  -o $* $(COMPILE-SWITCHES) $*.F  $(libraries)


