      integer nphi,num
      parameter (nphi=200,num=1)
c nphi is the number of phi (i.e. u^2) positions
c phimax the maximum, zero the minimum.
c num is the number of drift velocities um
c umstep the size of the step in um
c fbinit is the initial background distribution function on separatrix
c
c phi is the potential (grid). denuntrap is the untrapped electron density
      real phi(0:nphi-1),denuntrap(0:nphi-1,0:num-1)
c refden is total density for the reference flat-top distribution
      real refden(0:nphi-1,0:num-1)
c denintu is the integral of denuntrap with respect to phi, from 0 to psi.
      real denintu(0:nphi-1,0:num-1)
c Af is the current area of the orbits as a function of kinetic energy.
      real Af(0:nphi-1,0:num-1)
c fb is the trapped (background) phase-space density (as a fn of KE)
      real fb(0:nphi-1,0:num-1)
c As is the area of the separatrix when the separatrix velocity is u_s.
      real As(0:nphi-1,0:num-1)
c fA the trapped background density as a function of A.
      real fA(0:nphi-1,0:num-1)
c Vhatp is the passing particle contribution to Vhat.
c Vhatf is the flat-trapped reference value of Vhat.
      real Vhatp(0:nphi-1,0:num-1),Vhatf(0:nphi-1,0:num-1)
c sq is the square root of its index. us is the value of u_s=\sqrt(phi).
      real sq(0:nphi-1),us(0:nphi-1)
c Vn is the classical potential at phi-values less than psi
      real Vn(0:nphi-1)
c xn is the resulting position 
      real xn(0:nphi-1)
c Plotting storage of Vn, xn, and fb
      integer nnphistore
      parameter (nnphistore=20)
      real Vna(0:nphi-1,nnphistore),xna(0:nphi-1,nnphistore)
      real fba(0:nphi-1,nnphistore)
c Contouring storage:
      integer nv,nx,ncl
      parameter (nv=51,nx=nphi,ncl=100)
      real fenergy(nx,nv),cworka(nx,nv),v(nv)

      common/holegcom/phi,denuntrap,refden,denintu,fb,Vhatp,Vhatf,sq,us
     $     ,Vn,xn,fenergy,cworka,v,Af,As,fA,Vna,xna,fba
