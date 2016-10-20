c BGKint solves the integral equation to find the trapped and hence
c entire distribution function for an electron hole. 
c The potential shape is specified via a function phiofx(x) which can
c be made whatever one wishes.
      program runBGKint
      integer nphi
      parameter(nphi=501,npass=500)
      real psi,um,xmax,coshlen,tl
c phi is the potential (grid).  denuntrap is the untrapped electron density
c coshlen is the scale of distant roll-off. tl is the flattened top length.
c tl is unused by the cosh^4 phiofx, tl large and negative gives same.
      real phi(0:nphi-1),us(0:nphi-1),xofphi(0:nphi-1)
      real den(0:nphi-1),denuntrap(0:nphi-1),dentrap(0:nphi-1)
      real tilden(0:nphi-1)
      real f(0:nphi),u0(0:nphi)
c Store up to three solutions
      real phi1(0:nphi-1),f1(0:nphi-1),u1(0:nphi),xofphi1(0:nphi-1)
      real phi2(0:nphi-1),f2(0:nphi-1),u2(0:nphi),xofphi2(0:nphi-1)
      real phi3(0:nphi-1),f3(0:nphi-1),u3(0:nphi),xofphi3(0:nphi-1)
      real fpp(npass),fpm(npass),upp(npass),upm(npass)
      character*10 string
      real p2x(2)
      real p2y(2)

c The hole size      
      psi=.1
c The default hole length. Greater than 4 for fast moving holes.
      coshlen=4.7
c The flattop length. Negligible for large negative values      
      tl=-10.
c The hole speed
      um=0.4
c The factor by which to increase or decrease the length
      clenfac=1.33
c The maximum velocity to which to plot.
      upmax=1.9
c Whether additional plots etc are to be emitted.
      debug=0.
      xmax=1.3*findxofphi(psi/(nphi-1.),psi,coshlen,tl,0.,50.,7)
c      write(*,*)'xmax',xmax
      p2x(1)=sqrt(psi)
      p2x(2)=p2x(1)
      p2y(1)=0.
      p2y(2)=1.
      
c Passing distribution shifted maxwellian.
      upmax=max(sqrt(psi)+1.e-5,upmax)
      do i=1,npass
         upp(i)=sqrt(psi)+(upmax-sqrt(psi))*(i-1.)/(npass-1.)
         upm(i)=-upp(i)
         uinf=sqrt(upp(i)**2-psi)
         fpp(i)=exp(-(uinf+um)**2)/sqrt(3.1415926)
         fpm(i)=exp(-(-uinf+um)**2)/sqrt(3.1415926)
c         write(*,*)i,upp(i),fpp(i),fpm(i)
      enddo
c------------------------------
      if(debug.gt.0)then
      call BGKint(nphi,psi,um,xmax,coshlen,tl,phi,us,xofphi,den
     $     ,denuntrap,dentrap,tilden,f,u0)
      call autoplot(xofphi,phi,nphi)
      call axlabels('x','!Af!@')
      call polymark(xofphi,phi,nphi,3)
      call pltend()
      call autoplot(xofphi,den,nphi)
      call axlabels('x','n!de!d')
      call pltend()
      endif
      if(debug.gt.1.)then
      call autoplot(xofphi,denuntrap,nphi)
      call axlabels('x','denuntrap')
      call pltend()
      call autoplot(phi,denuntrap,nphi)
      call axlabels('phi','denuntrap')
      call pltend()
      call autoplot(xofphi,dentrap,nphi)
      call axlabels('x','dentrap')
      call pltend()
      call autoplot(phi,dentrap,nphi)
      call axlabels('phi','dentrap')
      call pltend()

      call autoplot(u0,f,nphi)
      call axlabels('u!d0!d','f!d0!d(u!d0!d)')
c      call polymark(u0,f,nphi,1)
      call pltend()
      endif
c End of debugging section.
c------------------------------
      call BGKint(nphi,psi,um,xmax,coshlen/clenfac,tl,phi1,us,xofphi1
     $     ,den,denuntrap,dentrap,tilden,f1,u1)
      call BGKint(nphi,psi,um,xmax,coshlen*clenfac,tl,phi2,us,xofphi2
     $     ,den,denuntrap,dentrap,tilden,f2,u2)
      call BGKint(nphi,psi,um,xmax,coshlen,tl,phi3,us,xofphi3,den
     $     ,denuntrap,dentrap,tilden,f3,u3)
c 3-case plots
      call pfset(3)
      call multiframe(1,2,3)
      
c First frame
c      call autoinit(u1,f1,nphi)
      call pltinit(0.,u1(0)*1.4,f1(nphi-1)*.8,1.07/sqrt(3.14159))
      call charsize(0.018,0.018)
      call axis()
      call axis2()
      call axlabels('u!d0!d','f!d0!d(u!d0!d)     ')
      call boxtitle('Distribution Function')
      call winset(.true.)
c Passing particles
      call polyline(upp,fpp,npass)
      call polyline(upp,fpm,npass)
c Trap-passing boundary
      call dashset(4)
      call polyline(p2x,p2y,2)
      call jdrwstr(wx2nx(p2x(1)),0.1,'Trapped',-1.2)
c Trapped particles      
      call dashset(0)
      call color(1)
      call polyline(u1,f1,nphi)
      call fwrite(1./clenfac,iwdth,2,string)
      call jdrwstr(wx2nx(0.01),wy2ny(f1(nphi-1))+.02,
     $     string(1:lentrim(string)),1.)
c
      call dashset(0)
      call color(2)
      call polyline(u2,f2,nphi)
      call fwrite(clenfac,iwdth,2,string)
      call jdrwstr(wx2nx(0.01),wy2ny(f2(nphi-1))-.02,
     $     string(1:lentrim(string)),1.)
c
      call dashset(2)
      call color(4)
      call polyline(u3,f3,nphi)
      call fwrite(1.,iwdth,2,string)
      call jdrwstr(wx2nx(0.01),wy2ny(f3(nphi-1))-.02,
     $     string(1:lentrim(string)),1.)
      call color(15)
      call legendline(.02,.7,258,'Length')
      call legendline(.02,.65,258,'factor:')
      call winset(.false.)
      call jdrwstr(wx2nx(p2x(1)),0.1,'Passing',1.1)

c Second frame of two-frame plot. Potential shape.
      call pltinit(0.,xofphi3(0),0.,psi*1.2)
      call charsize(0.018,0.018)
      call axis()
      call axis2()
      call axlabels('x','!Af!@')
      call boxtitle('Potential')
      call winset(.true.)
      call color(4)
      call polyline(xofphi3,phi3,nphi)
      call dashset(0)
      call color(2)
      call polyline(xofphi2,phi2,nphi)
      call fwrite(clenfac,iwdth,2,string)
      call jdrwstr(wx2nx(xofphi2(nphi/3)),wy2ny(phi2(nphi/3)),
     $     string(1:lentrim(string)),1.)
      call color(1)
      call polyline(xofphi1,phi1,nphi)
      call fwrite(1./clenfac,iwdth,2,string)
      call jdrwstr(wx2nx(xofphi1(nphi/3)),wy2ny(phi1(nphi/3)),
     $     string(1:lentrim(string)),-1.)
      call pltend()
      
c----------------------------
c Extended plot asymmetrical.
      ncol=3
      call multiframe(1,ncol,2)
      call pltinit(-upmax,upmax,0.,1./sqrt(3.14159)+.05)
      call charsize(0.018,0.018)
      call axis()
      call axis2()
      call axlabels('u!d0!d','f!d0!d(u!d0!d)     ')
      call ticrev()
      call boxtitle('Distribution Function')
      call ticrev()
c      call polyline(u1,f1,nphi)
c      call polyline(u2,f2,nphi)
      call polyline(u3,f3,nphi)
c Passing distribution.
      call polyline(upp,fpp,npass)
      call polyline(upm,fpm,npass)
      call winset(.true.)
      call dashset(4)
      call polyline(p2x,p2y,2)
      call dashset(0)
c Reverse the axes to plot the negative part of trapped dist
      call scalewn(upmax,-upmax,0.,1./sqrt(3.14159)+.05,.false.,.false.)
c      call polyline(u1,f1,nphi)
c      call polyline(u2,f2,nphi)
      call polyline(u3,f3,nphi)
      call winset(.true.)
      call dashset(4)
      call polyline(p2x,p2y,2)
      call dashset(0)
c Second frame
c      call autoinit(u1,f1,nphi)
      call pltinit(0.,u1(0)*1.4,0.,1./sqrt(3.14159)+0.05)
      call charsize(0.018,0.018)
      call axis()
      call axis2()
      call axlabels('|u!d0!d|','')
c      call boxtitle('Distribution Function')
      call winset(.true.)
c Passing particles
      call polyline(upp,fpp,npass)
      call polyline(upp,fpm,npass)
c Trap-passing boundary
      call dashset(4)
      call polyline(p2x,p2y,2)
      call jdrwstr(wx2nx(p2x(1)),0.1,'Trapped',-1.2)
c Trapped particles      
      call dashset(0)
      call color(1)
      call polyline(u1,f1,nphi)
      call fwrite(1./clenfac,iwdth,2,string)
      call jdrwstr(wx2nx(0.01),wy2ny(f1(nphi-1))-.02,
     $     string(1:lentrim(string)),1.)
c
      call dashset(0)
      call color(2)
      call polyline(u2,f2,nphi)
      call fwrite(clenfac,iwdth,2,string)
      call jdrwstr(wx2nx(0.01),wy2ny(f2(nphi-1))+.02,
     $     string(1:lentrim(string)),1.)
c
      call dashset(2)
      call color(4)
      call polyline(u3,f3,nphi)
      call fwrite(1.,iwdth,2,string)
      call jdrwstr(wx2nx(0.01),wy2ny(f3(nphi-1))-.015,
     $     string(1:lentrim(string)),1.)
      call color(15)
      call legendline(.02,.7,258,'Length')
      call legendline(.02,.65,258,'factor:')
      call winset(.false.)
      call jdrwstr(wx2nx(p2x(1)),0.1,'Passing',1.1)
      if(ncol.eq.3)then
c Last frame of three-frame plot. Potential shape.
      call pltinit(5.e-4,xofphi3(0),0.,psi*1.2)
      call charsize(0.018,0.018)
      call axis()
      call axis2()
      call axlabels('x','')
      call axptset(1.,0.)
      call ticrev()
      call altyaxis(1.,1.)
      call axlabels('','!Af!@')
      call boxtitle('Potential')
      call winset(.true.)
      call color(4)
      call polyline(xofphi3,phi3,nphi)
      call dashset(0)
      call color(2)
      call polyline(xofphi2,phi2,nphi)
      call fwrite(clenfac,iwdth,2,string)
      call jdrwstr(wx2nx(xofphi2(nphi/3)),wy2ny(phi2(nphi/3)),
     $     string(1:lentrim(string)),1.)
      call color(1)
      call polyline(xofphi1,phi1,nphi)
      call fwrite(1./clenfac,iwdth,2,string)
      call jdrwstr(wx2nx(xofphi1(nphi/3)),wy2ny(phi1(nphi/3)),
     $     string(1:lentrim(string)),-1.)
      endif
      call pltend()

      end
c**********************************************************************
c BGKint solves the integral equation to find the trapped and hence
c entire distribution function for an electron hole. 
c The potential shape is specified via a function phiofx(x) which can
c be made whatever one wishes.
c Units of x are debyelengths, of potential Te/e, of time omega_p^{-1}
c But u is v/sqrt(2), i.e. normalized to sqrt(2Te/me).

      subroutine BGKint(nphi,psi,um,xmax,coshlen,tl,phi,us,xofphi,den
     $     ,denuntrap,dentrap,tilden,f,u0)
c nphi is the number of phi (i.e. u^2) positions
c psi the maximum, zero the minimum.
      integer nphi
c phi is the potential (grid).  denuntrap is the untrapped electron density
      real phi(0:nphi-1),us(0:nphi-1),xofphi(0:nphi-1)
      real den(0:nphi-1),denuntrap(0:nphi-1),dentrap(0:nphi-1)
      real tilden(0:nphi-1)
      real f(0:nphi),u0(0:nphi)
      real pi
      parameter (pi=3.1415926)
      parameter (nbi=20)
      real delx

      phistep=psi/(nphi-1.)
      delx=4.*xmax/nphi
      sphistep=sqrt(phistep)
      flatf=exp(-um**2)/sqrt(pi)
c 
      do i=0,nphi-1
         phi(i)=i*phistep
      enddo
      do i=0,nphi-1
c Find the xofphi by bisection.
         xofphi(i)=findxofphi(phi(i),psi,coshlen,tl,0.,xmax,nbi)
c Calculate the total density -d^2\phi/dx^2 as a function of potential,
c at the nodes.
         xc=xofphi(i)
         den(i)=1.+(phiofx(xc+delx,psi,coshlen,tl)
     $        +phiofx(xc-delx,psi,coshlen,tl)
     $        -2.*phiofx(xc,psi,coshlen,tl))/delx**2
         us(i)=sqrt(phi(i))
c Get the untrapped electron density at this potential and drift.
         denuntrap(i)=untrappedden(phi(i),um)
         dentrap(i)=den(i)-denuntrap(i)
c Density difference c.f. flat:
         tilden(i)=dentrap(i)-2*us(i)*flatf
      enddo

c f(u) = (1/pi) \int_0^{psi-u^2} dn/d\phi d\phi/sqrt(\psi-u^2-phi).
c u^2=psi-i*phistep, phi=j*phistep, so sqrt -> (i-j)*psistep.
      u0(0)=sqrt(psi)
      f(0)=flatf
      do i=1,nphi-1
         u0(i)=sqrt(psi-i*phistep)
         fi=0.
         do j=1,i
c We calculate the dndphi based upon \tilde f's density rather than on the
c total density, because this avoids big errors near the separatrix.
            dndphi=(tilden(j)-tilden(j-1))/phistep
            fi=fi+dndphi*2.*sphistep*(sqrt(i-j+1.)-sqrt(float(i-j)))
         enddo
         f(i)=fi/pi+flatf
      enddo

      end
c*********************************************************************
c Chosen potential function. Unneeded less general version.
c The coshlen factor should be proportional to the hole length.
c      real function phiofx1(x,psi,coshlen)
c      real x,psi,coshlen
c      phiofx1=psi/(cosh(x/coshlen))**4
c      end
c*********************************************************************
c Flattened sech^4 potential function.
      real function phiofx(x,psi,coshlen,toplen)
      real x,psi,coshlen,toplen
      phiofx=psi*(1.+exp(-toplen))
     $     /(1.+exp(-toplen)*cosh(x/coshlen)**4)
      end
c*********************************************************************
c Untrapped density function for a shifted Maxwellian
c  untrappedden(\phi,u_m) = {2\over \sqrt{\pi}}\int_{\sqrt{\phi}}^\infty
c     \exp(-u^2+\phi-u_m^2)\cosh(2u_m\sqrt{u^2-\phi}) du.
c   = {1\over\sqrt{\pi}} \int_{\sqrt{\phi}}^\infty \sum_{\pm}
c             \exp(-[\pm\sqrt{u^2-\phi}-u_m]^2)du
c written as f = \int g du.

c This is the density relative to the background density of untrapped
c particles having a Maxwellian background shifted by a speed um,
c measured in units of sqrt(2T/m), at a potential energy -phi
c measured in units of T/e.

c The integration is carried out on uniform grid, initially with 
c velocity spacing dui. The spacing is halved until relative difference
c from the prior integral is less than df.
c [Some sort of Gaussian integration would probably be faster but tricky
c because of the infinite range.]
c umswitch is currently unused.
      real function untrappedden(phi,um)
      real phi,um

      parameter (dui=.04,df=1.e-5,np=10000,umswitch=4.)

c silence warnings not really needed.
      f=0.
      fp=0.
      fm=0.
      v=0.
      gm1=0.
      f1=0
      g1=1.
      g1p=1.
      g1m=1.
      gp=0.
      gm=0.

      um2=um**2
      du=dui
      nstepit=7

      if(phi.lt.0)stop 'Negative phi in untrappedden call not allowed'
c Iterate over steps sizes
      do k=1,nstepit
c Integrate
         do i=0,np
            u=sqrt(phi)+i*du
            u2=u**2
            if(i.eq.0)then
               g=exp(-um2)
               gp=g
               gm=g
               g1=g
               if(g.eq.0)stop 'untrappedden error um2 overflow'
               f=0.
               fp=0.
               fm=0.
            else
c This is simply more reliable:
               gp=exp(-(sqrt(u2-phi)+abs(um))**2)
               gm=exp(-(sqrt(u2-phi)-abs(um))**2)
               g=0.5*(gp+gm)
c               g=0.5*(exp(-(sqrt(u2-phi)-abs(um))**2)
c     $              +exp(-(sqrt(u2-phi)+abs(um))**2))
c This implicitly multiplies by 2:               
               f=f+(u-v)*(g+gm1)
               fp=fp+(u-v)*(gp+g1p)*0.5
               fm=fm+(u-v)*(gm+g1m)*0.5
            endif
            if(.not.g.ge.0 .or. .not.g.lt.1.e30)then
c Error trap
               write(*,*)'u,um,phi,g error',u,um,phi,g
c               stop
               g=0.
            endif
c            write(*,'(10f8.4)')f,fp,fm,g,gp,gm
            f=fp+fm
            gm1=g
            g1p=gp
            g1m=gm
            v2=u2
            v=u
c If new contributions are negligible, break
            if(g.lt.g1*df)goto 1
            if(gp.lt.g1*df.and.gm.lt.g1*df)
     $           write(*,*)'g,gp,gm,g1',g,gp,gm,g1
         enddo
         write(*,*)'untrappedden exhausted number of steps np',i
 1       continue
c If converged, break
         if(abs(f1-f).lt.df)goto 2
         f1=f
         du=du/2.
      enddo
      write(*,*)'untrappedden exhausted step iterations',k,f1,f,du
      write(*,*)'Steps, du, u, f, um',i,du,u,f,um
      write(*,*)u,g,f
 2    continue

      untrappedden=f/sqrt(3.1415926)

      end
c********************************************************************
      real function findxofphi(phiv,psi,coshlen,tl,xmin,xmax,nbi)
c Solve by bisection phiv=phiofx(xc,psi,coshlen,tl) and return xc.
c The intial x-range is [0,xmax]. Up to nbi bisections are allowed.
c This version uses a function, with three extra parameters
c psi and coshlen and tl. 
         xa=xmin
         xb=xmax
c This extra tiny value prevents rounding errors from causing outside
c range potentials.
         fva=phiofx(xa,psi,coshlen,tl)-phiv+1.e-7
c         fvb=phiofx(xb,psi,coshlen,tl)-phiv
c Allow a value beyond the xmax range so long as phiv is non-negative.
         fvb=-phiv
         if(fva*fvb.gt.0.)then
            write(*,*)xa,fva,xb,fvb,phiv,phiofx(xa,psi,coshlen,tl)
     $           ,phiofx(xb,psi,coshlen,tl)
            stop 'Potential outside range'
         endif
         do j=1,nbi
            xc=0.5*(xa+xb)
            fvc=phiofx(xc,psi,coshlen,tl)-phiv
            if(fvc.eq.0.)then
               goto 1
            elseif(sign(1.,fvc).eq.sign(1.,fva))then
               xa=xc
               fva=fvc
            else
               xb=xc
               fvb=fvc
            endif
         enddo
 1       continue
         findxofphi=xc
         end
