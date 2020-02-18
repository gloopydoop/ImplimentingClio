c-----------------------------------------------------------------------
      subroutine uservp (ix,iy,iz,ieg)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      if (ifield.eq.1) then
         utrans  = param(1)
         udiff   = param(2)

      else

         utrans  = param(7)        ! thermal properties
         udiff   = param(8)

         if (ieg .gt. nelgv) then  ! properties in the solid
            udiff   = 0.1*param(8) ! conductivity
            utrans  = 1.0
         endif

      endif

      return
      end
c-----------------------------------------------------------------------
      subroutine userf  (ix,iy,iz,ieg)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      ffx = 0.0
      ffy = 0.0
      ffz = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userq  (ix,iy,iz,ieg)
      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      qvol = 0.0
      if (ieg.gt.nelgv) qvol = 1.0

      return
      end
c-----------------------------------------------------------------------
      subroutine userchk
      include 'SIZE'
      include 'TOTAL'


      if (ISTEP.eq.NSTEPS.or.LASTEP.eq.1) then
	! print our my parameters to compare with old version
	write(*,"(L1,L2)") IFFLOW
	write(*,"(L1,L2)") IFHEAT
	write(*,"(L1,L2)") IFTRAN
	write(*,"(L1,L2)") IFADV
	write(*,"(L1,L2)") IFMSH
	write(*,"(L1,L2)") IFAXIS
	write(*,"(L1,L2)") IFSTRS
	write(*,"(L1,L2)") IFLOMACH
	write(*,"(L1,L2)") IFMGRID
	write(*,"(L1,L2)") IFMVBD
	write(*,"(L1,L2)") IFCHAR
	write(*,"(L1,L2)") IFSYNC
	write(*,"(L1,L2)") IFUSERVP
	do i = 1, 118
		write(6,1) i, param(i)
	enddo
      endif

      nv = nx1*ny1*nz1*nelv
      nt = nx1*ny1*nz1*nelt

      if (mod(istep,10).eq.0) then
         tmax = glmax(t ,nt)
         umax = glmax(vx,nt)
         if (nid.eq.0) write(6,1) istep,time,umax,tmax
   1     format(i9,1p3e12.5,' tmax')
      endif

      if (mod(ISTEP,999).eq.0) then
         call outpost2(VX,VY,VZ,PR,T,0,'   ')
      endif


      return
      end
c-----------------------------------------------------------------------
      subroutine userbc (ix,iy,iz,iside,ieg)
c     NOTE ::: This subroutine MAY NOT be called by every process

C     Set boundary conditions

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      ux   = 4.0*y*(1. - y)
      uy   = 0.0
      uz   = 0.0

      temp = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine useric (ix,iy,iz,ieg)

C     Set initial conditions

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

      ux   = 4.0*y*(1. - y)
      uy   = 0.0
      uz   = 0.0
      temp = 0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat
      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat2
      include 'SIZE'
      include 'TOTAL'
c     param(66) = 4
c     param(67) = 4
      return
      end
c-----------------------------------------------------------------------
      subroutine usrdat3
      return
      end
c-----------------------------------------------------------------------

c automatically added by makenek
      subroutine usrsetvert(glo_num,nel,nx,ny,nz) ! to modify glo_num
      integer*8 glo_num(1)

      return
      end

c automatically added by makenek
      subroutine userqtl

      call userqtl_scig

      return
      end