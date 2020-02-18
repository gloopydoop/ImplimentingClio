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
      include 'NEKUSE'          ! FF[XYZ], U[XYZ]
!      include 'ADJOINT'
      include 'INPUT'
      include 'SOLN'
      include 'PARALLEL'
      
      FFX = 0.0
      FFY = 0.0
      FFZ = 0.0
      iel=GLLEL(ieg)
!     Buoyancy
!      call cht_forcing(FFX,FFY,FFZ,ix,iy,iz,ieg)
!     IBM
      call IBM_forcing(FFX,FFY,FFZ,ix,iy,iz,ieg,UX,UY,UZ)


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
      include 'LEVELSET'
	integer i

!	Frame stuff
!     start framework
	if (ISTEP.eq.0) then
         call frame_start
      endif

!     monitor simulation
      call frame_monitor

!     finalise framework
      if (ISTEP.eq.NSTEPS.or.LASTEP.eq.1) then
         call frame_end
	write(*,*) "for Harry"
	write(*,*) nelv
	write(*,*) nelgv
	write(*,*) nelgt
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
         call outpost2(VX,VY,VZ,PR,T,0,'out')
		call outpost2(IBM_MSKNF,IBM_MSKLS,VZ,PR,T,0,'msk')
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

!     argument list
      integer ix,iy,iz,iside,ieg

!     Boundary conditions
      call IBM_bc(x,y,z,IFIELD,TEMP)

      UY=0.0
	ux= 4.0*y*(1. - y)
      if(IF3D) UZ=0.0

      return
      end
c-----------------------------------------------------------------------
      subroutine useric (ix,iy,iz,ieg)

C     Set initial conditions

      include 'SIZE'
      include 'TOTAL'
      include 'NEKUSE'

	call IBM_ic(x,y,z,IFIELD,TEMP,UX,UY,UZ,ix,iy,iz,ieg)

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
	call IBM_testing_mask()
      return
      end
c-----------------------------------------------------------------------

!======================================================================
!> @brief Register user specified modules
      subroutine frame_usr_register
      implicit none

      include 'SIZE'
      include 'FRAMELP'
!-----------------------------------------------------------------------
!     register modules
      call io_register
      call chkpt_register
	call IBM_register
!      call nseb_register

      return
      end subroutine
!======================================================================
!> @brief Initialise user specified modules
      subroutine frame_usr_init
      implicit none

      include 'SIZE'
      include 'FRAMELP'
      include 'SOLN'
!-----------------------------------------------------------------------
!     initialise modules
      call chkpt_init
	call IBM_init
!      call nseb_init

      return
      end subroutine
!======================================================================
!> @brief Finalise user specified modules
      subroutine frame_usr_end
      implicit none

      include 'SIZE'
      include 'FRAMELP'
!-----------------------------------------------------------------------

      
      return
      end subroutine
!======================================================================



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
