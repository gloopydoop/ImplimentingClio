ge!=======================================================================
!> @brief Register IBM module
!! @ingroup CLIO
!! @note This routine should be called in frame_usr_register
      subroutine IBM_register()
      implicit none

      include 'SIZE'
      include 'INPUT'
      include 'FRAMELP'
      include 'LEVELSET'

      ! local variables
      integer lpmid, il
      character*2 str
!-----------------------------------------------------------------------
      ! check if the current module was already registered
      call mntr_mod_is_name_reg(lpmid,IMB_name)
      if (lpmid.gt.0) then
         call mntr_warn(lpmid,
     $        'module ['//trim(IBM_name)//'] already registered')
         return
      endif

      ! find parent module
      call mntr_mod_is_name_reg(lpmid,'FRAME')
      if (lpmid.le.0) then
         lpmid = 1
         call mntr_abort(lpmid,
     $        'parent module ['//'FRAME'//'] not registered')
      endif

      ! register module
      call mntr_mod_reg(IBM_id,lpmid,IBM_name,
     $      'Conjugated heat transfer tools')

      ! register and set active section
      call rprm_sec_reg(IBM_sec_id,IBM_id,'_'//adjustl(IBM_name),
     $     'Runtime paramere section for IBM tool module')
      call rprm_sec_set_act(.true.,IBM_sec_id)

      ! register parameters
      call rprm_rp_reg(IBM_AMPMSK_id,IBM_sec_id,'AMPMSK',
     $     'who knows',rpar_real,0,7250,.false.,' ')

      ! set initialisation flag
      IBM_ifinit=.false.

      return
      end subroutine
!=======================================================================
!> @brief Initilise IBM tools  module
!! @ingroup CLIO
!! @note This routine should be called in frame_usr_init
      subroutine IBM_init()
      implicit none

      include 'SIZE'
      include 'FRAMELP'
      include 'INPUT'
      include 'SOLN'
      include 'LEVELSET'

      ! local variables
      integer itmp, il
      real rtmp
      logical ltmp
      character*20 ctmp
!-----------------------------------------------------------------------
      ! check if the module was already initialised
      if (IBM_ifinit) then
         call mntr_warn(IBM_id,
     $        'module ['//trim(IBM_name)//'] already initiaised.')
         return
      endif

      ! get runtime parameters
      call rprm_rp_get(itmp,rtmp,ltmp,ctmp,IBM_sc_id,rpar_real)
      IBM_AMPMSK = rtmp

      ! everything is initialised
      IBM_ifinit=.true.

      return
      end subroutine
!=======================================================================
!> @brief Check if module was initialised
!! @ingroup CLIO
!! @return IBM_is_initialised
      logical function IBM_is_initialised()
      implicit none

      include 'SIZE'
      include 'LEVELSET'
!-----------------------------------------------------------------------
      IBM_is_initialised = IBM_ifinit

      return
      end function
