!==============================================================
! File: types.f90
! Purpose: Define all derived types for the mesh translation workflow
! Created by Chinmai Vellala on 07/10/2025
!==============================================================
module parameter_types
  use iso_fortran_env, only : real64
  implicit none

  type :: parameter_store
    real(real64) :: dx = 0.0_real64
    real(real64) :: dy = 0.0_real64
    real(real64) :: dz = 0.0_real64
    character(len=256) :: input_file = ''
    character(len=256) :: output_file = ''
  end type parameter_store

end module parameter_types

module mesh_types
  use iso_fortran_env, only : real64
  implicit none

  type :: node_t
    integer :: id
    real(real64) :: x
    real(real64) :: y
    real(real64) :: z
  end type node_t

  type :: element_t
    integer :: id
    integer :: nodes(10)
  end type element_t

  type :: set_t
    character(len=256) :: name
    integer, allocatable :: ids(:)
  end type set_t

  type :: mesh_data_t
    type(node_t), allocatable :: nodes(:)
    type(element_t), allocatable :: elements(:)
    type(set_t), allocatable :: elsets(:)
    type(set_t), allocatable :: nsets(:)
  end type mesh_data_t

end module mesh_types
