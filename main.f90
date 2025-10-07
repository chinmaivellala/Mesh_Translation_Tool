!==============================================================
! Program: mesh_shift
! Purpose: Orchestrate the mesh translation pipeline by invoking
!          parameter loading, mesh read, coordinate translation,
!          and write-out stages in sequence.
! Created by Chinmai Vellala 06/10/2025
! Modified 07/10/2025
!==============================================================
program mesh_shift
  use parameter_types, only : parameter_store
  use mesh_types, only : mesh_data_t
  implicit none

  external :: read_params, read_mesh, translate_mesh, write_mesh

  type(parameter_store) :: params
  type(mesh_data_t) :: mesh

  call read_params(params)
  call read_mesh(mesh, params)
  call translate_mesh(mesh, params)
  call write_mesh(mesh, params)
end program mesh_shift

 