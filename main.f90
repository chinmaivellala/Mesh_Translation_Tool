!==============================================================
! Program: mesh_shift
! Purpose: Orchestrate the mesh translation pipeline by invoking
!          parameter loading, mesh read, coordinate translation,
!          and write-out stages in sequence.
! Created by Chinmai Vellala
! Date : 06/10/2025
!==============================================================
program mesh_shift
  implicit none

  ! external tells the compiler these names refer to subroutines defined elsewhere.
  external :: read_params, read_mesh, translate_nodes, write_mesh

  call read_params()       ! Load translation offsets and file paths
  call read_mesh()         ! Populate mesh_store with input mesh data
  call translate_nodes()   ! Apply dx/dy/dz offsets to the stored mesh
  call write_mesh()        ! Write the translated mesh to the output file
end program mesh_shift

 