!==============================================================
! Subroutine: translate_nodes
! Purpose   : Apply the global translation offsets (dx, dy, dz)
!             to every node stored in mesh_store.
! Created by Chinmai Vellala
! Date : 06/10/2025
!==============================================================
subroutine translate_nodes()
  use parameter_store, only : dx, dy, dz      ! Pull in the global offsets
  use mesh_store, only : mesh                ! Access the mesh data 
  implicit none

  integer :: i

  ! Shift each node coordinate by the requested offsets
  do i = 1, size(mesh%nodes)
    ! size(mesh%nodes) tells us how many nodes are stored.
    mesh%nodes(i)%x = mesh%nodes(i)%x + dx
    mesh%nodes(i)%y = mesh%nodes(i)%y + dy
    mesh%nodes(i)%z = mesh%nodes(i)%z + dz
  end do
end subroutine translate_nodes
