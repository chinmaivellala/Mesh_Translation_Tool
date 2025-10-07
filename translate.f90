!==============================================================
! Subroutine: translate_mesh
! Purpose: Shift all mesh nodes using translation offsets from params
! Created by Chinmai Vellala 06/10/2025
! Modified 07/10/2025
!==============================================================
subroutine translate_mesh(mesh, params)
  use mesh_types, only : mesh_data_t
  use parameter_types, only : parameter_store
  implicit none

  type(mesh_data_t), intent(inout) :: mesh
  type(parameter_store), intent(in) :: params
  integer :: i

  if (.not. allocated(mesh%nodes)) return

  do i = 1, size(mesh%nodes)
    mesh%nodes(i)%x = mesh%nodes(i)%x + params%dx
    mesh%nodes(i)%y = mesh%nodes(i)%y + params%dy
    mesh%nodes(i)%z = mesh%nodes(i)%z + params%dz
  end do
end subroutine translate_mesh
